!****************************************************************
!  subroutine to define sediment flow routing
!JW  modified March 2017 to allow Crank-Nicholson flow routing
!****************************************************************
subroutine route_sediment_xml

use shared_data
use parameters_from_xml

implicit none
!implicit double precision (a - h, o - z)
!implicit integer (i - n)

double precision, dimension (6) :: rain_det_temp
double precision :: uc !, re !now defined in shared_data for MiC
double precision :: net_eros_here
double precision :: dstar
double precision :: dsum
double precision :: dsumlast
double precision :: susp_crit
double precision :: cnsedrhs
double precision :: cnsedrhs1, cnsedrhs2, cnsedrhs3!
!  following only used for error checking
!      double precision, allocatable :: d50_out (:, :)

integer, dimension (4, 2) :: sdirin
integer :: kcount
integer :: i, ii, iin
integer :: j, jj, jin
integer :: k, kk
integer :: l, ll

logical :: check

data sdirin / 1, 0, -1, 0, 0, -1, 0, 1 /

!Eva    uc: unit converter from fluxes (mm2/s * cell width [mm] * density [g/cm3] to g
!JWJan2006 other variables for calculating flow detachment component in
!          transitional regime
!
!      MAIN LOOP to define detachment and mode of transport for each cell
!
!write (6, *) 'route_sediment_xml MAIN LOOP start'
do im = 2, nr1
   do jm = 2, nc1
!
!  only calculate if rmask not set to blank (-ve)
!
      if (rmask (im, jm).ge.0.0d0) then
!
!     feedback with coarse particles as in Wainwright et al. ESPL 1995
!
!         grav_propn = -6.0d0 * (sed_propn (5, im, jm) +  sed_propn (6, im, jm))
!         if (grav_propn.gt.0.0d0) then
!            write (6, *) ' grav_propn: ', grav_propn
!         endif
!
!JWAug2005         No feedback
         grav_propn = 0.0d0
!
!        is there overland flow in the current cell?
         if (d (1, im, jm).gt.0.0d0) then
!   ...             then prepare for concentrated/suspended flow by
!                   calculating d50 and shear velocity
!
!LTOct2007             dsum is the proportion total sediment in each size class
            dsum = 0.0d0
            dsumlast = 0.0d0
            d50 = -9999.d0
            do phi = 1, 6
               dsum = dsum + sed_propn (phi, im, jm)
               if (dsum.ge.0.5d0.and.dsumlast.lt.0.5d0) then
                  if (phi.eq.1) then
                     d50 = (diameter (1) / dsum) * 0.5d0
                  else
                     d50 = diameter (phi - 1) +  ((diameter (phi) - diameter (phi - 1)) / &
                           (dsum - dsumlast)) * (0.5d0 - dsumlast)
                  endif
                  exit
               endif
               dsumlast = dsum
            enddo
            if (d50.lt.0.0d0) then
           d50 = diameter (6)
        endif
!
!      9.81d-3 is used to multiply by g and convert d from mm to m
!
            ustar = sqrt (9.81d-3 * d (1, im, jm) * slope (im, jm))
! ...                 if so, check for concentrated flow using a Re>=2500
!                     criterion (i.e. turbulence)
!JWFeb05              Re>=2500 criterion (i.e. turbulence)
!                     1.d-6 converts v*d from mm^2/s to m^2/s

            re = (1.d-6 * v (im, jm) * d (1, im, jm)) / viscosity

!JWFeb05               Re>=2500 criterion (i.e. turbulence)
            if (re.ge.2500.0d0) then
!   ...                    if so, detachment is by the flow
               call flow_detachment
!   ...                    and then check for suspension
               do phi = 1,6
                  dstar = diameter (phi) * dstar_const
!                              from van Rijn (1984): Sediment transport, Part II:
!                              Suspended load Transport
                  if (dstar.le.10) then
                     susp_crit = (4.d0 * settling_vel (phi))/ dstar
                  else
                     susp_crit = 0.4d0 * settling_vel (phi)
                  endif
!
                  if (ustar.ge.susp_crit) then
!   ... and if so, cause transport by suspension
                     call suspended_transport
                  else
!   ... otherwise, transport is by concentrated flow (bedload)
                     call conc_flow_transport
                  endif
!
               enddo

!JWFeb05               Re>=2500 criterion (i.e. turbulence) (else => re<2500)
            elseif (r2 (im, jm).gt.0.0d0) then
!    ...                   flow is present but not concentrated -- thus
!                          detachment is by raindrops.
!                          Check whether suspension might still occur but if
!                          not, use unconcentrated flow algorithm
               call raindrop_detachment
!JWJan2006
!JWJan2006                  add in transitional flow conditions with flow
!JWJan2006              detachment
               if (re.gt.500.d0) then
                  do phi = 1, 6
                     rain_det_temp (phi) = detach_soil (phi, im, jm)
                  enddo
                  call flow_detachment
                  do phi = 1, 6
                     detach_soil (phi, im, jm) = detach_soil (phi, im, jm) + &
                                                 rain_det_temp (phi)
                  enddo
               endif
               do phi = 1, 6
                  call diffuse_flow_transport
               enddo
!
!JWFeb05 avoid sudden drop in movement at end of event
!JWFeb05
            elseif ( r2 (im, jm).eq.0 ) then
! -----                if re < 2500 and no rainfall
!JWFeb05               Re>=2500 criterion (i.e. turbulence)
!JWJan2006
!JWJan2006             add in transitional flow conditions with flow detachment
!JWJan2006
!LTOct2007                 added in phi do loop to stop model crashing under
!                          certain conditions.
               if (re.gt.500.d0) then
                  call flow_detachment
                  do phi = 1, 6
                     call conc_flow_transport
                  enddo
               else
!JWJan2006
!JWJan2006                     otherwise no detachment
!JWJan2006
                  do phi=1,6
                     detach_soil (phi, im, jm) = 0.0d0
                     depos_soil (phi, im, jm) = 0.0d0
!                     depos_soil (phi, im, jm) = d_soil (phi, 1, im, jm) / dt
                  enddo
               endif
!
            else
!                 error trap
               write (6, *) 'Error - Re must be >=0 or <0!'
               stop
            endif   !  end of if for Re>=2500
!
!                  else no flow in current cell
!                  Check if there is rainfall and if so calculate splash
         elseif (r2 (im, jm).gt.0.0d0) then
            call raindrop_detachment
            call splash_transport
         endif  !   end of if statement for overland flow in current cell, i.e. d(1,im,jm)>0
!
      endif !   end of if statement for rmask not blank
   enddo
enddo    !      END OF MAIN LOOP (defining detachment and mode of transport for each cell)
!write (6, *) 'route_sediment_xml MAIN LOOP end'
!
!JCMay2011  Run marker-in-cell components if MiC has been set to 1
!converted JW May 2014 for xml input file format
if (MiC.eq.1) then
   call route_markers_xml
endif
!
!Eva:  Sediment Routing following kinematic wave approach (fluxes of sediments with virtual
!      travel rates v_soil, based on mass balance of sediments)
!      d_soil(phi,1,im,jm): soil depth within water flow in current cell [mm]
!      v_soil(phi,im,jm): virtual travel velocity [mm/s]
!      q_soil(phi,1,im,jm): unit sediment discharge [mm2/s]
!      detach_soil(phi,im,jm): detachment at a point [mm/s]
!      depos: deposition at a point [mm/s]
!LTOct2007    nutrient_phi (phi, 1, im, jm): nutrient concentration/flux per cell
!
!JWMar2017   modification to allow different sediment-flow routing algorithms
!JWMar2017   sediment_routing_solution_method = 1: Euler method
!JWMar2017   sediment_routing_solution_method = 2: Crank-Nicholson with Newton-Raphson method
!
uc = dx * density * 1.e-6 * dt  !LT                   multiplied q_soil by UC to get kg/s
!
if (sediment_routing_solution_method.eq.1) then ! Euler method
!   write (6, *) 'sediment_routing_solution_method.eq.1 start'
   do im = 2, nr1
      do jm = 2, nc1
         if (d (2, im, jm).gt.0.0d0.and.rmask (im, jm).ge.0.0d0) then
            do phi = 1, 6
               d_soil (phi, 2, im, jm) = d_soil (phi, 1, im, jm) + &
                      (detach_soil (phi, im, jm) - depos_soil (phi, im, jm)) * dt + &
                      (dtdx * (add_soil (phi, im, jm) - q_soil (phi, 1, im, jm)) )
               if (d_soil (phi, 2, im, jm).lt.0) then
                  d_soil (phi, 2, im, jm) = 0.
               endif
               q_soil (phi, 2, im, jm) = d_soil (phi, 2, im, jm) * v_soil (phi, im, jm)
            enddo
         elseif (d (2, im, jm).le.0.0d0.and.rmask (im, jm).ge.0.0d0) then
            do phi = 1, 6
               d_soil (phi, 2, im, jm) = 0.0d0
               q_soil (phi, 2, im, jm) = 0.0d0
            enddo
         endif
      enddo
   enddo
!   write (6, *) 'sediment_routing_solution_method.eq.1 end'
elseif (sediment_routing_solution_method.eq.2) then ! Crank-Nicolson method
!
!
!
!   write (6, *) 'sediment_routing_solution_method.eq.2 start'
   kcount = 0
   do k = 1, ncell1
      i = order (k, 1)
      j = order (k, 2)
      if (i.ge.2.and.j.ge.2.and.rmask (i, j).ge.0.0d0) then   !   calculate flux into cell for each particle size
         do phi = 1, 6
            qsedin (phi, 2, i, j) = 0.0d0
            do l = 1, 4
               iin = i + sdirin (l, 1)
               jin = j + sdirin (l, 2)
               if (aspect (iin, jin).eq.l.and.rmask (iin, jin).ge.0.0d0) then
                  if (q_soil (phi, 2, iin, jin).ge.0.0d0 ) then
                     qsedin (phi, 2, i, j) = qsedin (phi, 2, i, j) +  q_soil (phi, 2, iin, jin)
                  else
!                     write (6, *) 'Error - no +ve inflow in Crank-Nicolson / Newton-Raphson routine'
!                     write (6, 50) iin, jin, q_soil (phi, 2, iin, jin)
!                     write (6, 51) i, j, q_soil (phi, 2, i, j), ff (i, j)
!                     write (6, *) 'Surrounding cells:'
!                     do ll = 1, 4
!                        iin = i + sdirin (ll, 1)
!                        jin = j + sdirin (ll, 2)
!                        write (6, 50) iin, jin, q_soil (phi, 2, iin, jin), v_soil (phi, i, j)
!                     enddo
!                     write (6, *) 'Other problem cells:'
!                     do kk = 1, ncell1
!                        ii = order (kk, 1)
!                        jj = order (kk, 2)
!                        if (.not.(q_soil (phi, 2, ii, jj).ge.0.0d0)) then
!                           write (6, 50) ii, jj, q_soil (phi, 2, ii, jj), v_soil (phi, i, j)
!                        endif
!                     enddo
!                     kcount = kcount + 1
!                     if (kcount.gt.1) then
!                        write (6, 54)
!                        write (6, 55)
!                        write (6, 56)
!                        stop
!                     endif
                  endif
               endif
            enddo
!               end of flux into cell calculation
!
!               Crank-Nicolson method for new sediment depth
!
            cnsedrhs1 = ((1.d0 / dt) * d_soil (phi, 1, i, j))
            cnsedrhs2 = ((0.5d0 / dx) * (qsedin (phi, 2, i, j) - q_soil (phi, 1, i, j) + qsedin (phi, 1, i, j)))
            cnsedrhs3 = (detach_soil (phi, i, j) - depos_soil (phi, i, j))
            cnsedrhs = cnsedrhs1 + cnsedrhs2 + cnsedrhs3
            d_soil (phi, 2, i, j) = cnsedrhs / ((1.d0 / dt) + (0.5d0 / dx) * v_soil (phi, i, j))
            if (d_soil (phi, 2, i, j).lt.0.d0) then
               d_soil (phi, 2, i, j) = 0.0d0
            endif
            q_soil (phi, 2, i, j) = d_soil (phi, 2, i, j) * v_soil (phi, i, j)
!            if (iter.gt.1) then !for error checking
!               write (6, '(i1, 2(1x,t i4), 7(1x, d10.4))') phi, i, j, cnsedrhs1, cnsedrhs2, cnsedrhs3,  qsedin (phi, 1, i, j), &
!                               qsedin (phi, 2, i, j), d_soil (phi, 2, i, j), v_soil (phi, i, j)
!            endif
         enddo
      endif
   enddo
!   write (6, *) 'sediment_routing_solution_method.eq.2 end'
else
!
!JWMar2017  Unknown solution method -- report and stop
!
   write (6, 52) sediment_routing_solution_method
   write (6, 53)
   write (6, 54)
   write (6, 55)
   write (6, 56)
   stop
endif
!
!JWMar2017 Moved nutrient transfers and sediment totals here to after sediment-routing options
!JWMar2017    to avoid duplicating code
!
!write (6, *) 'Starting nutrient transfers'
do im = 2, nr1
   do jm = 2, nc1
      if (d (2, im, jm).gt.0.0d0.and.rmask (im, jm).ge.0.0d0) then
         do phi = 1, 6
!
!LTOct2007            Added routine to calculate the sediment-bound nutrient discharge
!                     for each phi class.
!                     q_soil is in mm2/s. Use UC to conver to kg/s. P_nutrients in g/kg.
!                     multiplied by nutrient conc (g/kg) to get g/s
            amm_q (phi, im, jm) = q_soil (phi, 2, im, jm) * uc * p_ammonium (phi)
            nit_q (phi, im, jm) = q_soil (phi, 2, im, jm) * uc * p_nitrate (phi)
            TN_q (phi, im, jm) = q_soil (phi, 2, im, jm) * uc * p_TN (phi)
            TP_q (phi, im, jm) = q_soil (phi, 2, im, jm) * uc * p_TP (phi)
            IC_q (phi, im, jm) = q_soil (phi, 2, im, jm) * uc * p_IC (phi)
            TC_q (phi, im, jm) = q_soil (phi, 2, im, jm) * uc * p_TC (phi)
!
!LTOct2007            TASK/QUESTION need to work out how to account for net deposition and
!                     erosion of soil nutrients.
!                     Have an initial reserve of nutrients in the surface soil. This reserve
!                     is increased or decreased depending on removal or deposition of nutrients?
!                     Something like this will be needed for coupling with DAYCENT.
!
!                 Calculation of total mass fluxes (all six soil classes) in kg
!                 for spatial representation of sediment flux
!
!JWAug05         n.b. previous multiplication by uc = dx * density * 1.e-6 * dt
!JWAug05              at each time step now moved to final output because dt is constant
!JWAug05
            sed_tot (im, jm) = sed_tot (im, jm) + q_soil (phi, 2, im, jm)
            detach_tot (im, jm) = detach_tot (im, jm) + detach_soil (phi, im, jm)
            depos_tot (im, jm) = depos_tot (im, jm) + depos_soil (phi, im, jm)
            z_change (im, jm) = z_change (im, jm) + depos_soil (phi, im, jm) - detach_soil (phi, im, jm)
!
!LTOct2007            Calculation of total mass fluxes (all six soil classes) for spatial
!LTOct2007            representation of nutrient flux in g
!
            amm_tot (im, jm) = amm_tot (im, jm) + amm_q (phi, im, jm)
            nit_tot (im, jm) = nit_tot (im, jm) + nit_q (phi, im, jm)
            TN_tot (im, jm) = TN_tot (im, jm) + TN_q (phi, im, jm)
            TP_tot (im, jm) = TP_tot (im, jm) + TP_q (phi, im, jm)
            IC_tot (im, jm) = IC_tot (im, jm) + IC_q (phi, im, jm)
            TC_tot (im, jm) = TC_tot (im, jm) + TC_q (phi, im, jm)
         enddo
      elseif (d (2, im, jm).le.0.0d0.and.rmask (im, jm).ge.0.0d0) then
         do phi = 1, 6
!
!                     for splash-only case, add fluxes to total for output
!
!JWAug05  n.b. previous multiplication by uc = dx * density * 1.e-6 * dt
!JWAug05       at each time step now moved to final output because dt is constant
!JWAug05
!
            net_eros_here = (detach_soil (phi, im, jm) - depos_soil (phi, im, jm))
            sed_tot (im, jm) = sed_tot (im, jm) + net_eros_here
            detach_tot (im, jm) = detach_tot (im, jm) + detach_soil (phi, im, jm)
        depos_tot (im, jm) = depos_tot (im, jm) + depos_soil (phi, im, jm)
!            z_change (im, jm) = z_change (im, jm) + &
!                                depos_soil (phi, im, jm) - detach_soil (phi, im, jm)
            z_change (im, jm) = z_change (im, jm) - net_eros_here
!LTOct2007 add in sediment-bound nutrients
            amm_q (phi, im, jm) = q_soil (phi, 2, im, jm) * (net_eros_here * uc * p_ammonium (phi))
            nit_q (phi, im, jm) = q_soil (phi, 2, im, jm) * (net_eros_here * uc * p_nitrate (phi))
            TN_q (phi, im, jm) = q_soil (phi, 2, im, jm) * (net_eros_here * uc * p_TN (phi))
            TP_q (phi, im, jm) = q_soil (phi, 2, im, jm) * (net_eros_here * uc * p_TP (phi))
            IC_q (phi, im, jm) = q_soil (phi, 2, im, jm) * (net_eros_here * uc * p_IC (phi))
            TC_q (phi, im, jm) = q_soil (phi, 2, im, jm) * (net_eros_here * uc * p_TC (phi))
            amm_tot (im, jm) = amm_tot (im, jm) + amm_q (phi, im, jm)
            nit_tot (im, jm) = nit_tot (im, jm) + nit_q (phi, im, jm)
            TN_tot (im, jm) = TN_tot (im, jm) + TN_q (phi, im, jm)
            TP_tot (im, jm) = TP_tot (im, jm) + TP_q (phi, im, jm)
            IC_tot (im, jm) = IC_tot (im, jm) + IC_q (phi, im, jm)
            TC_tot (im, jm) = TC_tot (im, jm) + TC_q (phi, im, jm)
         enddo
      endif
   enddo
enddo
!write (6, *) 'End nutrient transfers'
!
!     used for error checking
!if (iter.eq.1) then
!   open (666, file = 'hmmm.dat', status = 'unknown')
!   rewind (666)
!   write (666, *) 'Detachment: '
!   do phi = 1, 6
!      write (666, '("phi=", i1)') phi
!      do im = 1, nr1
!         write (666, '(100(e10.4, 1x))') (detach_soil (phi, im, jm), jm = 1, nc1)
!      enddo
!      write (666, *)
!   enddo
!   write (666, *)
!   write (666, *) 'Deposition: '
!   do phi = 1, 6
!      write (666, '("phi=", i1)') phi
!      do im = 1, nr1
!         write (666, '(100(e10.4, 1x))') (depos_soil (phi, im, jm), jm = 1, nc1)
!      enddo
!      write (666, *)
!   enddo
!   allocate (d50_out (nr1, nc1))
!
!   write (666, *)
!   write (666, *) 'd50: '
!   do im = 1, nr1
!      do jm = 1, nc1
!     dsum = 0.0d0
!     dsumlast = 0.0d0
!     d50 = -9999.d0
!     do phi = 1, 6
!        dsum = dsum + sed_propn (phi, im, jm)
!        if (dsum.ge.0.5d0.and.dsumlast.lt.0.5d0) then
!           if (phi.eq.1) then
!              d50 = (diameter (1) / dsum) * 0.5d0
!           else
!              d50 = diameter (phi - 1) + ((diameter (phi) - diameter (phi - 1)) / &
!                        (dsum - dsumlast)) * (0.5d0 - dsumlast)
!           endif
!           exit
!        endif
!        dsumlast = dsum
!     enddo
!     if (d50.lt.0.0d0) then
!        d50 = diameter (6)
!     endif
!     d50_out (im, jm) = d50
!      enddo
!      write (666, '(100(e10.4, 1x))') (d50_out (im, jm), jm = 1, nc1)
!   enddo
!   write (666, *)
!   write (666, *)
!   write (666, *) 'Grain size proportions: '
!   do phi = 1, 6
!      write (666, '("phi=", i1)') phi
!      do im = 1, nr1
!         write (666, '(100(e10.4, 1x))') (sed_propn (phi, im, jm), jm = 1, nc1)
!      enddo
!      write (666, *)
!   enddo
!
!   close (666)
!endif

return
 50       format ('qsedin (2,', i4, ',', i4,') =', d16.8, 1x, 'v_soil: ', d16.8)
 51       format ('Central cell: qsedin (2,',i4,',',i4,') =', d16.8, 1x, 'v_soil: ', d16.8)
 52       format ('Undefined sediment routing solution method: ', i2)
 53       format ('Please adjust value of sediment-routing_solution_method to 1 or 2 in input file and rerun the model')
 54       format ('*****************************************************************')
 55       format ('                      run terminated')
 56       format ('*****************************************************************')

end
