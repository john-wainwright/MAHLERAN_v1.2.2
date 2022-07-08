c****************************************************************
c  subroutine to define sediment flow routing
c****************************************************************
       subroutine route_sediment
       use shared_data
       implicit double precision (a - h, o - z)
       implicit integer (i - n)

       logical check
cEva  uc: unit converter from fluxes (mm2/s * cell width [mm] * density [g/cm3] to g
cJWJan2006 other variables for calculating flow detachment component in
c          transitional regime
       double precision rain_det_temp (6)
       double precision uc !, re !now defined in shared_data for MiC
c      following only used for error checking
c      double precision, allocatable :: d50_out (:, :)
c
c      MAIN LOOP to define detachment and mode of transport for each cell
c
       do im = 2, nr1
           do jm = 2, nc1
c
c              only calculate if rmask not set to blank (-9999)
c
             if (rmask (im, jm).ge.0.0d0) then
c
c                 feedback with coarse particles as in Wainwright et al. ESPL 1995
c
c                 grav_propn = -6.0d0 * (sed_propn (5, im, jm) +
c     &                       sed_propn (6, im, jm))
c               if (grav_propn.gt.0.0d0) then
c                  write (6, *) ' grav_propn: ', grav_propn
c               endif
c
cJWAug2005         No feedback
                   grav_propn = 0.0d0
c
c                  is there overland flow in the current cell?
                   if (d (1, im, jm).gt.0.0d0) then
c   ...             then prepare for concentrated/suspended flow by
c                   calculating d50 and shear velocity
c
cLTOct2007             dsum is the proportion total sediment in each size class
                       dsum = 0.0d0
                       dsumlast = 0.0d0
                       d50 = -9999.d0
                       do phi = 1, 6
                           dsum = dsum + sed_propn (phi, im, jm)
                           if (dsum.ge.0.5d0.and.dsumlast.lt.0.5d0) then
                               if (phi.eq.1) then
                                   d50 = (diameter (1) / dsum) * 0.5d0
                               else
                                   d50 = diameter (phi - 1) +
     &                            ((diameter (phi) - diameter (phi - 1))
     &                         / (dsum - dsumlast)) * (0.5d0 - dsumlast)
                               endif
                               exit
                           endif
                           dsumlast = dsum
                       enddo
                       if (d50.lt.0.0d0) then
                         d50 = diameter (6)
                     endif
c
c      9.81d-3 is used to multiply by g and convert d from mm to m
c
                       ustar = sqrt (9.81d-3 * d (1, im, jm) *
     &                     slope (im, jm))
c ...                 if so, check for concentrated flow using a Re>=2500
c                     criterion (i.e. turbulence)
cJWFeb05              Re>=2500 criterion (i.e. turbulence)
c                     1.d-6 converts v*d from mm^2/s to m^2/s

                       re = (1.d-6 * v (im, jm) * d (1, im, jm))
     &                       / viscosity

cJWFeb05               Re>=2500 criterion (i.e. turbulence)
                       if (re.ge.2500.0d0) then
c   ...                    if so, detachment is by the flow
                         call flow_detachment
c   ...                    and then check for suspension
                           do phi = 1,6
                               dstar = diameter (phi) * dstar_const
c                              from van Rijn (1984): Sediment transport, Part II:
c                              Suspended load Transport
                               if (dstar.le.10) then
                                   susp_crit = (4.d0 *
     &                                  settling_vel (phi))/ dstar
                               else
                                   susp_crit = 0.4d0 *
     &                                  settling_vel (phi)
                               endif
c
                             if (ustar.ge.susp_crit) then
c   ... and if so, cause transport by suspension
                                 call suspended_transport
                               else
c   ... otherwise, transport is by concentrated flow (bedload)
                                   call conc_flow_transport
                               endif
c
                           enddo

cJWFeb05               Re>=2500 criterion (i.e. turbulence) (else => re<2500)
                       elseif ( r2 (im, jm).gt.0.0d0 ) then
c    ...                   flow is present but not concentrated -- thus
c                          detachment is by raindrops.
c                          Check whether suspension might still occur but if
c                          not, use unconcentrated flow algorithm
                           call raindrop_detachment
cJWJan2006
cJWJan2006                  add in transitional flow conditions with flow
cJWJan2006              detachment
                            if (re.gt.500.d0) then
                                do phi = 1, 6
                                    rain_det_temp (phi) =
     &                                   detach_soil (phi, im, jm)
                                enddo
                                call flow_detachment
                                do phi = 1, 6
                                       detach_soil (phi, im, jm) =
     &                                  detach_soil (phi, im, jm)
     &                                      + rain_det_temp (phi)
                                enddo
                            endif
                            do phi = 1, 6
                                call diffuse_flow_transport
                            enddo
c
cJWFeb05 avoid sudden drop in movement at end of event
cJWFeb05
                       elseif ( r2 (im, jm).eq.0 ) then
c -----                if re < 2500 and no rainfall
cJWFeb05               Re>=2500 criterion (i.e. turbulence)
cJWJan2006
cJWJan2006             add in transitional flow conditions with flow detachment
cJWJan2006
cLTOct2007                 added in phi do loop to stop model crashing under
c                          certain conditions.
                           if (re.gt.500.d0) then
                               call flow_detachment
                               do phi = 1, 6
                                   call conc_flow_transport
                               enddo
                           else
cJWJan2006
cJWJan2006                     otherwise no detachment
cJWJan2006
                               do phi=1,6
                                   detach_soil (phi, im, jm) = 0.0d0
                                   depos_soil (phi, im, jm) = 0.0d0
c                          depos_soil (phi, im, jm) =
c     &                                   d_soil (phi, 1, im, jm) / dt
                               enddo
                           endif
c
                       else
c                          error trap
                           write (6, *) 'Error - Re must be >=0 or <0!'
                           stop
c                      end of if for Re>=2500
                       endif
c
c                  else no flow in current cell
c                  Check if there is rainfall and if so calculate splash
                   elseif (r2 (im, jm).gt.0.0d0) then
                       call raindrop_detachment
                       call splash_transport
                   endif
c                  end of if statement for overland flow in current cell, i.e. d(1,im,jm)>0
c
             endif
c              end of if statement for rmask not blank
           enddo
       enddo
c      END OF MAIN LOOP (defining detachment and mode of transport for each
c      cell)
cJCMay2011  Run marker-in-cell components if MiC has been set to 1
!converted JW May 2014 for xml input file format
       if (MiC.eq.1) then
        call route_markers_xml
       endif
c
cEva:  Sediment Routing following kinematic wave approach (fluxes of sediments with virtual
c      travel rates v_soil, based on mass balance of sediments)
c      d_soil(phi,1,im,jm): soil depth within water flow in current cell [mm]
c      v_soil(phi,im,jm): virtual travel velocity [mm/s]
c      q_soil(phi,1,im,jm): unit sediment discharge [mm2/s]
c      detach_soil(phi,im,jm): detachment at a point [mm/s]
c      depos: deposition at a point [mm/s]
cLTOct2007    nutrient_phi (phi, 1, im, jm): nutrient concentration/flux per cell

       do im = 2, nr1
           do jm = 2, nc1
c
cj             Eva's version with 2.5 mm depth threshold for flow routing
cj             Flow routing for depths above 0.
c
               if (d (2, im, jm).gt.0.0d0.and.rmask (im, jm).ge.0.0d0)
     &          then
                  do phi = 1, 6
c                     change detach and depos from weight per cell to mm
cJWFeb05              Convert back to rates (in mm/s) for kinematic wave calculations
cJWFeb05              after conversion to total volume in subroutine flow_distribution
cJWAug05
cJWAug05              test to see if converting to volume and then back to depth makes a
cJWAug05              difference
cJWAug05              detach_soil (phi, im, jm) = detach_soil (phi, im, jm)
cJWAug05     &                    / (density * area (im, jm) * dt)
cJWAug05              depos_soil (phi, im, jm) = depos_soil (phi, im, jm)
cJWAug05     &                   / (density * area (im, jm) * dt)
cJWAug05
cJWAug05              There is no difference, so these lines should remain commented out
cJWAug05
                      d_soil (phi, 2, im, jm) = d_soil (phi, 1, im, jm)
     &                 + (detach_soil (phi, im, jm) -
     &                    depos_soil (phi, im, jm)) * dt + (dtdx *
     &                     (add_soil (phi, im, jm) -
     &                      q_soil (phi, 1, im, jm)) )
                      if (d_soil (phi, 2, im, jm).lt.0) then
                          d_soil (phi, 2, im, jm) = 0.
                      endif
                      q_soil (phi, 2, im, jm) = d_soil (phi, 2, im, jm)
     &                                     * v_soil (phi, im, jm)
c
cLTOct2007            Added routine to calculate the sediment-bound nutrient discharge
c                     for each phi class.
c                     q_soil is in mm2/s. Use UC to conver to kg/s. P_nutrients in g/kg.
                      uc = dx * density * 1.e-6 * dt
cLT                   multiplied q_soil by UC to get kg/s
c                     multiplied by nutrient conc (g/kg) to get g/s
                      amm_q (phi, im, jm) = q_soil (phi, 2, im, jm) * UC
     &                                     * p_ammonium (phi)
                      nit_q (phi, im, jm) = q_soil (phi, 2, im, jm) * UC
     &                                     * p_nitrate (phi)
                      TN_q (phi, im, jm) = q_soil (phi, 2, im, jm) * UC
     &                                       * p_TN (phi)
                      TP_q (phi, im, jm) = q_soil (phi, 2, im, jm) * UC
     &                                       * p_TP (phi)
                      IC_q (phi, im, jm) = q_soil (phi, 2, im, jm) * UC
     &                                       * p_IC (phi)
                      TC_q (phi, im, jm) = q_soil (phi, 2, im, jm) * UC
     &                                       * p_TC (phi)
c
cLTOct2007            TASK/QUESTION need to work out how to account for net deposition and
c                     erosion of soil nutrients.
c                     Have an initial reserve of nutrients in the surface soil. This reserve
c                     is increased or decreased depending on removal or deposition of nutrients?
c                     Something like this will be needed for coupling with DAYCENT.
c
c                   Calculation of total mass fluxes (all six soil classes) in kg
c                   for spatial representation of sediment flux
c
cJWAug05         n.b. previous multiplication by uc = dx * density * 1.e-6 * dt
cJWAug05              at each time step now moved to final output because dt is constant
cJWAug05
                      sed_tot (im, jm) = sed_tot (im, jm) +
     &                                q_soil (phi, 2, im, jm)
                      detach_tot (im, jm) = detach_tot (im, jm) +
     &                                   detach_soil (phi, im, jm)
                      depos_tot (im, jm) = depos_tot (im, jm) +
     &                                 depos_soil (phi, im, jm)
                      z_change (im, jm) = z_change (im, jm)
     &                                  + depos_soil (phi, im, jm)
     &                                 - detach_soil (phi, im, jm)
c
cLTOct2007            Calculation of total mass fluxes (all six soil classes) for spatial
cLTOct2007            representation of nutrient flux in g
c
                      amm_tot (im, jm) = amm_tot (im, jm) +
     &                                amm_q (phi, im, jm)
                      nit_tot (im, jm) = nit_tot (im, jm) +
     &                                nit_q (phi, im, jm)
                      TN_tot (im, jm) = TN_tot (im, jm) +
     &                               TN_q (phi, im, jm)
                      TP_tot (im, jm) = TP_tot (im, jm) +
     &                               TP_q (phi, im, jm)
                      IC_tot (im, jm) = IC_tot (im, jm) +
     &                               IC_q (phi, im, jm)
                      TC_tot (im, jm) = TC_tot (im, jm) +
     &                               TC_q (phi, im, jm)

                  enddo

              elseif (d (2, im, jm).le.0.0d0.
     &            and.rmask (im, jm).ge.0.0d0) then
                  do phi = 1, 6
                      d_soil (phi, 2, im, jm) = 0.0d0
                      q_soil (phi, 2, im, jm) = 0.0d0
c
c                     for splash-only case, add fluxes to total for output
c
cJWAug05  n.b. previous multiplication by uc = dx * density * 1.e-6 * dt
cJWAug05       at each time step now moved to final output because dt is constant
cJWAug05
c
                      sed_tot (im, jm) = sed_tot (im, jm) +
     &                                (detach_soil (phi, im, jm) -
     &                                depos_soil (phi, im, jm))
                    detach_tot (im, jm) = detach_tot (im, jm) +
     &                                   detach_soil (phi, im, jm)
                    depos_tot (im, jm) = depos_tot (im, jm) +
     &                                  depos_soil (phi, im, jm)
                      z_change (im, jm) = z_change (im, jm)
     &                                  + depos_soil (phi, im, jm)
     &                                 - detach_soil (phi, im, jm)
cLTOct2007 add in sediment-bound nutrients
                      amm_tot (im, jm) = amm_tot (im, jm) +
     &                                ((detach_soil (phi, im, jm) -
     &                                depos_soil (phi, im, jm)
     &                                * p_ammonium (phi)))
                      nit_tot (im, jm) = nit_tot (im, jm) +
     &                                ((detach_soil (phi, im, jm) -
     &                                depos_soil (phi, im, jm)
     &                                * p_nitrate (phi)))
                      TN_tot (im, jm) = TN_tot (im, jm) +
     &                               ((detach_soil (phi, im, jm) -
     &                               depos_soil (phi, im, jm)
     &                               * p_TN (phi)))
                      TP_tot (im, jm) = TP_tot (im, jm) +
     &                               ((detach_soil (phi, im, jm) -
     &                               depos_soil (phi, im, jm)
     &                               * p_TP (phi)))
                      IC_tot (im, jm) = IC_tot (im, jm) +
     &                               ((detach_soil (phi, im, jm) -
     &                               depos_soil (phi, im, jm)
     &                               * p_IC (phi)))
                      TC_tot (im, jm) = TC_tot (im, jm) +
     &                               ((detach_soil (phi, im, jm) -
     &                               depos_soil (phi, im, jm)
     &                               * p_TC (phi)))

                  enddo
              endif
           enddo
       enddo
c
c     used for error checking
c      if (iter.eq.1) then
c         open (666, file = 'hmmm.dat', status = 'unknown')
c         rewind (666)
c         write (666, *) 'Detachment: '
c          do phi = 1, 6
c            write (666, '("phi=", i1)') phi
c            do im = 1, nr1
c                write (666, '(100(e10.4, 1x))')
c     &                           (detach_soil (phi, im, jm),
c     &                                         jm = 1, nc1)
c            enddo
c            write (666, *)
c         enddo
c         write (666, *)
c          write (666, *) 'Deposition: '
c          do phi = 1, 6
c            write (666, '("phi=", i1)') phi
c            do im = 1, nr1
c                write (666, '(100(e10.4, 1x))')
c     &                            (depos_soil (phi, im, jm),
c     &                                         jm = 1, nc1)
c            enddo
c            write (666, *)
c         enddo
c       allocate (d50_out (nr1, nc1))
c
c         write (666, *)
c          write (666, *) 'd50: '
c         do im = 1, nr1
c            do jm = 1, nc1
c               dsum = 0.0d0
c               dsumlast = 0.0d0
c               d50 = -9999.d0
c               do phi = 1, 6
c                  dsum = dsum + sed_propn (phi, im, jm)
c                  if (dsum.ge.0.5d0.and.dsumlast.lt.0.5d0) then
c                     if (phi.eq.1) then
c                        d50 = (diameter (1) / dsum) * 0.5d0
c                     else
c                        d50 = diameter (phi - 1) +
c     &                         ((diameter (phi) - diameter (phi - 1))
c     &                      / (dsum - dsumlast)) * (0.5d0 - dsumlast)
c                     endif
c                     exit
c                  endif
c                  dsumlast = dsum
c               enddo
c               if (d50.lt.0.0d0) then
c                  d50 = diameter (6)
c               endif
c               d50_out (im, jm) = d50
c            enddo
c             write (666, '(100(e10.4, 1x))')
c     &                           (d50_out (im, jm),
c     &                                         jm = 1, nc1)
c         enddo
c         write (666, *)
c         write (666, *)
c          write (666, *) 'Grain size proportions: '
c          do phi = 1, 6
c            write (666, '("phi=", i1)') phi
c            do im = 1, nr1
c                write (666, '(100(e10.4, 1x))')
c     &                           (sed_propn (phi, im, jm),
c     &                                         jm = 1, nc1)
c            enddo
c            write (666, *)
c         enddo
c
c         close (666)
c      endif

       return
       end
