c****************************************************************
c  subroutine to initialize variables at start of run (init_type = 1),
c     or during continuous run (init_type = 2)
c****************************************************************
       subroutine initialize_values_xml (init_type)

       use shared_data
       implicit double precision (a - h, o - z)
       implicit integer (i - n)

       integer *4 time_array (8)
cLTOct07 replaced
c       dimension sminit (4), ciinit (4), wt (4)
c     with
       dimension wt (4)

c      integer phi

       real timenow

       data idum / -1 /
       data wt / 3.333333333333d-2, 2.222222222222d-2, 0.d0, 0.d0 /
c
c  parameters for raindrop detachment and splash model
c  from article Wainwright et al. 1995, or Scriptum Table 3-3
c     spa is "erodibility" coefficient
c     spb is exponent on kinematic energy
c     spc is exponent on slope
c     sma for splash distribution (a1)
c     smb for splash distribution (b1)
c     spq is relationship between splash and flow depth
c     hs is depth of active sediment layer
c     radius is radius of particles in m for each class of phi
c
c
c  Particle-size classes as follows:
c
c   Size class  Size interval (mm)  Wentworth size class
c       1             < 0.0625           Clay and silt
c       2           0.0625 - 0.25        Fine sand
c       3               0.25 - 0.5         Medium sand
c       4              0.5 - 2.0         Coarse sand
c       5              2.0 - 12.0        Granules and fine pebbles
c       6               > 12.0           Coarse pebbles and larger
c
c  data values below from Savat and Poesen
c
c  spa, spb, spc, hs now read in from input file
c
c  Data values below optimized for WG in Wainwright et al. 1999 (ESPL)
cbq  Aren't the following values dependent on soil type.  If so, they should be
cbq  given as inputs by the user, and not be hardcoded.
c       data spa / 5.d-6, 9.5d-5, 6.d-5, 9.5d-5, 9.5d-6, 1.d-6 /
c       data spb / 1.24d0, 1.08d0, .79d0, 1.14d0, 1.75d0, 2.5d0 /
c       data spc / .23d0, .21d0, .11d0, 1.06d0, 1.75d0, 2.5d0 /
c ---------------------------------------------------------------------
cEva09       data sma / 1.373d0, 34.52d0, 17.459d0, 6.572d0, 3.d0, 0.d0 /
cEva09       data smb / .069d0, .091d0, .083d0, .081d0, .080d0, 0.06d0 /
cEva09       data spq / 2.72d0, 1.61d0, .92d0, .85d0, .75d0, .30d0 /
c
c set depth of active sediment layer constant to 50 mm which is
c     approximately equqivalent to 4 times diameter of largest particle
c      data hs / 50.d0, 50.d0, 50.d0, 50.d0, 50.d0, 50.d0 /
c       data hs / 0.125d0, 0.3125d0, 0.75d0, 2.5d0, 14.0d0, 48.0d0/
c       data hs / 145.d6, 245.d6, 345.d6, 445.d6, 545.0d6, 648.0d6/
c       data hs/ 6.25d-2, 1.5625d-1, 3.75d-1, 3.75d-1, 3.75d-1,
c     &          3.75d-1/
c       data hs/ 6.25d-2, 1.5625d-1, 3.75d-1, 1.25d0, 7.d0,
c     &          2.4d1/
c       data hs / 6 * 1.d3 /


       write (6, *) ' Initializing variables'

       if (idum.eq.-1) then
c
c   initialize random numbers
c
cJWMar09 Use f90 intrinsic call to date_and_time subroutine to initialize random number generator
! time_array(1)    year
! time_array(2)    month of the year
! time_array(3)    day of the month
! time_array(4)    time offset with respect to UTC in minutes
! time_array(5)    hour of the day
! time_array(6)    minutes of the hour
! time_array(7)    seconds of the minute
! time_array(8)    milliseconds of the second
! multiplying day, hour, min, sec, millisec gives 2675721600 discrete values
          call date_and_time (values = time_array)
          idum = time_array (3)
          do i = 5, 8
             idum = idum * time_array (i)
          enddo
          idum = -idum
          xdum = ran1  (idum)
       endif

       dtdx = dt / dx
       dt2dx = dt / (2. * dx)
c
c   sigma is relative density in kg/m^3
c
       sigma = (1.d3 * density - 1.d3) / 1.d3
c
c   p_par is constant term used to define pickup probability in concentrated flow
c
       p_par = -2.d0 / pi
c
c   dstar_const is constant used in suspension criterion
c
       dstar_const = (((sigma - 1.0d0) * 9.81d0) /
     &             (viscosity ** 2)) ** (1.d0 / 3.d0)

c
c   initial saturation (ciinit) and maximum saturation (sminit)
c
cLTOct2007  do i = 1,4 because model was once set up for 4 diff surface types
c           - not needed now so commented out and made spatially distributed
c
c       do i = 1, 4
c         sminit (i) = theta_sat (i) * soil_thick (i) * 1000.
c         ciinit (i) = theta_0 (i) * soil_thick (i) * 1000.
c       enddo
c
cLTOct2007 Make ciinit and sminit spatially distributed
       do i = 2, nr2
          do j = 2, nc2
           ciinit (i, j) = theta_0 (i, j) * soil_thick (1) * 1000.
             sminit (i, j) = theta_sat (i, j) * soil_thick (1) * 1000
        enddo
       enddo

c   gravel and fines proportions
       do i = 2, nr2
          do j = 2, nc2
            grav (i,j) = 100.0d0 * (ps_init_ave (5,i,j) +
     &           ps_init_ave (6,i,j))
            fines (i,j) = 100.0d0 - grav (i,j)
        enddo
       enddo


       do phi = 1, 6
c
c   set maximum number of splash redistribution steps equivalent to distance of 3 m
c   set maximum number of diffuse flow redistribution steps equivalent to distance of 10 m
c   set maximum number of concentrated flow redistribution steps equivalent to distance of 100 m
c   set maximum number of suspended flow redistribution steps equivalent to distance of 500 m
cb  nmax_splash = number of cells crossed by the splash (equivalent of 3 m)
cJWJan2006 set to 1 for large cells
cb  nmax_flow = number of cells crossed by sediment carried by flow (equivalent of 10 m)
c       max (*, 2) ensures that some movement is always calculated
cb  pave = surface covered by gravel (size classes 5 and 6).
cb 100 - pave = surface covered by fines (size classes 1-4)
c
          if (dx.gt.300.0d0) then
             nmax_splash (phi) = max (int (5.0d0 / (min (dx, dy) /
     &                           1.0d3) + 0.5d0), 2)
          else
             nmax_splash (phi) = 1
          endif
          nmax_diffuse_flow (phi) =
     &                       max (int (10.d0 / (min (dx, dy) / 1.0d3) +
     &                       0.5d0), 2)
          nmax_conc_flow (phi) =
     &                    max (int (100.d0 / (min (dx, dy) / 1.0d3) +
     &                    0.5d0), 2)
          nmax_susp_flow (phi) =
     &                    max (int (500.d0 / (min (dx, dy) / 1.0d3) +
     &                    0.5d0), 2)
c
c   scaling of spa to account for time units used in Quansah's original paper, so the
c      calculation doesn't need repeating every time step
c
          spa (phi) = spa (phi) / 1.2d3

          do i = 1, nr
             do j = 1, nc
c
c               account for correction factor applied in read routine (rel to infiltration calc)
c
                pave_perc = pave (i, j) * 1.d4
                if (pave_perc.le.0.0d0.or.grav (i, j).eq.0.0d0) then
                   sed_propn (phi, i, j) = ps_init_ave (phi,i,j)
                else
                   if (phi.le.4) then
                      sed_propn (phi, i, j) = ps_init_ave (phi,i,j) *
     &                                        (100.0d0 - pave_perc) /
     &                                        fines (i,j)
                   else
                      sed_propn (phi, i, j) = ps_init_ave (phi,i,j) *
     &                                        pave_perc / grav (i,j)
                   endif
                endif
             enddo
          enddo

c   set active sediment layer sensitivity
c         hs (phi) = hs (phi) * hz
c
c
c   initialize diameters from radius
c
          diameter (phi) = 2.d0 * radius (phi)
c
c   settling velocities
c
          if (diameter (phi).lt.1.d-4) then
             settling_vel (phi) = sigma * 9.81 * diameter (phi) ** 2 /
     &                            (18.d0 * viscosity)
          else
             settling_vel (phi) = 1.1d0 * sqrt (sigma * 9.81 *
     &                            diameter (phi))
          endif
c
c         set splash redistribution functions
c             nomov (phi) = amount of splashed material that does not exit cell
c             redist (phi, ncell) = amount of splashed material reaching ncells away
c             dx/10 converts to cm as per original functions
c
          nomov (phi) = sma (phi) * ((1.0d0 - exp (-smb (phi) *
     &                  (dx / 10.0d0))) / smb (phi))
          spsum = nomov (phi)
          do k = 1, nmax_splash (phi)
             spdist (phi, k) = sma (phi) * ((exp (-smb (phi) *
     &                  (dble (k) * dx / 10.0d0)) - exp (-smb (phi) *
     &                  (dble (k + 1) * dx / 10.0d0))) / smb (phi))
             spsum = spsum + spdist (phi, k)
          enddo

c
c   normalize distributions to sum to 1
c
          nomov (phi) = nomov (phi) / spsum
          do k = 1, nmax_splash (phi)
             spdist (phi, k) = spdist (phi, k) / spsum
          enddo
       enddo

c      write (6, *) ' Initialized sed transport, now arrays'

       if (init_type.eq.1) then
c
c   reset all variables
c
          do i = 1, nr2
             do j = 1, nc2
                do k = 1, 2
                   d (k, i, j) = 0.0d0
                   q (k, i, j) = 0.0d0
                 ammonium (k, i, j) = 0.0d0
                 nitrate (k, i, j) = 0.0d0
                 phosphorus (k, i, j) = 0.0d0
                enddo
                qsum (i, j) = 0.0d0
                if (inf_type.ne.4) then
                   ksat (i, j) = 0.0d0
                endif
                psi (i, j) = 0.0d0
                drain_par (i, j) = 0.0d0
                if (ff_type.ne.8) then
                   ff (i, j) = 0.0d0
                endif
                iveg = 1
c                if (iveg.lt.1.or.iveg.gt.4) then
c                   iveg = 1
c                endif

c     Eva: define different initial conditions for various different
c     parameterisation approaches
c     model_type = 1    average parameters will be used
c     model_type = 2    binary system, parameters in dependence on
c                   vegetated (cover=1) or bare (cover=2) surface cover
c     model_type = 3 stochastic simulation with Ksat read in via infiltration map
c     model_type = 4 stochastic simulation with ff read in via pavement map
c     model_type = 5 stochastic simulation for Ksat and ff

cLTOct2007 The below sections are required to initialise the current soil moisture content and ciinit, which is not used elsewhere?!
c        These were just used for evas different approaches.

cj          if(model_type.eq.1) then
c           average values
cj                stmax (i, j) = sminit (1)
cj                theta (i, j) = theta_0 (1)
cj                cum_inf (i, j) = ciinit (1)
cj                zs (i, j) = soil_thick (1)
cj                ksat (i,j) = ksave (1)
cj                  ff(i,j)=ffave(1)
cj
cj          elseif (model_type.eq.2) then
c           vegetated:
cj              if (cover(i,j).eq.1) then
cj                stmax (i, j) = sminit (1)
cj                theta (i, j) = theta_0 (1)
cj                cum_inf (i, j) = ciinit (1)
cj                zs (i, j) = soil_thick (1)
cj                ksat (i,j) = ksave (1)
cj                ff(i,j)=ffave(1)
c
c           bare:
cj              elseif (cover(i,j).eq.2) then
cj                stmax (i, j) = sminit (2)
cj                theta (i, j) = theta_0 (2)
cj                cum_inf (i, j) = ciinit (2)
cj                zs (i, j) = soil_thick (2)
cj                ksat (i,j) = ksave (2)
cj                ff(i,j)=ffave(2)
cj              endif

c     Stochastic simulation for Ksat + ff
cj              elseif (model_type.eq.5) then
cj                stmax (i, j) = sminit (1)
cj                theta (i, j) = theta_0 (1)
cj                cum_inf (i, j) = ciinit (1)
cj                zs (i, j) = soil_thick (1)
cj          endif
c
c   /  1.d6 converts from mm^2 to m^2
c
                area (i, j) = (dx * dy) / 1.d6
                sedch (i, j) = 0.0d0
                sed_tot (i, j) = 0.0d0
                detach_tot (i, j) = 0.0d0
                depos_tot (i, j) = 0.0d0
                raindrop_detach_tot (i, j) = 0.0d0
              flow_detach_tot (i, j) = 0.0d0
c
cLTOct2007 reset sediment-bound nutrients too
c
                amm_tot (i, j) = 0.0d0
                nit_tot (i, j) = 0.0d0
                TN_tot (i, j) = 0.0d0
                TP_tot (i, j) = 0.0d0
                IC_tot (i, j) = 0.0d0
                TC_tot (i, j) = 0.0d0

c   infiltration parameters
c

              if (inf_type.eq.1) then
c
cLTOct2007 ANSWER   use pavement- and rainfall-based parameterization using ave rf intensity
c
                   ksat (i, j) = 0.00585 + 0.000166667 * rf_mean -
     &                           pave (i, j)
                   psi (i, j) = psiave (iveg)
                   drain_par (i, j) = drain_par_ave (iveg)
                elseif (inf_type.eq.2) then
c
c   use values of mean (and sd) ksat from input file to define deterministic
c       (or stochastic) values
c
c                   write (6, *) ' setting infiltration params - Ksat'
c                   write (6, *) ' ksave = ', ksave (iveg),
c     &                          ' kssd = ', kssd (iveg)
                   do while (ksave (iveg).gt.0.0d0.and.
     &                       ksat (i, j).le.0.)
                      ksat (i, j) = rand_from_norm (ksave (iveg),
     &                           kssd (iveg), idum)
                   enddo
c                   write (6, *) ' setting infiltration params - psi'
c                   write (6, *) ' psiave = ', psiave (iveg),
c     &                          ' psisd = ', psisd (iveg)
                   do while (psiave (iveg).gt.0.0d0.and.
     &                       psi (i, j).le.0.)
                      psi (i, j) = rand_from_norm (psiave (iveg),
     &                          psisd (iveg), idum)
c                       temp = rand_from_norm (psiave (iveg),
c     &                          psisd (iveg), idum)
c                       temp1 = rand_from_norm (46.6d0,0.0d0, idum)
c                      psi (i, j) = temp
                   enddo
c                   write (6, *) ' setting infiltration params - drain'
                   do while (drain_par_ave (iveg).gt.0.0d0.and.
     &                     drain_par (i, j).le.0.)
                      drain_par (i, j) = rand_from_norm (
     &                                   drain_par_ave (iveg),
     &                                   drain_par_sd (iveg),
     &                                   idum)
                   enddo
c                   write (6, *) ' finished setting infiltration params'
                elseif (inf_type.eq.3) then
c
c   use pavement- and rainfall-based parameterization using dynamic rf intensity
c       ksat is thus not defined here but in the set_rain routine
c
                   psi (i, j) = psiave (iveg)
                   drain_par (i, j) = drain_par_ave (iveg)
                elseif (inf_type.eq.4) then
c
c   use ksat map file to define ksat
c
                   psi (i, j) = psiave (iveg)
                   drain_par (i, j) = drain_par_ave (iveg)
                elseif (inf_type.eq.5) then
cJWFeb2006
cJWFeb2006   use simplified Green and Ampt
cJWFeb2006
cLT_QUESTION these ways of calculating ksat seem to come up with negative values??????
c                  ksat (i, j) = (0.00585 + 0.000166667 * rf_mean -
c     &                           pave (i, j)) * 60.0d0  !mm/min
                   ksat (i, j) = (1.45 - 0.014 * (pave (i, j) * 1.d4)) *
     &                           ksat_mod
cLT_QUESTION why is the above ksat derivation different from that which is in ESPL in press paper 2, equation 1a?
cLT_QUESTION eq 1a in espl paper 2 does not appear to feature anywhere - why?
cuse rainfall variable 1 - gave better results
                 psi (i, j) = 0.785 + 0.021 * (pave (i, j) * 1.d4)
cLT_QUESTION how data specific is the above equation for psi (the b storage parameter) since it was derived from experimental data from creosotebush
c John wouldn't trust this much b is difficult to get good. above is completely unverified. unreliable. can stick to constant value of 5 using clapham honberg type equation - in kinematic wave book in 1st chapter.
cLTOct2007 therefore could keep psi set to value read in, and thus include as above:
c                psi (i, j) = psiave (iveg)
                 drain_par (i, j) = drain_par_ave (iveg)
                endif
cJWMay05
cJWMay05  multiply through by calibration factor psi_mod
cJWMay05
cLT here theta is defined as theta_0. The 1 after theta_0 is because it reads in 4 values
cLTOct07 changed
cLTOct07        theta (i, j) = theta_0 (1)
cLTOct07          cum_inf (i, j) = ciinit (1)

                psi (i, j) = psi (i, j) * psi_mod
              stmax (i, j) = sminit (i, j)
              theta (i, j) = theta_0 (i,j)
              cum_inf (i, j) = ciinit (i,j)
              zs (i, j) = soil_thick (1)
c
c   friction-factor parameters
c
                if (ff_type.eq.1) then
                   ff (i, j) = ffave (iveg)
                elseif (ff_type.eq.2) then
                   do while (ff (i, j).le.0.)
                      ff (i, j) = rand_from_norm (ffave (iveg),
     &                            ffsd (iveg), idum)
                   enddo
                elseif (ff_type.eq.3) then
c
c   dynamic versions -- initialize
c
                   ff (i, j) = 14.d0
                elseif (ff_type.eq.4) then
c
c  logf = -1.099 +0.024%G - 0.313 log Re + 0.915logDg
c
                   if (sed_propn (5, i, j).gt.0.0d0.or.
     &                  sed_propn (6, i, j).gt.0.0d0) then
                      if (sed_propn (5, i, j).gt.sed_propn (6, i, j))
     &                     then
                         dg = 2.0d0 + 10.0d0 * (sed_propn (5, i, j) /
     &                        (sed_propn (5, i, j) +
     &                         sed_propn (6, i, j)))
                      else
                         dg = 12.0d0 + (20.0d0 * sed_propn (6, i, j))
                      endif
                   else
                      dg = 2.0d0
                   endif
                   if (pave (i, j).ge.0.0d0) then
                        pave_perc = pave (i, j) * 1.d4
                   else
                      pave_perc = 0.0d0
                   endif
                   ff (i, j) = 10.d0 ** (2.4d3 * pave_perc) *
     &                         (dg ** 0.915)
                elseif (ff_type.eq.5) then

c  ff = 9.143 x 10^-6 Re^-0.307 Dg^1.025 P%^3.470
c
                   if (sed_propn (5, i, j).gt.0.0d0.or.
     &                 sed_propn (6, i, j).gt.0.0d0) then
                      if (sed_propn (5, i, j).gt.sed_propn (6, i, j))
     &                     then
                         dg = 2.0d0 + 10.0d0 * (sed_propn (5, i, j) /
     &                        (sed_propn (5, i, j) +
     &                         sed_propn (6, i, j)))
                      else
                         dg = 12.0d0 + (20.0d0 * sed_propn (6, i, j))
                      endif
                   else
                      dg = 2.0d0
                   endif
                   if (pave (i, j).ge.0.0d0) then
                      pave_perc = pave (i, j) * 1.d4
                   else
                      pave_perc = 0.0d0
                   endif
                   ff (i, j) = 9.143d-6 * (pave_perc ** 3.470) *
     &                         (dg ** 1.025)
                   if (ff (i, j).lt.0.1d0) then
                      ff (i, j) = 0.1d0
                   endif
                elseif (ff_type.eq.6) then
c
c  ff = Re^0.33, with default 16.17 for Re=0
c
                   ff (i, j) = 16.17
                elseif (ff_type.eq.7) then
c
c  ff = 1.202 Dg^1.383 Q^-0.317
c
                   if (sed_propn (5, i, j).gt.0.0d0.or.
     &                 sed_propn (6, i, j).gt.0.0d0) then
                      if (sed_propn (5, i, j).gt.sed_propn (6, i, j))
     &                     then
                         dg = 2.0d0 + 10.0d0 * (sed_propn (5, i, j) /
     &                        (sed_propn (5, i, j) +
     &                         sed_propn (6, i, j)))
                      else
                         dg = 12.0d0 + (20.0d0 * sed_propn (6, i, j))
                      endif
                   else
                      dg = 2.0d0
                   endif
                   ff (i, j) = 1.202d0 * dg ** 1.383d0
!                elseif (ff_type.eq.8) then
! can't do here because reading in all values in one go
                endif
             enddo
          enddo
       else
c
c   just reset flux variables
c
          do i = 1, nr2
             do j = 1, nc2
                do k = 1, 2
                   d (k, i, j) = 0.
                   q (k, i, j) = 0.
                 ammonium (k, i, j) = 0.0d0
                 nitrate (k, i, j) = 0.0d0
                 phosphorus (k, i, j) = 0.0d0
                enddo
                sedch (i, j) = 0.0d0
                sed_tot (i, j) = 0.0d0
                detach_tot (i, j) = 0.0d0
                depos_tot (i, j) = 0.0d0
                raindrop_detach_tot (i, j) = 0.0d0
              flow_detach_tot (i, j) = 0.0d0
c
cLTOct2007 added sediment-bount nutrients
c
                amm_tot (i, j) = 0.0d0
                nit_tot (i, j) = 0.0d0
                TN_tot (i, j) = 0.0d0
                TP_tot (i, j) = 0.0d0
                IC_tot (i, j) = 0.0d0
                TC_tot (i, j) = 0.0d0

             enddo
          enddo
       endif

       return
       end
