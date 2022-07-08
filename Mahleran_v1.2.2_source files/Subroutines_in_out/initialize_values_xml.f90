!****************************************************************
!  subroutine to initialize variables at start of run (init_type = 1),
!     or during continuous run (init_type = 2)
!****************************************************************
subroutine initialize_values_xml (init_type)

!
!   conversions from original version to xml version:
!       ksave --> final_infiltration_rate_mean
!       kssd --> final_infiltration_rate_std_dev
!       psiave --> wetting_front_suction_mean
!       psisd --> wetting_front_suction_std_dev
!       drain_par_ave --> drainage_parameter_mean
!       drain_par_sd --> drainage_parameter_std_dev
!       ffave --> friction_factor_mean
!       ffsd --> friction_factor_std_dev
!

use shared_data
use parameters_from_xml

implicit none

integer *4 time_array (8)

integer :: init_type
integer :: i, j, k
integer :: iveg
integer *4 :: idum
!
!cLTOct07 replaced
!       dimension sminit (4), ciinit (4), wt (4)
!   with
double precision :: wt (4)
double precision :: xdum
double precision :: dg
double precision :: rand_from_norm
double precision :: ran1
double precision :: pave_perc
double precision :: spsum
double precision :: unit_conv

real :: timenow

data idum / -1 /
data wt / 3.333333333333d-2, 2.222222222222d-2, 0.d0, 0.d0 /
!
!  parameters for raindrop detachment and splash model
!  from article Wainwright et al. 1995, or Scriptum Table 3-3
!     spa is "erodibility" coefficient
!     spb is exponent on kinematic energy
!     spc is exponent on slope
!     sma for splash distribution (a1)
!     smb for splash distribution (b1)
!     spq is relationship between splash and flow depth
!     hs is depth of active sediment layer
!     radius is radius of particles in m for each class of phi
!
!
!  Particle-size classes as follows:
!
!   Size class  Size interval (mm)  Wentworth size class
!       1             < 0.0625           Clay and silt
!       2           0.0625 - 0.25        Fine sand
!       3               0.25 - 0.5         Medium sand
!       4              0.5 - 2.0         Coarse sand
!       5              2.0 - 12.0        Granules and fine pebbles
!       6               > 12.0           Coarse pebbles and larger
!
!  data values below from Savat and Poesen
!
!  spa, spb, spc, hs now read in from input file
!
!  Data values below optimized for WG in Wainwright et al. 1999 (ESPL)
!       data spa / 5.d-6, 9.5d-5, 6.d-5, 9.5d-5, 9.5d-6, 1.d-6 /
!       data spb / 1.24d0, 1.08d0, .79d0, 1.14d0, 1.75d0, 2.5d0 /
!       data spc / .23d0, .21d0, .11d0, 1.06d0, 1.75d0, 2.5d0 /
! ---------------------------------------------------------------------
!Eva09       data sma / 1.373d0, 34.52d0, 17.459d0, 6.572d0, 3.d0, 0.d0 /
!Eva09       data smb / .069d0, .091d0, .083d0, .081d0, .080d0, 0.06d0 /
!Eva09       data spq / 2.72d0, 1.61d0, .92d0, .85d0, .75d0, .30d0 /
!
! set depth of active sediment layer constant to 50 mm which is
!   approximately equqivalent to 4 times diameter of largest particle
!    data hs / 50.d0, 50.d0, 50.d0, 50.d0, 50.d0, 50.d0 /
!       data hs / 0.125d0, 0.3125d0, 0.75d0, 2.5d0, 14.0d0, 48.0d0/
!       data hs / 145.d6, 245.d6, 345.d6, 445.d6, 545.0d6, 648.0d6/
!       data hs/ 6.25d-2, 1.5625d-1, 3.75d-1, 3.75d-1, 3.75d-1, 3.75d-1/
!       data hs/ 6.25d-2, 1.5625d-1, 3.75d-1, 1.25d0, 7.d0, 2.4d1/
!       data hs / 6 * 1.d3 /

write (6, *) ' Initializing variables'

! dealt with explicitly elsewhere, but set for compatibility
if (model_run_type.eq.'MiC'.or.model_run_type.eq.'continuous_MiC') then
   MiC = 1   !cJCMay2011 for MiC
endif


!
!  convert xml versions of parameters to existing model versions
!
!  surface characteristics
!
do i = 1, n_types
   write (6, *) 'Setting up surface type ', i
   ksave (i) = final_infiltration_rate_mean (i)
   kssd (i) = final_infiltration_rate_std_dev (i)
   psiave (i) = wetting_front_suction_mean (i)
   psisd (i) = wetting_front_suction_std_dev (i)
   drain_par_ave (i) = drainage_parameter_mean (i)
   drain_par_sd (i) = drainage_parameter_std_dev (i)
   ffave (i) = friction_factor_mean (i)
   ffsd (i) = friction_factor_std_dev (i)
   soil_thick (i) = soil_thickness (i)
   write (6, *) soil_thick (i)
enddo
ndirn = flow_direction
if (ndirn.ne.4.and.ndirn.ne.8) then
   write (6, *) ' Error in input - flow_direction should only be 4 or 8 '
   stop
elseif (ndirn.ne.4) then
   write (6, *) ' Error in input - flow_direction should only be 4'
   stop
endif
iroute = flow_routing_solution_method
if (iroute.lt.1.or.iroute.gt.7) then
   write (6, *) ' Error in input - flow_routing_solution_method should be between 1 and 7'
   stop
endif
rain_type = rain_type_xml
stormlength = stormlength_xml
dt = time_step
density = particle_density
excess_density = 1000. * (density - 1.)
bagnold_density_scale = (excess_density / 1650.) ** (-0.5)
hz = active_layer_sensitivity
if (update_topography_xml.eq.'y'.or.update_topography_xml.eq.'Y') then
   update_topography = .TRUE.
else
   update_topography = .FALSE.
endif
!
! LTOct2007 Cs is in mg/l '(ammonium, nitrate, phosphorus):'
!
Cs (1) = Cs_NH4
Cs (2) = Cs_NO3
Cs (3) = Cs_PO
! mass transfer coefficients '(ammonium, nitrate, phosphorus): '
alpha (1) = mass_transfer_coefficient_NH4
alpha (2) = mass_transfer_coefficient_NO3
alpha (3) = mass_transfer_coefficient_PO
!
!  Detachment parameters
!
do i = 1, 6
! raindrop detachment a parameter '
   spa (i) = Raindrop_detachment_a_parameter_size (i)
   spb (i) = Raindrop_detachment_b_parameter_size (i)
   spc (i) = Raindrop_detachment_c_parameter_size (i)
   spd (i) = Raindrop_detachment_d_parameter_size (i)
   hs (i) = Raindrop_detachment_max_parameter_size (i)
!
!LTOct2007  Read in concentration of particulate-bound nutrient for each phi class.
!
   p_ammonium (i) = Particulate_bound_NH4 (i)
   p_nitrate (i) = Particulate_bound_NO3 (i)
   p_TN (i) = Particulate_bound_TN (i)
   p_TP (i) = Particulate_bound_TP (i)
   p_IC (i) = Particulate_bound_IC (i)
   p_TC (i) = Particulate_bound_TC (i)
enddo
!
!LTOct2007  Read in dissolved rainfall nutrient concentrations.
!
Rn (1) = rainfall_dissolved_NH4_conc
Rn (2) = rainfall_dissolved_NO3_conc
Rn (3) = rainfall_dissolved_PO_conc
write (6, *) 'converted xml versions of parameters to existing model versions'
if (idum.eq.-1) then
!
!   initialize random numbers
!
!JWMar09 Use f90 intrinsic call to date_and_time subroutine to initialize random number generator
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
write (6, *) 'initialized random-number generator'

dtdx = dt / dx
dt2dx = dt / (2. * dx)
!
!   sigma is relative density in kg/m^3
!
sigma = (1.d3 * density - 1.d3) / 1.d3
!
!   p_par is constant term used to define pickup probability in concentrated flow
!
p_par = -2.d0 / pi
!
!   dstar_const is constant used in suspension criterion
!
dstar_const = (((sigma - 1.0d0) * 9.81d0) / (viscosity ** 2)) ** (1.d0 / 3.d0)
write (6, *) 1, nr2, nc2
!
!   initial saturation (ciinit) and maximum saturation (sminit)
!
do i = 2, nr2
   do j = 2, nc2
      iveg = cover (i, j)  !cover confusingly contains the integer values of the surface types
!      write (6, *) i, j, iveg, cover (i, j)
      ciinit (i, j) = theta_0 (i, j) * soil_thick (iveg) * 1000.
      sminit (i, j) = theta_sat (i, j) * soil_thick (iveg) * 1000
   enddo
enddo
write (6, *) 2

!   gravel and fines proportions
do i = 2, nr2
   do j = 2, nc2
      grav (i, j) = 100.0d0 * (ps_init_ave (5, i, j) + ps_init_ave (6, i, j))
      fines (i, j) = 100.0d0 - grav (i, j)
   enddo
enddo

write (6, *) 'Starting distribution-step calculations'
do phi = 1, 6
!
!   set maximum number of splash redistribution steps equivalent to distance of 3 m
!   set maximum number of diffuse flow redistribution steps equivalent to distance of 10 m
!   set maximum number of concentrated flow redistribution steps equivalent to distance of 100 m
!   set maximum number of suspended flow redistribution steps equivalent to distance of 500 m
!b  nmax_splash = number of cells crossed by the splash (equivalent of 3 m)
!JWJan2006 set to 1 for large cells
!b  nmax_flow = number of cells crossed by sediment carried by flow (equivalent of 10 m)
!       max (*, 2) ensures that some movement is always calculated
!b  pave = surface covered by gravel (size classes 5 and 6).
!b 100 - pave = surface covered by fines (size classes 1-4)
!
   if (dx.gt.300.0d0) then
      nmax_splash (phi) = max (int (5.0d0 / (min (dx, dy) / 1.0d3) + 0.5d0), 2)
   else
      nmax_splash (phi) = 1
   endif
   nmax_diffuse_flow (phi) = max (int (10.d0 / (min (dx, dy) / 1.0d3) + 0.5d0), 2)
   nmax_conc_flow (phi) = max (int (100.d0 / (min (dx, dy) / 1.0d3) + 0.5d0), 2)
   nmax_susp_flow (phi) = max (int (500.d0 / (min (dx, dy) / 1.0d3) +  0.5d0), 2)
!
!   scaling of spa to account for time units used in Quansah's original paper, so the
!      calculation doesn't need repeating every time step
!
   spa (phi) = spa (phi) / 1.2d3

!   do i = 1, nr
!      do j = 1, nc
!
!   account for correction factor applied in read routine (rel to infiltration calc)
!NOW IN MAHLERAN_storm_setting
!         pave_perc = pave (i, j) * 1.d4
!    if (pave_perc.le.0.0d0.or.grav (i, j).eq.0.0d0) then
!       sed_propn (phi, i, j) = ps_init_ave (phi,i,j)
!         else
!       if (phi.le.4) then
!               sed_propn (phi, i, j) = ps_init_ave (phi,i,j) *  (100.0d0 - pave_perc) / fines (i,j)
!       else
!               sed_propn (phi, i, j) = ps_init_ave (phi,i,j) * pave_perc / grav (i,j)
!       endif
!         endif
!      enddo
!   enddo
!
!   set active sediment layer sensitivity
!       hs (phi) = hs (phi) * hz
!
!
!   initialize diameters from radius
!
   diameter (phi) = 2.d0 * radius (phi)
!
!   settling velocities
!
   if (diameter (phi).lt.1.d-4) then
      settling_vel (phi) = sigma * 9.81 * diameter (phi) ** 2 / (18.d0 * viscosity)
   else
      settling_vel (phi) = 1.1d0 * sqrt (sigma * 9.81 * diameter (phi))
   endif
!
!         set splash redistribution functions
!             nomov (phi) = amount of splashed material that does not exit cell
!             redist (phi, ncell) = amount of splashed material reaching ncells away
!             dx/10 converts to cm as per original functions
!
   nomov (phi) = sma (phi) * ((1.0d0 - exp (-smb (phi) * (dx / 10.0d0))) / smb (phi))
   spsum = nomov (phi)
   do k = 1, nmax_splash (phi)
      spdist (phi, k) = sma (phi) * ((exp (-smb (phi) *  (dble (k) * dx / 10.0d0)) - exp (-smb (phi) * &
                        (dble (k + 1) * dx / 10.0d0))) / smb (phi))
      spsum = spsum + spdist (phi, k)
   enddo

!
!   normalize distributions to sum to 1
!
   nomov (phi) = nomov (phi) / spsum
   do k = 1, nmax_splash (phi)
      spdist (phi, k) = spdist (phi, k) / spsum
   enddo
enddo

!
write (6, *) ' Initialized sed transport, now arrays'
!
if (init_type.eq.1) then
!
!   reset all variables
!
   d (:, :, :) = 0.0d0
   q (:, :, :) = 0.0d0
   ammonium (:, :, :) = 0.0d0
   nitrate (:, :, :) = 0.0d0
   phosphorus (:, :, :) = 0.0d0
   qsum (:, :) = 0.0d0
!
!   /  1.d6 converts from mm^2 to m^2
!
   unit_conv = (dx * dy) / 1.d6
   area (:, :) = unit_conv
   sedch (:, :) = 0.0d0
   sed_tot (:, :) = 0.0d0
   detach_tot (:, :) = 0.0d0
   depos_tot (:, :) = 0.0d0
   raindrop_detach_tot (:, :) = 0.0d0
   flow_detach_tot (:, :) = 0.0d0
!
!LTOct2007 reset sediment-bound nutrients too
!
   amm_tot (:, :) = 0.0d0
   nit_tot (:, :) = 0.0d0
   TN_tot (:, :) = 0.0d0
   TP_tot (:, :) = 0.0d0
   IC_tot (:, :) = 0.0d0
   TC_tot (:, :) = 0.0d0

   stmax (:, :) = sminit (:, :)
   theta (:, :) = theta_0 (:, :)
   cum_inf (:, :) = ciinit (:, :)
   do i = 2, nr2
      do j = 2, nc2
         iveg = cover (i, j)  !cover confusingly contains the integer values of the surface types
         zs (i, j) = soil_thick (iveg)
      enddo
   enddo

!   do i = 1, nr2
!      do j = 1, nc2
!
!
!   all these variables now defined in MAHLERAN_storm_setting
!    if (inf_type.ne.4) then
!            ksat (i, j) = 0.0d0
!    endif
!         psi (i, j) = 0.0d0
!         drain_par (i, j) = 0.0d0
!         if (ff_type.ne.8) then
!            ff (i, j) = 0.0d0
!         endif
!         iveg = cover (i, j)
!         if (iveg.lt.1.or.iveg.gt.4) then
!            iveg = 1
!         endif

!   Eva: define different initial conditions for various different
!   parameterisation approaches
!   model_type = 1  average parameters will be used
!   model_type = 2  binary system, parameters depending on
!                   vegetated (cover=1) or bare (cover=2) surface cover
!   model_type = 3 stochastic simulation with Ksat read in via infiltration map
!   model_type = 4 stochastic simulation with ff read in via pavement map
!   model_type = 5 stochastic simulation for Ksat and ff

!LTOct2007 The below sections are required to initialise the current soil moisture content and ciinit, which is not used elsewhere?!
!        These were just used for evas different approaches.

!       if(model_type.eq.1) then
!           average values
!                 stmax (i, j) = sminit (1)
!                 theta (i, j) = theta_0 (1)
!                 cum_inf (i, j) = ciinit (1)
!                 zs (i, j) = soil_thick (1)
!                 ksat (i,j) = ksave (1)
!                   ff(i,j)=ffave(1)
!
!           elseif (model_type.eq.2) then
!           vegetated:
!               if (cover(i,j).eq.1) then
!                 stmax (i, j) = sminit (1)
!                 theta (i, j) = theta_0 (1)
!                 cum_inf (i, j) = ciinit (1)
!                 zs (i, j) = soil_thick (1)
!                 ksat (i,j) = ksave (1)
!                 ff(i,j)=ffave(1)
!
!           bare:
!               elseif (cover(i,j).eq.2) then
!                 stmax (i, j) = sminit (2)
!                 theta (i, j) = theta_0 (2)
!                 cum_inf (i, j) = ciinit (2)
!                 zs (i, j) = soil_thick (2)
!                 ksat (i,j) = ksave (2)
!                 ff(i,j)=ffave(2)
!               endif

!   Stochastic simulation for Ksat + ff
!               elseif (model_type.eq.5) then
!                 stmax (i, j) = sminit (1)
!                 theta (i, j) = theta_0 (1)
!                 cum_inf (i, j) = ciinit (1)
!                 zs (i, j) = soil_thick (1)
!           endif
!
!   infiltration parameters
!
! INFILTRATION COMMENTED OUT HERE -- NOW ALL DONE IN MAHLERAN_storm_setting
!         if (inf_type.eq.1) then
!
!LTOct2007 ANSWER   use pavement- and rainfall-based parameterization using ave rf intensity
!
!            ksat (i, j) = 0.00585 + 0.000166667 * rf_mean - pave (i, j)
!            psi (i, j) = wetting_front_suction_mean (iveg)
!            drain_par (i, j) = drainage_parameter_mean (iveg)
!         elseif (inf_type.eq.2) then
!
!   use values of mean (and sd) ksat from input file to define deterministic
!       (or stochastic) values
!
!                   write (6, *) ' setting infiltration params - Ksat'
!                   write (6, *) ' ksave = ', ksave (iveg),
!     &                          ' kssd = ', kssd (iveg)
!            do while (final_infiltration_rate_mean (iveg).gt.0.0d0.and.ksat (i, j).le.0.)
!               ksat (i, j) = rand_from_norm (final_infiltration_rate_mean (iveg), final_infiltration_rate_std_dev (iveg), idum)
!            enddo
!           write (6, *) ' setting infiltration params - psi'
!                   write (6, *) ' psiave = ', psiave (iveg),
!     &                          ' psisd = ', psisd (iveg)
!            do while (wetting_front_suction_mean (iveg).gt.0.0d0.and.psi (i, j).le.0.)
!               psi (i, j) = rand_from_norm (wetting_front_suction_mean (iveg), wetting_front_suction_std_dev (iveg), idum)
!                       temp = rand_from_norm (psiave (iveg),
!     &                          psisd (iveg), idum)
!                       temp1 = rand_from_norm (46.6d0,0.0d0, idum)
!                    psi (i, j) = temp
!            enddo
!                   write (6, *) ' setting infiltration params - drain'
!            do while (drainage_parameter_mean (iveg).gt.0.0d0.and.drain_par (i, j).le.0.)
!               drain_par (i, j) = rand_from_norm (drainage_parameter_mean (iveg), drainage_parameter_std_dev (iveg), idum)
!            enddo
!                   write (6, *) ' finished setting infiltration params'
!         elseif (inf_type.eq.3) then
!
!   use pavement- and rainfall-based parameterization using dynamic rf intensity
!       ksat is thus not defined here but in the set_rain routine
!
!         psi (i, j) = wetting_front_suction_mean (iveg)
!         drain_par (i, j) = drainage_parameter_mean (iveg)
!      elseif (inf_type.eq.4) then
!
!   use ksat map file to define ksat
!
!         write (6, *) 'inf_type.eq.4, ksat (i, j)=', ksat (i, j)
!         psi (i, j) = wetting_front_suction_mean (iveg)
!         drain_par (i, j) = drainage_parameter_mean (iveg)
!      elseif (inf_type.eq.5) then
!
!JWFeb2006
!JWFeb2006   use simplified Green and Ampt
!JWFeb2006
!LT_QUESTION these ways of calculating ksat seem to come up with negative values??????
!                ksat (i, j) = (0.00585 + 0.000166667 * rf_mean -
!     &                           pave (i, j)) * 60.0d0  !mm/min
!         ksat (i, j) = (1.45 - 0.014 * (pave (i, j) * 1.d4)) * ksat_mod
!
!LT_QUESTION why is the above ksat derivation different from that which is in ESPL in press paper 2, equation 1a?
!LT_QUESTION eq 1a in espl paper 2 does not appear to feature anywhere - why?
!use rainfall variable 1 - gave better results
!         psi (i, j) = 0.785 + 0.021 * (pave (i, j) * 1.d4)
!LT_QUESTION how data specific is the above equation for psi (the b storage parameter) since it was derived from experimental data from creosotebush
! John wouldn't trust this much b is difficult to get good. above is completely unverified. unreliable. can stick to constant value of 5 using clapham honberg type equation - in kinematic wave book in 1st chapter.
!LTOct2007 therefore could keep psi set to value read in, and thus include as above:
!                psi (i, j) = psiave (iveg)
!         drain_par (i, j) = drainage_parameter_mean (iveg)
!      endif
!JWMay05
!JWMay05  multiply through by calibration factor psi_mod
!JWMay05
!LT here theta is defined as theta_0. The 1 after theta_0 is because it reads in 4 values
!LTOct07 changed
!LTOct07        theta (i, j) = theta_0 (1)
!LTOct07          cum_inf (i, j) = ciinit (1)

!      psi (i, j) = psi (i, j) * psi_mod  !done in MAHLERAN_Storm_setting
!
!   friction-factor parameters
!
!      if (ff_type.eq.1) then
!         ff (i, j) = friction_factor_mean (iveg)
!      elseif (ff_type.eq.2) then
!         do while (ff (i, j).le.0.)
!            ff (i, j) = rand_from_norm (friction_factor_mean (iveg), friction_factor_std_dev (iveg), idum)
!         enddo
!      elseif (ff_type.eq.3) then
!
!   dynamic versions -- initialize
!
!         ff (i, j) = 14.d0
!      elseif (ff_type.eq.4) then
!
!  logf = -1.099 +0.024%G - 0.313 log Re + 0.915logDg
!
!         if (sed_propn (5, i, j).gt.0.0d0.or.sed_propn (6, i, j).gt.0.0d0) then
!            if (sed_propn (5, i, j).gt.sed_propn (6, i, j)) then
!          dg = 2.0d0 + 10.0d0 * (sed_propn (5, i, j) / (sed_propn (5, i, j) + sed_propn (6, i, j)))
!       else
!          dg = 12.0d0 + (20.0d0 * sed_propn (6, i, j))
!       endif
!    else
!       dg = 2.0d0
!    endif
!    if (pave (i, j).ge.0.0d0) then
!            pave_perc = pave (i, j) * 1.d4
!    else
!       pave_perc = 0.0d0
!    endif
!         ff (i, j) = 10.d0 ** (2.4d3 * pave_perc) * (dg ** 0.915)
!      elseif (ff_type.eq.5) then
!
!  ff = 9.143 x 10^-6 Re^-0.307 Dg^1.025 P%^3.470
!
!         if (sed_propn (5, i, j).gt.0.0d0.or.sed_propn (6, i, j).gt.0.0d0) then
!            if (sed_propn (5, i, j).gt.sed_propn (6, i, j)) then
!               dg = 2.0d0 + 10.0d0 * (sed_propn (5, i, j) / (sed_propn (5, i, j) + sed_propn (6, i, j)))
!       else
!          dg = 12.0d0 + (20.0d0 * sed_propn (6, i, j))
!       endif
!    else
!       dg = 2.0d0
!    endif
!    if (pave (i, j).ge.0.0d0) then
!           pave_perc = pave (i, j) * 1.d4
!    else
!       pave_perc = 0.0d0
!    endif
!         ff (i, j) = 9.143d-6 * (pave_perc ** 3.470) * (dg ** 1.025)
!    if (ff (i, j).lt.0.1d0) then
!       ff (i, j) = 0.1d0
!    endif
!      elseif (ff_type.eq.6) then
!
!  ff = Re^0.33, with default 16.17 for Re=0
!
!         ff (i, j) = 16.17
!      elseif (ff_type.eq.7) then
!
!  ff = 1.202 Dg^1.383 Q^-0.317
!
!         if (sed_propn (5, i, j).gt.0.0d0.or.sed_propn (6, i, j).gt.0.0d0) then
!            if (sed_propn (5, i, j).gt.sed_propn (6, i, j)) then
!               dg = 2.0d0 + 10.0d0 * (sed_propn (5, i, j) / (sed_propn (5, i, j) + sed_propn (6, i, j)))
!       else
!               dg = 12.0d0 + (20.0d0 * sed_propn (6, i, j))
!       endif
!    else
!       dg = 2.0d0
!    endif
!    ff (i, j) = 1.202d0 * dg ** 1.383d0
!    elseif (ff_type.eq.8) then
! can't do here because reading in all values in one go
!       endif
!    enddo
! enddo
else
!
!   just reset flux variables
!
   d (:, :, :) = 0.0d0
   q (:, :, :) = 0.0d0
   ammonium (:, :, :) = 0.0d0
   nitrate (:, :, :) = 0.0d0
   phosphorus (:, :, :) = 0.0d0
   qsum (:, :) = 0.0d0
   sedch (:, :) = 0.0d0
   sed_tot (:, :) = 0.0d0
   detach_tot (:, :) = 0.0d0
   depos_tot (:, :) = 0.0d0
   raindrop_detach_tot (:, :) = 0.0d0
   flow_detach_tot (:, :) = 0.0d0
!
!LTOct2007 reset sediment-bound nutrients too
!
   amm_tot (:, :) = 0.0d0
   nit_tot (:, :) = 0.0d0
   TN_tot (:, :) = 0.0d0
   TP_tot (:, :) = 0.0d0
   IC_tot (:, :) = 0.0d0
   TC_tot (:, :) = 0.0d0
endif

return
end
