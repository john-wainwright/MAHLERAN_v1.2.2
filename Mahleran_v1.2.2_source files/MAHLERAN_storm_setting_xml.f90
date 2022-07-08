!****************************************************************
!  subroutine to calculate storm dynamics (reads in parameters & files)
!  corresponds to Version 1.01.6 (first half of main programme)
!  modified from Eva's version to include Caspar's latest version
!  with topographic updates May 2014
!****************************************************************
subroutine MAHLERAN_storm_setting_xml

use shared_data
use parameters_from_xml

implicit none

character (len = 160) :: filename

integer :: ifinish
integer :: istart
integer :: init
integer :: ireset
integer :: nt
integer :: n_cols
integer :: n_rows
integer :: i
integer :: k
integer :: istate
integer :: distribution
integer :: ierr
integer *4 :: time_array (8)
! time_array(1)    year
! time_array(2)    month of the year
! time_array(3)    day of the month
! time_array(4)    time offset with respect to UTC in minutes
! time_array(5)    hour of the day
! time_array(6)    minutes of the hour
! time_array(7)    seconds of the minute
! time_array(8)    milliseconds of the second

double precision :: sum
double precision :: a
double precision :: b
double precision :: xllcorner
double precision :: yllcorner
double precision :: cellsize
double precision :: nodata_value
double precision :: psi_std_dev (10)
double precision :: drain_std_dev (10)
double precision :: ff_sd_0 (10)
double precision :: dg
double precision :: pave_perc
double precision, allocatable :: this_phi_init (:,:)

logical :: fexist

interface
    subroutine read_spatial_data (filename, data_array, n_cols, n_rows, xllcorner, yllcorner, cellsize, nodata_value)
        implicit none

        double precision, allocatable, intent(inout) :: data_array (:,:)
        double precision, intent (out) :: xllcorner, yllcorner, cellsize
        double precision, intent (out) :: nodata_value

        integer, intent (out) :: n_cols
        integer, intent (out) :: n_rows
        integer i, k

        logical fexist

        character (len=*) :: filename
        character *14 a_col, a_row, a_dx, a_minx, a_miny, a_nodata
        character *80 a_line
    end subroutine read_spatial_data
end interface

interface
    subroutine read_int_spatial_data (filename, int_data_array, n_cols, n_rows, xllcorner, yllcorner, cellsize, nodata_value)
        implicit none

        integer *4, allocatable, intent(inout) :: int_data_array (:,:)
        double precision, intent (out) :: xllcorner, yllcorner, cellsize
        double precision, intent (out) :: nodata_value

        integer, intent (out) :: n_cols
        integer, intent (out) :: n_rows
        integer i, k

        logical fexist

        character (len=*) :: filename
        character *14 a_col, a_row, a_dx, a_minx, a_miny, a_nodata
        character *80 a_line
    end subroutine read_int_spatial_data
end interface

interface
    subroutine calculate_surface_properties_from_types (data_array, cover_array, n_cols, n_rows, nodata_value, &
                                                    location, scale, distribution)
        implicit none

        double precision, allocatable, intent(inout) :: data_array (:,:)
        double precision, intent (inout) :: nodata_value
        double precision, intent (in) :: location (10)
        double precision, intent (in) :: scale (10)
        double precision :: ZBQLUAB
        double precision :: ZBQLEXP
        double precision :: ZBQLNOR
        double precision :: ZBQLGAM
        double precision :: ZBQLWEI

        integer, allocatable, intent(inout) :: cover_array (:,:)
        integer, intent (inout) :: n_cols
        integer, intent (inout) :: n_rows
        integer, intent (in) :: distribution
        integer :: i, k
        integer :: surface_type
    end subroutine calculate_surface_properties_from_types
end interface

ifinish = 0
istart = 1
init = 1
ireset = 2
nt = 0
sum = 0
a = 1
b = 1

!
!   initialize everything
!
!       call read_parameters
!
! CJMHJun13 initialize reorder logical variable to "false" - reorder to be
!      switched to "true" when updating topography has changed upslope
!      ordering.
       reorder = .false.
!
!CJMH  open files to dump output in at testing stage
!filename = output_folder (1:output_folder_length) // 'TestOutput1.dat'
!open (51, file = filename, iostat = istate, status = 'unknown')
!write (6, *) 'istate: ', istate, ', for file: ', filename
!CJMH  Output time & date to file
!call date_and_time (values = time_array)
!write (51, 903) time_array (5), time_array (6), time_array (7), time_array (3), time_array (2), time_array (1)

if (update_topography) then
!   write (51,*) 'Topography updated every ', nt_top_up,' time steps'
   write (6,*) 'nt_top_up =', nt_top_up
   if (nt_top_up.le.0 ) then
      stop
   endif
endif

!filename = output_folder (1:output_folder_length) // 'TestOutput2.dat'
!open (52, file = filename, iostat = istate, status = 'unknown')
!write (6, *) 'istate: ', istate, ', for file: ', filename
!CJMH  Output time & date to file
!call date_and_time (values = time_array)
!write (52, 903) time_array (5), time_array (6), time_array (7), time_array (3), time_array (2), time_array (1)
!
!call read_topog
filename = input_folder (1:input_folder_length) // dem_file
call read_spatial_data (filename, z, n_cols, n_rows, xllcorner, yllcorner, cellsize, nodata_value)
!
! save xllcorner and yllcorner from topography as master reference in xmin and ymin to use for spatial output
! and nodata_value for consistency elsewhere
!
xmin = xllcorner
ymin = yllcorner
nodata_value_from_topog = nodata_value
!
!  added JW Feb 2017 to allow output to screen to be turned on/off
!
if (verbose_output) then
   write (6, *) ' Topography data read in from file: ', filename
   write (6, *) n_rows, n_cols
   do i = 1, n_rows
      write (6, 9995) (z (i, k), k = 1, n_cols)
   enddo
endif
!
! elevations need to be converted to mm
!
do i = 1, n_rows
   do k = 1, n_cols
      if (z (i, k).gt.nodata_value_from_topog) then
         z (i, k) = z (i, k) * 1000.0d0  ! needs to be converted to mm
      endif
   enddo
enddo
!
! set shared_data values used for array manipulation elsewhere in the model
!
dx = cellsize
xmax = xmin + dx * n_cols
ymax = ymin + dx * n_rows
dx_m = dx
dx = dx * 1000.0d0 !convert to mm
dy = dx

nc2 = n_cols
nr2 = n_rows
!nc = n_cols - 2
!nr = n_rows - 2
nc = n_cols - 1
nr = n_rows - 1
nc1 = n_cols - 1
nr1 = n_rows - 1
ncell = nr1 * nc1
ff_type = friction_factor_type
inf_type = infiltration_parameter_type
inf_model = infiltration_model
rainfile = rainfall_data
rf_mean = mean_rainfall
rfvar1 = std_dev_rainfall
rfvar2 = skewness_rainfall
!
!   use information from topography header file to allocate space for model variables
!
write (6, *) ' about to allocate '
include 'allocat.var'
write (6, *) ' done allocate '
call initialize_allocatables
write (6, *) ' allocatables initialized '

!call read_veg_map
filename = input_folder (1:input_folder_length) // vegetation_cover_map
call read_spatial_data (filename, veg, n_cols, n_rows, xllcorner, yllcorner, cellsize, nodata_value)
!
!  added JW Feb 2017 to allow output to screen to be turned on/off
!
if (verbose_output) then
   write (6, *) ' Vegetation data read in from file: ', filename
   write (6, *) n_rows, n_cols
   do i = 1, n_rows
      write (6, 9995) (veg (i, k), k = 1, n_cols)
   enddo
endif

!call read_cover_map
filename = input_folder (1:input_folder_length) // surface_type_map
call read_int_spatial_data (filename, cover, n_cols, n_rows, xllcorner, yllcorner, cellsize, nodata_value)
!
!  check cover values are within range 1:10 to avoid issues later
!
!
!  added JW Feb 2017 to allow output to screen to be turned on/off
!
if (verbose_output) then
   write (6, *) ' Surface_type data read in from file: ', filename
   write (6, *) n_rows, n_cols
   do i = 1, n_rows
      write (6, 9994) (cover (i, k), k = 1, n_cols)
   enddo
endif
do i = 1, n_rows
    do k = 1, n_cols
       if (cover (i, k).ne.int(nodata_value)) then
          if (cover (i, k).lt.1) then
             cover (i, k) = 1
          elseif (cover (i, k).gt.n_types) then
             write (6, *) ' WARNING -- surface-type file contains value of ', cover (i, k), ' but only ', n_types, &
                           ' values provided in input file'
             write (6, *) '         -- value has been reset to: ', n_types
             cover (i, k) = n_types
          endif
       else
          cover (i, k) = 1 !set no_data values to 1 to avoid array bounds issues later
       endif
    enddo
enddo
write (6, *) ' Surface-type data read in from file: ', filename
write (6, *) n_rows, n_cols

!call read_rain_mask
filename = input_folder (1:input_folder_length) // rainfall_scaling_map
call read_spatial_data (filename, rmask, n_cols, n_rows, xllcorner, yllcorner, cellsize, nodata_value)
write (6, *) ' Rainfall-scaling map data read in from file: ', filename

filename = input_folder (1:input_folder_length) // pavement_map
fexist = .FALSE.
!
!     check if filename passed to the subroutine exists and throw and error if not
!
inquire (file = filename, exist = fexist, err = 1)

1   continue
if (len_trim (pavement_map).gt.5.and.fexist) then
    call read_spatial_data (filename, pave, n_cols, n_rows, xllcorner, yllcorner, cellsize, nodata_value)
    write (6, *) ' Pavement map data read in from file: ', filename
else
    do i = 1, n_rows - 1
        do k = 1, n_cols - 1
           pave (i, k) = 0.0d0
        enddo
    enddo
    write (6, *) ' No pavement file found -- values set to zero'
endif

if (use_map_phi) then
   allocate (this_phi_init (nr2, nc2))
   write (6, *) ' Initializing particle size from map data'
   do phi = 1, 6
      filename = input_folder (1:input_folder_length) // particle_size_map (phi)
      call read_spatial_data (filename, this_phi_init, n_cols, n_rows, xllcorner, yllcorner, cellsize, nodata_value)
      write (6, *) ' Read particle-size map data (phi=', phi, ') read in from file: ', filename
      do i = 1, n_rows - 1
         do k = 1, n_cols - 1
            ps_init_ave (phi, i, k) = this_phi_init (i, k)
         enddo
      enddo
   enddo
else
   allocate (this_phi_init (nr2, nc2))
   write (6, *) ' Initializing particle size from surface types'
   select case (particle_size_distribution)
      case ('deterministic')
           distribution = 0
      case ('uniform')
           distribution = 1
      case ('exponential')
           distribution = 2
      case ('normal')
           distribution = 3
      case ('lognormal')
           distribution = 4
      case ('gamma')
           distribution = 5
      case ('Weibull')
           distribution = 6
      case default
           distribution = 0
           write (6, *) 'WARNING: unknown distribution function in particle_size_distribution: ', &
              particle_size_distribution, ', resetting to deterministic'
   end select
   do phi = 1, 6
!      filename = input_folder (1:input_folder_length) // particle_size_map (phi)
      call calculate_surface_properties_from_types (this_phi_init, cover, n_cols, n_rows, nodata_value, &
                                      mean_particle_size (:, phi), std_dev_particle_size (:, phi), distribution)
      write (6, *) ' Initialized particle-size map data (phi=', phi, ') from surface types'
      do i = 1, n_rows - 1
         do k = 1, n_cols - 1
            ps_init_ave (phi, i, k) = this_phi_init (i, k)
         enddo
      enddo
   enddo

   write (6, *) ' Particle-size data parameterized using surface cover'
endif
!
!   account for correction factor applied in read routine (rel to infiltration calc)
!
do phi = 1, 6
   do i = 1, n_rows - 1
      do k = 1, n_cols - 1
         grav (i, k) = (ps_init_ave (5, i, k) + ps_init_ave (6, i, k))
         fines (i, k) = 1.0d0 - grav (i, k)
         pave_perc = pave (i, k) / 1.d2  ! convert from % to proportion
         if (pave_perc.le.0.0d0.or.grav (i, k).eq.0.0d0) then
        sed_propn (phi, i, k) = ps_init_ave (phi, i, k)
         else
            if (phi.le.4) then
               sed_propn (phi, i, k) = ps_init_ave (phi, i, k) *  ((1.0d0 - pave_perc) / fines (i, k))
        else
               sed_propn (phi, i, k) = ps_init_ave (phi, i, k) * (pave_perc / grav (i, k))
        endif
         endif
      enddo
   enddo
enddo


!
!   following conversion avoids repeat calculation in infiltration routine
!
do i = 1, n_rows - 1
   do k = 1, n_cols - 1
      pave (i, k) = 0.0001 * pave (i, k)
   enddo
enddo

!
!  for debugging of pavement component identified by Andy Cunliffe
!
!
!  added JW Feb 2017 to allow output to screen to be turned on/off
!
if (verbose_output) then
   write (6, *)
   write (6, *) ' values of grav(i,k)'
   do i = 1, n_rows
      write (6, 9995) (grav (i, k), k = 1, n_cols)
   enddo
   write (6,*)
   write (6, *)
   write (6, *) ' values of pave(i,k)'
   do i = 1, n_rows
      write (6, 9995) (pave (i, k), k = 1, n_cols)
   enddo
   write (6,*)
   write (6, *) ' values of sed_propn (5, i, k)'
   do i = 1, n_rows
      write (6, 9995) (sed_propn (5, i, k), k = 1, n_cols)
   enddo
   write (6,*)
   write (6, *) ' values of sed_propn (6, i, k)'
   do i = 1, n_rows
      write (6, 9995) (sed_propn (6, i, k), k = 1, n_cols)
   enddo
   write (6,*)
endif

inf_type = infiltration_parameter_type
write (6, *) 'inf_type = ', inf_type
if (use_final_infiltration_map.or.inf_type.eq.4) then
   filename = input_folder (1:input_folder_length) // final_infiltration_map
   call read_spatial_data (filename, ksat, n_cols, n_rows, xllcorner, yllcorner, cellsize, nodata_value)
   write (6, *) ' Final infiltration map data read in from file: ', filename
else
   write (6, *) 'inf_type is now = ', inf_type
   if (inf_type.eq.1) then
!
! use pavement- and rainfall-based parameterization using ave rf intensity
!
      do i = 1, n_rows - 1
         do k = 1, n_cols - 1
            ksat (i, k) = 0.00585 + 0.000166667 * rf_mean - pave (i, k)
         enddo
      enddo
   elseif (inf_type.eq.2) then
      select case (final_infiltration_rate_distribution)
        case ('deterministic')
           distribution = 0
        case ('uniform')
           distribution = 1
        case ('exponential')
           distribution = 2
        case ('normal')
           distribution = 3
        case ('lognormal')
           distribution = 4
        case ('gamma')
           distribution = 5
        case ('Weibull')
           distribution = 6
        case default
           distribution = 0
           write (6, *) 'WARNING: unknown distribution function in final_infiltration_rate_distribution: ', &
              final_infiltration_rate_distribution, ', resetting to deterministic'
      end select
      call calculate_surface_properties_from_types (ksat, cover, n_cols, n_rows, nodata_value, &
                                                 final_infiltration_rate_mean, final_infiltration_rate_std_dev, distribution)
      write (6, *) ' Final infiltration data parameterized using surface cover'
               elseif (inf_type.eq.3) then
! inf_type = 3 is dynamic through time so not allocated here
! inf_type = 4 is the map, already dealt with in the first part of the loop
   elseif (inf_type.eq.5) then
      do i = 1, n_rows - 1
         do k = 1, n_cols - 1
!
!JWFeb2006
!JWFeb2006   use simplified Green and Ampt
!JWFeb2006
            ksat (i, k) = (1.45 - 0.014 * (pave (i, k) * 1.d4))
         enddo
      enddo
   endif
endif
write (6, *) 'Now here'
if (use_suction_map) then
    filename = input_folder (1:input_folder_length) // suction_map
    call read_spatial_data (filename, psi, n_cols, n_rows, xllcorner, yllcorner, cellsize, nodata_value)
    write (6, *) ' Suction map data read in from file: ', filename
else
   if (inf_type.eq.1.or.inf_type.eq.3.or.inf_type.eq.4) then
!
! use pavement- and rainfall-based parameterization using ave rf intensity
!
      do i = 1, 10
          psi_std_dev = 0.0d0
      enddo
      distribution = 0
      call calculate_surface_properties_from_types (psi, cover, n_cols, n_rows, nodata_value, &
                                                 wetting_front_suction_mean, psi_std_dev, distribution)
   elseif (inf_type.eq.2) then
      select case (wetting_front_suction_distribution)
          case ('deterministic')
             distribution = 0
          case ('uniform')
             distribution = 1
          case ('exponential')
             distribution = 2
          case ('normal')
             distribution = 3
          case ('lognormal')
             distribution = 4
          case ('gamma')
             distribution = 5
          case ('Weibull')
             distribution = 6
          case default
             distribution = 0
             write (6, *) 'WARNING: unknown distribution function in wetting_front_suction_distribution: ', &
                wetting_front_suction_distribution, ', resetting to deterministic'
      end select
      call calculate_surface_properties_from_types (psi, cover, n_cols, n_rows, nodata_value, &
                                                 wetting_front_suction_mean, wetting_front_suction_std_dev, distribution)
      write (6, *) ' Suction data parameterized using surface cover'
   elseif (inf_type.eq.5) then
      do i = 1, n_rows - 1
         do k = 1, n_cols - 1
            psi (i, k) = 0.785 + 0.021 * (pave (i, k) * 1.d4)
         enddo
      enddo
   endif
endif
if (use_drainage_map) then
    filename = input_folder (1:input_folder_length) // drainage_map_map
    call read_spatial_data (filename, drain_par, n_cols, n_rows, xllcorner, yllcorner, cellsize, nodata_value)
    write (6, *) ' Drainage map data read in from file: ', filename
else
   if (inf_type.ne.2) then
      do i = 1, 10
          drain_std_dev = 0.0d0
      enddo
      call calculate_surface_properties_from_types (psi, cover, n_cols, n_rows, nodata_value, &
                                                 drainage_parameter_mean, drain_std_dev, distribution)
   else
      select case (drainage_parameter_distribution)
          case ('deterministic')
             distribution = 0
          case ('uniform')
             distribution = 1
          case ('exponential')
             distribution = 2
          case ('normal')
             distribution = 3
          case ('lognormal')
             distribution = 4
          case ('gamma')
             distribution = 5
          case ('Weibull')
             distribution = 6
          case default
             distribution = 0
             write (6, *) 'WARNING: unknown distribution function in drainage_parameter_distribution: ', &
                drainage_parameter_distribution, ', resetting to deterministic'
      end select
      call calculate_surface_properties_from_types (drain_par, cover, n_cols, n_rows, nodata_value, &
                                                 drainage_parameter_mean, drainage_parameter_std_dev, distribution)
      write (6, *) ' Drainage data parameterized using surface cover'
   endif
endif
if (use_friction_factor_map.or.friction_factor_type.eq.8) then
   filename = input_folder (1:input_folder_length) // friction_factor_map
   call read_spatial_data (filename, ff, n_cols, n_rows, xllcorner, yllcorner, cellsize, nodata_value)
   write (6, *) ' Friction factor map data read in from file: ', filename
else
   if (ff_type.eq.1) then
      do i = 1, 10
         ff_sd_0 (i) = 0.0d0
      enddo
      call calculate_surface_properties_from_types (ff, cover, n_cols, n_rows, nodata_value, &
                                                 friction_factor_mean, ff_sd_0, 0)
   elseif (ff_type.eq.2) then
      select case (friction_factor_distribution)
         case ('deterministic')
            distribution = 0
         case ('uniform')
            distribution = 1
         case ('exponential')
            distribution = 2
         case ('normal')
            distribution = 3
         case ('lognormal')
            distribution = 4
         case ('gamma')
            distribution = 5
         case ('Weibull')
            distribution = 6
         case default
            distribution = 0
            write (6, *) 'WARNING: unknown distribution function in friction_factor_distribution: ', &
               friction_factor_distribution, ', resetting to deterministic'
      end select
      call calculate_surface_properties_from_types (ff, cover, n_cols, n_rows, nodata_value, &
                                                   friction_factor_mean, friction_factor_std_dev, distribution)
      write (6, *) ' Friction factor data parameterized using surface cover'
   elseif (ff_type.eq.3) then
!
!   dynamic versions -- initialize
!
      do i = 1, n_rows - 1
         do k = 1, n_cols - 1
            ff (i, k) = 14.d0
         enddo
      enddo
   elseif (ff_type.eq.4) then
!
!  logf = -1.099 +0.024%G - 0.313 log Re + 0.915logDg
!
      do i = 1, n_rows - 1
         do k = 1, n_cols - 1
            if (sed_propn (5, i, k).gt.0.0d0.or.sed_propn (6, i, k).gt.0.0d0) then
               if (sed_propn (5, i, k).gt.sed_propn (6, i, k)) then
              dg = 2.0d0 + 10.0d0 * (sed_propn (5, i, k) / (sed_propn (5, i, k) + sed_propn (6, i, k)))
           else
                  dg = 12.0d0 + (20.0d0 * sed_propn (6, i, k))
               endif
            else
               dg = 2.0d0
            endif
            if (pave (i, k).ge.0.0d0) then
               pave_perc = pave (i, k) * 1.d4
            else
               pave_perc = 0.0d0
            endif
            ff (i, k) = 10.d0 ** (2.4d3 * pave_perc) * (dg ** 0.915)
         enddo
      enddo
   elseif (ff_type.eq.5) then
!
!  ff = 9.143 x 10^-6 Re^-0.307 Dg^1.025 P%^3.470
!
      do i = 1, n_rows - 1
         do k = 1, n_cols - 1
            if (sed_propn (5, i, k).gt.0.0d0.or.sed_propn (6, i, k).gt.0.0d0) then
               if (sed_propn (5, i, k).gt.sed_propn (6, i, k)) then
                  dg = 2.0d0 + 10.0d0 * (sed_propn (5, i, k) / (sed_propn (5, i, k) + sed_propn (6, i, k)))
               else
              dg = 12.0d0 + (20.0d0 * sed_propn (6, i, k))
           endif
            else
           dg = 2.0d0
            endif
            if (pave (i, k).ge.0.0d0) then
               pave_perc = pave (i, k) * 1.d4
            else
           pave_perc = 0.0d0
            endif
            ff (i, k) = 9.143d-6 * (pave_perc ** 3.470) * (dg ** 1.025)
            if (ff (i, k).lt.0.1d0) then
               ff (i, k) = 0.1d0
            endif
         enddo
      enddo
   elseif (ff_type.eq.6) then
!
!  ff = Re^0.33, with default 16.17 for Re=0
!
      do i = 1, n_rows - 1
         do k = 1, n_cols - 1
            ff (i, k) = 16.17
         enddo
      enddo
   elseif (ff_type.eq.7) then
!
!  ff = 1.202 Dg^1.383 Q^-0.317
!
      do i = 1, n_rows - 1
         do k = 1, n_cols - 1
            if (sed_propn (5, i, k).gt.0.0d0.or.sed_propn (6, i, k).gt.0.0d0) then
               if (sed_propn (5, i, k).gt.sed_propn (6, i, k)) then
                  dg = 2.0d0 + 10.0d0 * (sed_propn (5, i, k) / (sed_propn (5, i, k) + sed_propn (6, i, k)))
           else
                  dg = 12.0d0 + (20.0d0 * sed_propn (6, i, k))
               endif
            else
           dg = 2.0d0
            endif
            ff (i, k) = 1.202d0 * dg ** 1.383d0
         enddo
      enddo
!    elseif (ff_type.eq.8) then
! already covered above
   endif
endif
!
!  added JW Feb 2017 to allow output to screen to be turned on/off
!
if (verbose_output) then
   write (6, *) ' fftype = ', ff_type, ' values:'
   do i = 1, n_rows
      write (6, *)(ff (i, k), k = 1, n_cols)
   enddo
   write (6, *)
endif

if (use_initial_soil_moisture_map) then
    filename = input_folder (1:input_folder_length) // initial_soil_moisture_map
    call read_spatial_data (filename, theta_0, n_cols, n_rows, xllcorner, yllcorner, cellsize, nodata_value)
    write (6, *) ' Initial soil-moisture map data read in from file: ', filename
else
   select case (initial_soil_moisture_distribution)
       case ('deterministic')
          distribution = 0
       case ('uniform')
          distribution = 1
       case ('exponential')
          distribution = 2
       case ('normal')
          distribution = 3
       case ('lognormal')
          distribution = 4
       case ('gamma')
          distribution = 5
       case ('Weibull')
          distribution = 6
       case default
          distribution = 0
          write (6, *) 'WARNING: unknown distribution function in initial_soil_moisture_distribution: ', &
             initial_soil_moisture_distribution, ', resetting to deterministic'
   end select

   call calculate_surface_properties_from_types (theta_0, cover, n_cols, n_rows, nodata_value, &
                                                 initial_soil_moisture_mean, initial_soil_moisture_std_dev, distribution)
   write (6, *) ' Initial soil-moisture data parameterized using surface cover'
endif
if (use_saturated_soil_moisture_map) then
    filename = input_folder (1:input_folder_length) // saturated_soil_moisture_map
    call read_spatial_data (filename, theta_sat, n_cols, n_rows, xllcorner, yllcorner, cellsize, nodata_value)
    write (6, *) ' Saturated soil-moisture map data read in from file: ', filename
else
   select case (saturated_soil_moisture_distribution)
       case ('deterministic')
          distribution = 0
       case ('uniform')
          distribution = 1
       case ('exponential')
          distribution = 2
       case ('normal')
          distribution = 3
       case ('lognormal')
          distribution = 4
       case ('gamma')
          distribution = 5
       case ('Weibull')
          distribution = 6
       case default
          distribution = 0
          write (6, *) 'WARNING: unknown distribution function in saturated_soil_moisture_distribution: ', &
             saturated_soil_moisture_distribution, ', resetting to deterministic'
   end select

   call calculate_surface_properties_from_types (theta_sat, cover, n_cols, n_rows, nodata_value, &
                                                 saturated_soil_moisture_mean, saturated_soil_moisture_std_dev, distribution)
   write (6, *) ' Saturated soil-moisture data parameterized using surface cover'
endif


!call read_sm_map
!call read_theta_sat_map
!call read_PHI_maps
!call read_pavement_map
!  inf_type = 1 uses pavement map with mean rainfall intensity
!  inf_type = 2 uses values from mahleran_input.dat file
!  inf_type = 3 uses pavement map with dynamic rainfall intensity
!  inf_type = 4 uses ksat file, only read in ksat map for inf_type = 4
!if (inf_type.eq.4) then
!    call read_ksat_map
!endif

!
!   find next output file and initialize all output files
!

call find_output_file_xml
write (6, *) ' Just left find_output_file_xml, iout = ', iout
!
!   to output soil moisture and drainage at end of run
!
!call output_maps_xml

!
!   calculate contributing area and flow order
!
!       call topog_attrib
! July 2013 from Caspar Hewett for compatibility with 1.01.6
! call topog_attrib
!
!   read in calibration files, if required
!
call calibration_xml
!
!  final infiltration can change dynamically so ksat_mod is used in infilt.for directly
!
do i = 1, n_rows - 1
   do k = 1, n_cols - 1
      psi (i, k) = psi (i, k) * psi_mod
   enddo
enddo

call initialize_values_xml (init)

!JW Nov15 moved here as only knows value of ndirn from initialize_values_xml
call topog_attrib
write (6, *) ' Returned from topog_attrib, ndirn = ', ndirn

write (6, *) 4, ' ff (5, 5): ', ff (5, 5)

9995   format (1000 (f9.2, 1x))
9994   format (1000 (i5, 1x))
 900   format ('Time: ', e20.14, ' seconds')
 901   format ('End time:', f10.4, ' seconds')
 902   format (10000 (e17.10, 1x))
 903   format (' Output time ', i2, ':', i2, ':', i2, ' on ', i2, '/', i2, '/', i4)

end subroutine
