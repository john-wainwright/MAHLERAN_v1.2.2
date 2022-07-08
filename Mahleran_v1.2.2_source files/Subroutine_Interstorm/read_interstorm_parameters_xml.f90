!****************************************************************
!  subroutine to read parameters for interstorm calculations
!****************************************************************
subroutine read_interstorm_parameters_xml

use interstorm_shared_data
use shared_data
use time_h
use parameters_from_xml

implicit none

integer :: i, ierr

logical :: fexist

character (len = 120) :: filename, file


!****************************************************************
! variables used in this subroutine listed in order of their occurrence
! local
!****************************************************************
! filename is variable for name of interstorm input file [-]
! file is variable for filename in data input inquiry [-]
! fexist is variable for data input inquiry [-]
! i is a running variable for checking size of filename in name loop [-]
!****************************************************************
! global
!****************************************************************
! pfadin_interstorm declared in interstorm_shared_data [-]
! pfadout_interstorm declared in interstorm_shared_data [-]
! tstart declared in general_h [-]
! tstop declared in general_h [-]
! mstart declared in general_h [-]
! mstop declared in general_h [-]
! no_years declared in interstorm_shared_data [-]
! cont_temp_file declared in interstorm_shared_data [-]
! cont_daily_rain_file declared in interstorm_shared_data [-]
! cont_minute_rain_file declared in interstorm_shared_data [-]
! shrub_cov_file declared in interstorm_shared_data [-]
! threshold_rain declared in interstorm_shared_data [mm]
! latitude declared in interstorm_shared_data [rad]
! depth_1 declared in interstorm_shared_data [mm]
! depth_2 declared in interstorm_shared_data [mm]
! sm_1_init declared in interstorm_shared_data [mm]
! sm_2_init declared in interstorm_shared_data [mm]
! t declared in general_h [-]
! total_days declared in interstorm_shared_data [-]
! dtot declared in general_h [-]
! Tmean(:) declared in interstorm_shared_data (day) [�C]
! Tmax(:) declared in interstorm_shared_data (day) [�C]
! Tmin(:) declared in interstorm_shared_data (day) [�C]
! rain_daily(:) declared in interstorm_shared_data (day) [mm]
! shrub_cover(:,:) declared in interstorm_shared_data (row,column) [%]
! nr2 declared in shared_data [-]
! nc2 declared in shared_data [-]
! grass_cover(:,:) declared in interstorm_shared_data (day) [m2/m2]
! et_pot(:,:,:) declared in interstorm_shared_data (time step,row,column) [mm]
! et_1(:,:,:) declared in interstorm_shared_data (time step,row,column) [mm]
! et_2(:,:,:) declared in interstorm_shared_data (time step,row,column) [mm]
! inf_2(:,:,:) declared in interstorm_shared_data (time step,row,column) [mm]
! inf_1(:,:,:) declared in interstorm_shared_data (time step,row,column) [mm]
! drain_1(:,:,:) declared in interstorm_shared_data (time step,row,column) [mm]
! drain_2(:,:,:) declared in interstorm_shared_data (time step,row,column) [mm]
! sm_1to2(:,:,:) declared in interstorm_shared_data (time step,row,column) [mm]
! sm_sf(:,:,:) declared in interstorm_shared_data (time step,row,column) [mm]
! sm_1(:,:,:) declared in interstorm_shared_data (time step,row,column) [mm]
! sm_2(:,:,:) declared in interstorm_shared_data (time step,row,column) [mm]
! theta_begin(:,:) declared in interstorm_shared_data (row,column) [m3/m3]
! field_cap(:,:) declared in interstorm_shared_data (row,column) [m3/m3]
! pfadi_interstorm declared in interstorm_shared_data [-]
! pfado_interstorm declared in interstorm_shared_data [-]
!****************************************************************

!
! get parameters already read in by  read_parameters_from_xml_file
!
tstart = start_year                                 ! read in start year of simulation
tstop = end_year                                    ! read in end year of simulation
mstart = start_month                                ! read in start months in the first year
mstop = end_month                               ! read in stop months in the last year
no_years = tstop - tstart + 1                       ! number of simulated years
cont_temp_file = continuous_temperature_data        ! read in name of file with cont. rainfall data
cont_daily_rain_file = continuous_rainfall_data     ! read in name of file with cont. temperature data
cont_minute_rain_file = one_min_rainfall_data       ! read in name of file with cont. temperature data
shrub_cov_file = shrub_cover_map                    ! read in name of file with shrub cover mask data
threshold_rain = threshold_rain_xml                 ! read in minimal daily rainfall amount for which Mahleran storm is calculated [mm]
latitude = latitude_xml                             ! read in value for latitude as used for evapotranspiration calculation in rad
lat_deg = latitude_degrees                          ! read in value for latitude as used for evapotranspiration calculation in degrees
depth (1) = depth_of_upper_layer                    ! read in value for depth of layer 1 in mm
depth (2) = depth_of_lower_layer                    ! read in value for depth of layer 2 in mm
rw = residual_water_content             ! read in value for residual water content in mm**3/mm**3
wsc = water_content_for_stomatal_closure        ! read in value for water content of beginning stomatal closure [m**3/m**3]
et_red_fac = crusting_evapotranspiration_reduction  ! read in value for constatnt for evapotranspiration reduction due to crusting, desert pavement [-]
inf_rate_bare_2 = infiltration_from_bare_to_layer_2 ! read in value for infiltration rate from bare surface to layer 2 [mm]
inf_rate_2 = infiltration_from_shrub_to_layer_2     ! read in value for infiltration rate from shrub covered surface to layer 2 [mm]
k_s = ksat_xml                          ! read in value for saturated hydraulic conductivity [mm/h]
d_const = diffusion_coefficient             ! read in value for diffusion coefficient [-]
sm_1_init = initial_value_for_sm_1          ! read in initial value for sm_1 [mm]
sm_2_init = initial_value_for_sm_2                  ! read in initial value for sm_2 [mm]

!check total number of days (total_days)
t = tstart          !set the beginning of the calculation
total_days = 0
do t = tstart, tstop !
   call calcyear
enddo
total_days = dtot

allocate (Tmean (total_days), source = 0.0e0, stat = ierr)
if (ierr /= 0 ) stop "Memory error allocating Tmean in read_interstorm_parameters_xml!"
allocate (Tmax (total_days), source = 0.0e0, stat = ierr)
if (ierr /= 0 ) stop "Memory error allocating Tmax in read_interstorm_parameters_xml!"
allocate (Tmin (total_days), source = 0.0e0, stat = ierr)
if (ierr /= 0 ) stop "Memory error allocating Tmin in read_interstorm_parameters_xml!"
allocate (rain_daily (total_days), source = 0.0e0, stat = ierr)
if (ierr /= 0 ) stop "Memory error allocating rain_daily in read_interstorm_parameters_xml!"
allocate (shrub_cover (nr2, nc2), source = 0.0d0, stat = ierr)
if (ierr /= 0 ) stop "Memory error allocating shrub_cover in read_interstorm_parameters_xml!"
allocate (grass_cover (nr2, nc2), source = 0.0d0, stat = ierr)
if (ierr /= 0 ) stop "Memory error allocating grass_cover in read_interstorm_parameters_xml!"
allocate (et_pot (nr2, nc2), source = 0.0e0, stat = ierr)
if (ierr /= 0 ) stop "Memory error allocating et_pot in read_interstorm_parameters_xml!"
allocate (et_1 (nr2, nc2), source = 0.0e0, stat = ierr)
if (ierr /= 0 ) stop "Memory error allocating et_1 in read_interstorm_parameters_xml!"
allocate (et_2 (nr2, nc2), source = 0.0e0, stat = ierr)
if (ierr /= 0 ) stop "Memory error allocating et_2 in read_interstorm_parameters_xml!"
allocate (inf_2 (nr2, nc2), source = 0.0e0, stat = ierr)
if (ierr /= 0 ) stop "Memory error allocating inf_2 in read_interstorm_parameters_xml!"
allocate (inf_1 (nr2, nc2), source = 0.0e0, stat = ierr)
if (ierr /= 0 ) stop "Memory error allocating inf_1 in read_interstorm_parameters_xml!"
allocate (drain_1 (nr2, nc2), source = 0.0e0, stat = ierr)
if (ierr /= 0 ) stop "Memory error allocating drain_1 in read_interstorm_parameters_xml!"
allocate (drain_2 (nr2, nc2), source = 0.0e0, stat = ierr)
if (ierr /= 0 ) stop "Memory error allocating drain_2 in read_interstorm_parameters_xml!"
allocate (sm_1to2 (nr2, nc2), source = 0.0e0, stat = ierr)
if (ierr /= 0 ) stop "Memory error allocating sm_1to2 in read_interstorm_parameters_xml!"
allocate (sm_sf (nr2, nc2), source = 0.0e0, stat = ierr)
if (ierr /= 0 ) stop "Memory error allocating sm_sf in read_interstorm_parameters_xml!"
allocate (sm (2, total_days, nr2, nc2), source = 0.0e0, stat = ierr)
if (ierr /= 0 ) stop "Memory error allocating sm in read_interstorm_parameters_xml!"
allocate (theta_begin (nr2, nc2), source = 0.0e0, stat = ierr)
if (ierr /= 0 ) stop "Memory error allocating theta_begin in read_interstorm_parameters_xml!"
allocate (field_cap (nr2, nc2), source = 0.0e0, stat = ierr)
if (ierr /= 0 ) stop "Memory error allocating field_cap in read_interstorm_parameters_xml!"
allocate (qsum_all (total_days, nr2, nc2), source = 0.0e0, stat = ierr)
if (ierr /= 0 ) stop "Memory error allocating qsum_all in read_interstorm_parameters_xml!"
allocate (sedtotal_all (total_days, nr2, nc2), source = 0.0e0, stat = ierr)
if (ierr /= 0 ) stop "Memory error allocating sedtotal_all in read_interstorm_parameters_xml!"
allocate (theta_areal_average (total_days, 2), source = 0.0e0, stat = ierr)
if (ierr /= 0 ) stop "Memory error allocating theta_areal_average in read_interstorm_parameters_xml!"

end
