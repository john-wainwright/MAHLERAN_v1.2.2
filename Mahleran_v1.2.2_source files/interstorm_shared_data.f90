module interstorm_shared_data
save

! parameters defined in interstorm.dat

character *80 pfadin_interstorm ! ./Input/Interstorm/; path for interstorm input data; assigned in interstorm.dat via read_interstorm_parameters [-]
character *80 pfadout_interstorm ! ./Output/; path for interstorm output data; assigned in interstorm.dat via read_interstorm_parameters [-]

integer no_years !number of simulated years; assigned in read_interstorm_parameters [-]
integer current_year !run variable for current year calculation
integer rday ! currently calculated running day; assigned in mahleran_interstorm [-]
integer Julian ! currently calculated Julian day; assigned in mahleran_interstorm [-]
integer total_days ! total days to calculate; assigned in read_interstorm_parameters [-]
integer pfadi_interstorm ! used to determine length of path name for output files; assigned in read_interstorm_parameters [-]
integer pfado_interstorm ! used to determine length of basic model path name; assigned in read_interstorm_parameters [-]
!JW short length restriction removed May 2014
character (len = 120) :: cont_temp_file ! Sev_temp.dat; name of continuous temperature file, must be exactly 8 characters!; assigned in interstorm.dat via read_interstorm_parameters [-]
character (len = 120) :: cont_daily_rain_file ! Sev_rain.dat; name of continuous rainfall file, must be exactly 8 characters!; assigned in interstorm.dat via read_interstorm_parameters [-]
character (len = 120) :: cont_minute_rain_file ! Sev_rain.dat; name of continuous rainfall file, must be exactly 8 characters!; assigned in interstorm.dat via read_interstorm_parameters [-]
character (len = 120) :: shrub_cov_file ! shrub_cv.asc; name of file for percentage shrub cover, must be exactly 8 characters!; assigned in interstorm.dat via read_interstorm_parameters [-]
real latitude ! latitude of entire site in radians;  assigned in interstorm.dat via read_interstorm_parameters [rad]
real lat_deg ! latitude of the site in degrees;  assigned in interstorm.dat via read_interstorm_parameters [�]
!real depth_1 ! value for depth of upper layer/layer 1 in mm; assigned in interstorm.dat via read_interstorm_parameters [mm]
!real depth_2 ! value for depth of deeper layer/layer 2 in mm; assigned in interstorm.dat via read_interstorm_parameters [mm]
!previous two lines replaced in ECOGEOM version with:
real depth (2) ! value for depth of upper layer/layer 1 and 2 in mm; assigned in interstorm.dat via read_interstorm_parameters [mm]
real threshold_rain ! amount of rainfall that triggers mahleran_storm; assigned in mahleran_interstorm [mm]
real rw ! residual water content value, soil type specific [m**3/m**3]
real wsc ! water content of beginning stomatal closure [m**3/m**3]
real et_red_fac ! constant for evapotranspiration reduction due to crusting, desert pavement [-]
real inf_rate_bare_2 ! infiltration rate from bare surface to layer 2 [mm]
real inf_rate_2 ! infiltration rate from shrub covered surface to layer 2 [mm]
real k_s ! constant for saturated hydraulic conductivity [mm/h]
real d_const ! diffusion coefficient [-]
real sm_1_init ! initial value for sm_1 [mm]
real sm_2_init ! initial value for sm_2 [mm]
! time series for interstorm calculation

real, allocatable :: Tmean (:) ! value for mean daily temperature in �C; assigned in read_temperature (day) [�C]
real, allocatable :: Tmax (:) ! value for maximum daily temperature in �C; assigned in read_temperature (day) [�C]
real, allocatable :: Tmin (:) ! value for minimum daily temperature in �C; assigned in read_temperature (day) [�C]

real, allocatable :: rain_daily (:) ! value for daily rainfall in mm; assigned in read_rainfall_minute (day) [mm]
integer, allocatable :: rain_minute_t (:,:)! array for rainfall time series with resolution of one minute to hold Year, Julian Day, hourminute
real, allocatable :: rain_minute (:) !array to hold rainfall/minute
real, allocatable :: rain_cont (:, :)   !array with continuous rainfall data: rday,hour, minute, intensity in mm/min
integer :: file_length_rain !length of the raindata which will be used for continuous simulation (above a certain threshold :minimal_storm)
integer :: rain_actual ! [-]
real:: theta_min !(m3/m3)
!next two are d.p. to allow use of read_spatial_data subroutine
double precision, allocatable :: shrub_cover (:, :) ! value for shrub cover for row,column; has to be between 0 and 100; assigned in read_shrub_cov (row,column) [%]
double precision, allocatable :: grass_cover (:, :) ! value for grass cover for row,column; has to be between 0 and 1; assigned in interstorm_initialise (row,column) [m2/m2]

real, allocatable :: et_pot (:, :)   ! value for  potential evaporation per time step for row,column in mm; assigned in calc_et (time step,row,column) [mm]
real, allocatable :: et_1 (:, :)     ! value for evaporation from upper layer/layer 1 per time step for row,column in mm; assigned in calc_et (time step,row,column) [mm]
real, allocatable :: et_2 (:, :)     ! value for evaporation from deeper layer/layer 2 per time step for row,column in mm; assigned in calc_et (time step,row,column) [mm]

real, allocatable :: inf_2 (:, :)    ! value for infiltration into layer 2 per time step for row,column in mm; assigned in calc_inf (time step,row,column) [mm]
real, allocatable :: inf_1 (:, :)    ! value for infiltration into layer 1 per time step for row,column in mm; assigned in calc_inf (time step,row,column) [mm]

real, allocatable :: drain_1 (:, :)  ! value for drainage from layer 1 per time step for row,column in mm; assigned in calc_inf (time step,row,column) [mm]
real, allocatable :: drain_2 (:, :)  ! value for drainage from layer 2 per time step for row,column in mm; assigned in calc_inf (time step,row,column) [mm]
real, allocatable :: sm_1to2 (:, :)  ! value for diffusive soil water movement from layer 1 to layer 2 per time step for row,column in mm; assigned in calc_inf (time step,row,column) [mm]

real, allocatable :: field_cap (:, :)   !field capacity in m3/m3 [m3/m3]
real, allocatable :: theta_begin (:, :) ! value for initial soil moisture for row,column in m3/m3;  assigned in interstorm_initialise (row,column) [m3/m3]
real, allocatable :: sm_sf (:, :)    ! value for water depth on surface per time step for row,column in mm; currently not implemented
!real, allocatable :: sm_1(:,:,:)        ! value for soil moisture in layer 1 per time step for row,column in mm; assigned in calc_sm (time step,row,column) [mm]
!real, allocatable :: sm_2(:,:,:)        ! value for soil moisture in layer 2 per time step for row,column in mm; assigned in calc_sm (time step,row,column) [mm]
!previous two lines replaced in ECOGEOM version with:
real, allocatable :: sm (:, :, :, :)    ! value for soil moisture in layer n per time step for row,column in mm; assigned in calc_sm (time step,row,column) [mm]

real, allocatable :: slope_radiant (:, :) ! value for slope for row,column in rad; assigned in interstorm_initialise
real, allocatable :: aspect_factor (:, :) ! value for aspect factor for row,column; assigned in interstorm_initialise

real, allocatable :: qsum_all (:, :, :)         ! time series for sum values of discharge (total amount after each storm event) in m3/sec??
real, allocatable :: sedtotal_all (:, :, :)     ! time series for sum values of sediment (total amount after each storm event) in kg
real, allocatable :: theta_areal_average (:, :) ! time series for areal average of soil moisture (1: upper and 2: lower layer) at the end of each day

real:: store_rainevents (4, 1000)    !stores the Julian day, year and number of records, maximum number of storm events set to 1000
real :: minimal_storm
integer:: r_event                    ! no. of rain events with threshold and minimal intensity greater or equal then specified in interstorm.dat

end module interstorm_shared_data
