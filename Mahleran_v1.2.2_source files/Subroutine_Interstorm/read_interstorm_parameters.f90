!****************************************************************
!  subroutine to read parameters for interstorm calculations
!****************************************************************
subroutine read_interstorm_parameters

use interstorm_shared_data
use shared_data
use time_h
implicit none
logical fexist
character *80 filename, file
integer i


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
! Tmean(:) declared in interstorm_shared_data (day) [°C]
! Tmax(:) declared in interstorm_shared_data (day) [°C]
! Tmin(:) declared in interstorm_shared_data (day) [°C]
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


 filename = 'interstorm.dat'


!
!check if input parameter file for interstorm dynamics exist
!
 inquire (file = filename, exist = fexist)
 if (.not.fexist) then
     write(*,*)
     write(*,*) 'input parameter file does not exist'
     write(*,*)
     stop
 endif

!
!if file interstorm.dat does exist, start reading from file
!
if (fexist) then

    open (2, file = filename, status = 'unknown')
  rewind (2)
  read (2,*)
  read (2,'(a)') pfadin_interstorm              ! set input folder for interstorm
  read (2,'(a)') pfadout_interstorm             ! set output folder for interstorm
  read (2,*) tstart                             ! read in start year of simulation
  read (2,*) tstop                              ! read in end year of simulation
  READ(2,*) mstart                              ! read in start months in the first year
  READ(2,*) mstop                               ! read in stop months in the last year
  no_years=tstop-tstart+1                       ! number of simulated years
  read (2,'(a)') cont_temp_file                 ! read in name of file with cont. rainfall data
  read (2,'(a)') cont_daily_rain_file           ! read in name of file with cont. temperature data
  read (2,'(a)') cont_minute_rain_file          ! read in name of file with cont. temperature data
  read (2,'(a)') shrub_cov_file                 ! read in name of file with shrub cover mask data
  read (2,*) threshold_rain                     ! read in minimal daily rainfall amount for which Mahleran storm is calculated [mm]
  read (2,*) latitude                           ! read in value for latitude as used for evapotranspiration calculation in rad
  read (2,*) lat_deg                            ! read in value for latitude as used for evapotranspiration calculation in degrees
  read (2,*) depth_1                            ! read in value for depth of layer 1 in mm
  read (2,*) depth_2                            ! read in value for depth of layer 2 in mm
  read (2,*) rw                                 ! read in value for residual water content in mm**3/mm**3
  read (2,*) wsc                                ! read in value for water content of beginning stomatal closure [m**3/m**3]
  read (2,*) et_red_fac                         ! read in value for constatnt for evapotranspiration reduction due to crusting, desert pavement [-]
  read (2,*) inf_rate_bare_2                    ! read in value for infiltration rate from bare surface to layer 2 [mm]
  read (2,*) inf_rate_2                         ! read in value for infiltration rate from shrub covered surface to layer 2 [mm]
  read (2,*) k_s                                ! read in value for saturated hydraulic conductivity [mm/h]
  read (2,*) d_const                            ! read in value for diffusion coefficient [-]
  read (2,*) sm_1_init                          ! read in initial value for sm_1 [mm]
  read (2,*) sm_2_init                          ! read in initial value for sm_2 [mm]

  close(2)

endif

!check total number of days (total_days)
t=tstart            !set the beginning of the calculation
total_days=0
do t=tstart, tstop !
    call calcyear
enddo
total_days=dtot

allocate (Tmean(total_days))
allocate (Tmax(total_days))
allocate (Tmin(total_days))
allocate (rain_daily(total_days))
allocate (shrub_cover(nr2, nc2), grass_cover(nr2,nc2))
allocate (et_pot(total_days,nr2,nc2), et_1(total_days,nr2,nc2), et_2(total_days,nr2,nc2))
allocate (inf_2(total_days,nr2,nc2), inf_1(total_days,nr2,nc2))
allocate (drain_1(total_days,nr2,nc2), drain_2(total_days,nr2,nc2), sm_1to2(total_days,nr2,nc2))
allocate (sm_sf(total_days,nr2,nc2), sm_1(total_days,nr2,nc2), sm_2(total_days,nr2,nc2))
allocate (theta_begin(nr2,nc2), field_cap(nr2,nc2))
allocate (qsum_all(total_days,nr2,nc2))
allocate (sedtotal_all(total_days,nr2,nc2))
allocate (theta_areal_average(total_days,2))


! determine length of path name for output files
DO i=1,sizeof(pfadin_interstorm)
  IF (pfadin_interstorm(i:i) == ' ') THEN
    pfadi_interstorm=i-1
    EXIT
  END IF
END DO

! determine length of basic model path name
DO i=1,sizeof(pfadout_interstorm)
  IF (pfadout_interstorm(i:i) == ' ') THEN
    pfado_interstorm=i-1
    EXIT
 END IF
END DO


end
