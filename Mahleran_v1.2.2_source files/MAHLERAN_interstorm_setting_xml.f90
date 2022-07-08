
!****************************************************************
!  subroutine to set parameters and files for interstorm calculation
!****************************************************************
 subroutine MAHLERAN_interstorm_setting_xml

 use interstorm_shared_data
 use parameters_from_xml


!****************************************************************
! variables used in this subroutine listed in order of their occurrence
! global
!****************************************************************
! rain_actual; declared in interstorm_shared_data [-]
!****************************************************************

!
!read in interstorm parameters from file
!
call read_interstorm_parameters_xml

!
!read in time series for daily rainfall
!
! call read_rainfall_daily_xml
call read_rainfall_minute_xml
! set rainmarker at the beginning of the continuous rain file
rain_actual = 1
!
! read in time series for daily temperature
!
call read_temperature_xml
!
! read in all maps
!
call read_shrub_cov_xml
!
!initialise all arrays and variables
!
call interstorm_initialize_xml

end



