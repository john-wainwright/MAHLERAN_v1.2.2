
!****************************************************************
!  subroutine to read in rainfall time series for interstorm calculations
!****************************************************************
subroutine read_rainfall_daily

use interstorm_shared_data
use time_h
implicit none
integer dummy

!****************************************************************
! variables used in this subroutine listed in order of their occurrence
! local
!****************************************************************
! dummy is a dummy for the date in time series given in [ddmmyyyy]
!****************************************************************
! global
!****************************************************************
! pfadin_interstorm declared in interstorm_shared_data [-]
! pfadi_interstorm declared in interstorm_shared_data [-]
! cont_daily_rain_file declared in interstorm_shared_data [-]
! rday declared in interstorm_shared_data [-]
! total_days declared in interstorm_shared_data [-]
! rain_daily(:) declared in interstorm_shared_data (day) [mm]
!****************************************************************


!****************************************************************
!        opens the *.dat file and reads data for the rainfall time series
!****************************************************************
open (4, FILE=pfadin_interstorm(1:pfadi_interstorm)//cont_daily_rain_file , status = 'unknown')
rewind(4)
read (4,*)

do rday=1,total_days
    read (4,*) dummy, rain_daily(rday)
enddo
close(4)


write(*,*) 'Rainfall time series read in'




end
