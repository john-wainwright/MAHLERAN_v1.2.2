
!****************************************************************
!  subroutine to read in temperature time series for interstorm calculations
!****************************************************************
subroutine read_temperature

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
! cont_temp_file declared in interstorm_shared_data [-]
! rday declared in interstorm_shared_data [-]
! total_days declared in interstorm_shared_data [-]
! Tmean(:) declared in interstorm_shared_data (day) [°C]
! Tmax(:) declared in interstorm_shared_data (day) [°C]
! Tmin(:) declared in interstorm_shared_data (day) [°C]
!****************************************************************


!****************************************************************
!        opens the *.dat file and reads data for the temperature time series
!****************************************************************
open (3, FILE=pfadin_interstorm(1:pfadi_interstorm)//cont_temp_file , status = 'unknown')
rewind(3)
read (3,*)

do rday=1, total_days
    read (3,*) dummy, Tmean(rday), Tmax(rday), Tmin(rday)
enddo
close(3)

write(*,*) 'Temperature time series read in'


end
