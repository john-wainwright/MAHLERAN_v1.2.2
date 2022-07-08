
!****************************************************************
!  subroutine to read in temperature time series for interstorm calculations
!****************************************************************
subroutine read_temperature_xml

use interstorm_shared_data
use time_h
use parameters_from_xml

implicit none

integer :: dummy

character (len = 160) :: filename

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
! Tmean(:) declared in interstorm_shared_data (day) [�C]
! Tmax(:) declared in interstorm_shared_data (day) [�C]
! Tmin(:) declared in interstorm_shared_data (day) [�C]
!****************************************************************


!****************************************************************
!        opens the *.dat file and reads data for the temperature time series
!****************************************************************
filename = interstorm_input_folder (1: interstorm_input_folder_length) // continuous_temperature_data

open (3, file = filename, status = 'unknown')
rewind(3)

read (3, *)

do rday = 1, total_days
   read (3, *) dummy, Tmean (rday), Tmax (rday), Tmin (rday)
enddo

close (3)

write (*, *) 'Continuous temperature time series read in'


end
