
!****************************************************************
!  subroutine to read in rainfall time series for interstorm calculations
!****************************************************************
subroutine read_rainfall_daily_xml

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
! --> now interstorm_input_folder in parameters_from_xml
! pfadi_interstorm declared in interstorm_shared_data [-]
! --> now interstorm_input_folder_length in parameters_from_xml
! cont_daily_rain_file declared in interstorm_shared_data [-]
! --> now continuous_rainfall_data in parameters_from_xml
! rday declared in interstorm_shared_data [-]
! total_days declared in interstorm_shared_data [-]
! rain_daily(:) declared in interstorm_shared_data (day) [mm]
!****************************************************************


!****************************************************************
!        opens the *.dat file and reads data for the rainfall time series
!****************************************************************
filename = interstorm_input_folder (1: interstorm_input_folder_length) // continuous_rainfall_data
open (4, file = filename, status = 'unknown')
rewind (4)
read (4, *)

do rday = 1, total_days
   read (4, *) dummy, rain_daily (rday)
enddo
close (4)


write (*, *) 'Rainfall time series read in'

end
