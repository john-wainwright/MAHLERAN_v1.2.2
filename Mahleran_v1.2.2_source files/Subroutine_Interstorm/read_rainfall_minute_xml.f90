
!****************************************************************
!  subroutine to read in rainfall time series for interstorm calculations
!****************************************************************
subroutine read_rainfall_minute_xml

use shared_data
use interstorm_shared_data
use time_h
use utils_h
use parameters_from_xml

implicit none

!real :: minimal_storm, dummy5  !minimal storm defined in interstorm_shared_data
real :: dummy5, dailyrain

integer :: k, j, counter, file_length, dummy, dummy1, dummy2, dummy3, dummy4, dtotal
integer :: n, dummy_year, dummy_day, switch, ierr

character (len = 1000) :: cdummy
character (len = 160) :: filename

!****************************************************************
! variables used in this subroutine listed in order of their occurrence
! local
!****************************************************************
! file_length
! k
! dummy1
! dummy2
! dummy3
! dummy4
! dummy5
! dtotal
!****************************************************************
! global
!****************************************************************
! rain_minute_t declared in interstorm_shared_data (year,day) [hourminute]
! rain_minute declared in interstorm_shared_data (minute) [mm/min]
! m declared in general_h [month]
! pfadin_interstorm declared in interstorm_shared_data [-]
! pfadi_interstorm declared in interstorm_shared_data [-]
! cont_minute_rain_file declared in interstorm_shared_data [-]
! tstart
! rday declared in interstorm_shared_data [-]
! dayoutsim
! rain_daily(:) declared in interstorm_shared_data (day) [mm]
! dayyear
! julian
! pfadout_interstorm
! pfadou_interstorm
! total_days declared in interstorm_shared_data [-]
! t
! minimal_storm
!****************************************************************

! opens the *.dat file and reads data for the rainfall time series

!Check length of rain file
file_length = 1
m = 1
filename = interstorm_input_folder (1: interstorm_input_folder_length) // one_min_rainfall_data
write (6, *) 'Minute rainfall file: ', filename
!JW changed from 5 to avoid conflict with stdin (keyboard)
open (555, file = filename, status = 'unknown')
rewind (555)

read (555, *)
read (555, *)
read (555, *)

do k = 1, 600000
   read (555,*) dummy1, dummy2, dummy3, dummy4, dummy5
   m = m + 1
   if (dummy1.eq.-9999) then
      exit
   endif
enddo
!close(555)
file_length = m - 1

allocate (rain_minute_t (4, file_length), source = 0, stat = ierr)
if (ierr /= 0 ) stop "Memory error allocating rain_minute_t in read_rainfall_minute_xml first attempt!"
allocate (rain_minute (file_length), source = 0.0e0, stat = ierr)
if (ierr /= 0 ) stop "Memory error allocating rain_minute in read_rainfall_minute_xml first attempt!"

!now replaced by source terms in allocate statement
!rain_minute_t (:, :) = 0
!rain_minute (:) = 0

! read in continuous minute rainfall data
!filename = interstorm_input_folder (1: interstorm_input_folder_length) // one_min_rainfall_data
!open (555, file = filename, status = 'unknown')
!file not now closed so just rewind to the start
rewind(555)
read (555, *)
read (555, *)
read (555, *)
do k = 1, file_length
   read (555, *) dummy1, dummy2, dummy3, dummy4, dummy5
   rain_minute_t (1, k) = dummy1    !year
   rain_minute_t (2, k) = dummy2    !Julian day
   rain_minute_t (3, k) = dummy3    !hour
   rain_minute_t (4, k) = dummy4    !minute
   rain_minute (k) = dummy5
   if (dummy1.eq.-9999) then
      exit
   endif
enddo
!close(555)

!calculate daily rainfall
rain_daily (:) = 0.
j = 0
rday = 0
dtotal = 0
dummy1 = tstart
do k = 1, file_length
! get current year in rain file
   t = rain_minute_t (1, k)
! check for leap years and no. of days in that year
   call calcyear
! if next year begins
   if (rain_minute_t (1, k).eq.(dummy1 + 1)) then
      j = j + 1
      dtotal = dtotal + daylastyear
      dummy1 = dummy1 + 1
   endif
! daily calculation for first year (not including the months without data)
   if (rain_minute_t (1, k).eq.tstart) then
      rday = rain_minute_t (2, k) - dayoutsim
      rain_daily (rday) = rain_daily (rday) + rain_minute (k)
!       total_days=dayyear-dayoutsim
   elseif (rain_minute_t (1, k).eq.(tstart + j)) then
      rday = dtotal + rain_minute_t (2, k)
      if (rday.gt.total_days) then
     write (*, *) 'WARNING: Rainfall file with minute data is longer than chosen simulation period: ', k
         exit
      endif
!      if (test_flag.eq.1) then
!          write (6, *) ' Shouldn"t really be here'
!      endif
      rain_daily (rday) = rain_daily (rday) + rain_minute (k)
!   elseif(dummy1.eq.tstart+m+1) then
!       m=m+1
!       dtotal=dtotal+dayyear
        ! leave loop at end of simulation period
   elseif (rain_minute_t (1, k).eq.-9999) then
      write (6, *) 'rain_minute_t (1, k).eq.-9999 - should leave read_rainfall_minute_xml loop'
      exit
   else
      write(*,*) 'WARNING: there seems to be a year without any rain?'
      stop
   endif
enddo
dtotal = dtotal + dayyear

deallocate (rain_minute_t, rain_minute)

!write file with daily rainfall generated from 1-min resolution time series
t = tstart
call calcyear
Julian = dayoutsim + 1
call calcyear   !check for leap years, number of days for year
filename = output_folder (1: output_folder_length) // 'daily_rain_calculated000.dat'
if (iout.lt.10) then
   write (filename (24 + output_folder_length: 24 + output_folder_length), '(i1)') iout
elseif (iout.lt.100) then
   write (filename (23 + output_folder_length: 24 + output_folder_length), '(i2)') iout
elseif (iout.lt.1000) then
   write (filename (22 + output_folder_length: 24 + output_folder_length), '(i3)') iout
endif
open (7, file = filename, status = 'unknown')
rewind (7)
write (7, *) 'Year Julian_day running_day daily_rain[mm/day]'
do rday = 1, total_days
   write (7, *) t, Julian, rday, rain_daily (rday)
   Julian = Julian + 1      ! to include information on leap years etc.
   if (t.eq.tstart) then
      if (Julian.eq.(dayyear + dayoutsim + 1)) then
         t = tstart + 1
         Julian = 1
         call calcyear
      endif
   else
      if (Julian.eq.(dayyear + 1)) then
     t = t + 1
         Julian = 1
         call calcyear
      endif
   endif
enddo
close (7)


!rewrite the block above, so that it only reads in arrays where it actually rains!
!minimal_storm  !minimal storm intensity in mm/min
!threshold_rain     !threshold daily rain in mm, is read in interstorm.dat
m = 0
n = 0
dummy_year = 0
dummy_day = 0
counter = 0
switch = 0
dailyrain = 0.
store_rainevents (:, :) = 0

!open (555, FILE=pfadin_interstorm(1:pfadi_interstorm)//cont_minute_rain_file , status = 'unknown')
rewind (555)  !not closed so just rewind
read (555, *)
read (555, *)
read (555, *)
do k = 1, file_length
   read (555,*) dummy1, dummy2, dummy3, dummy4, dummy5
   if (dummy1.eq.dummy_year.and.dummy2.eq.dummy_day) then
   ! check if during the rain event a minimal intensity is reached
      if (dummy5.ge.minimal_storm) then
         switch = 1
      endif
      counter = counter + 1
      dailyrain = dailyrain + dummy5
   else
      if (switch.eq.1.and.dailyrain.ge.threshold_rain) then
         m = m + counter + 1
         n = n + 1
         store_rainevents (1, n) = dummy_day    !stores Julian day, where high-intensity storm occured
         store_rainevents (2, n) = dummy_year   !stores year, where high-intensity storm occured
         store_rainevents (3, n) = counter      !stores length of record for that particular storm
         store_rainevents (4, n) = dailyrain / counter * 60
         switch = 0
         dailyrain = 0.
      elseif (switch.eq.1.and.dailyrain.lt.threshold_rain) then
         switch=0
      endif
      dummy_year=dummy1
      dummy_day = dummy2
      counter = 0
      dailyrain = 0.
   endif
enddo
!close(555)
if (switch.eq.1) then
   m = m + counter
endif

file_length_rain = m
!allocate (rain_minute_t (4, file_length_rain), rain_minute (file_length_rain))
allocate (rain_minute_t (4, file_length_rain), source = 0, stat = ierr)
if (ierr /= 0 ) stop "Memory error allocating rain_minute_t in read_rainfall_minute_xml second attempt!"
allocate (rain_minute (file_length_rain), source = 0.0e0, stat = ierr)
if (ierr /= 0 ) stop "Memory error allocating rain_minute in read_rainfall_minute_xml second attempt!"

!now replaced by source terms in allocate statement
!rain_minute_t (:, :) = 0
!rain_minute (:) = 0

!open (555, FILE=pfadin_interstorm(1:pfadi_interstorm)//cont_minute_rain_file , status = 'unknown')
rewind (555)  !still not closed!
read (555, *)
read (555, *)
read (555, *)
m = 1
n = 1
counter = 0
do k = 1, file_length
   read (555, *) dummy1, dummy2, dummy3, dummy4, dummy5
   if (store_rainevents (2, n).eq.dummy1.and.store_rainevents (1, n).eq.dummy2) then
      rain_minute_t (1, m) = dummy1 !year
      rain_minute_t (2, m) = dummy2 !Julian day
      rain_minute_t (3, m) = dummy3 !hour
      rain_minute_t (4, m) = dummy4 !minute
      rain_minute (m) = dummy5 * 60.    !!rainfall rate for mahleran storm with intensity in mm/h
      m = m + 1
      counter = counter + 1
      if (counter - 1.eq.store_rainevents (3, n)) then
         n = n + 1
         counter = 0
      endif
   endif
   if (dummy5.eq.-9999) then
      exit
   endif
enddo
close (555)  !now unit 555 can be closed!

filename = output_folder (1: output_folder_length) // 'rainstorm_events000.dat'
if (iout.lt.10) then
   write (filename (19 + output_folder_length: 19 + output_folder_length), '(i1)') iout
elseif (iout.lt.100) then
   write (filename (18 + output_folder_length: 19 + output_folder_length), '(i2)') iout
elseif (iout.lt.1000) then
   write (filename (17 + output_folder_length: 19 + output_folder_length), '(i3)') iout
endif
!JW changed from unit 6 to avoid conflict with screen
open (666, file = filename, status = 'unknown')
rewind (666)
do k = 1, file_length_rain
   write (666, *) rain_minute_t (1, k), rain_minute_t (2, k), rain_minute_t (3, k), rain_minute_t (4, k), rain_minute (k) / 60.
enddo

close (666)
write (*,*) 'Continuous rainfall time series (in minutes) read in'

end
