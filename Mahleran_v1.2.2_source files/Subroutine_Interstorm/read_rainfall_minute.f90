
!****************************************************************
!  subroutine to read in rainfall time series for interstorm calculations
!****************************************************************
subroutine read_rainfall_minute

use interstorm_shared_data
use time_h
use utils_h
integer k, j,counter, file_length, dummy, dummy1,dummy2, dummy3, dummy4, dtotal
real minimal_storm,dummy5
CHARACTER (LEN=1000) :: cdummy

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
file_length=1
m=1
open (5, FILE=pfadin_interstorm(1:pfadi_interstorm)//cont_minute_rain_file , status = 'unknown')
rewind(5)
read (5,*); read(5,*);read(5,*);
do k=1,600000
    read(5,*) dummy1, dummy2, dummy3, dummy4, dummy5
        m=m+1
    if (dummy1.eq.-9999) exit
enddo
close(5)
file_length=m-1

allocate (rain_minute_t(4,file_length), rain_minute(file_length))
rain_minute_t(:,:)=0; rain_minute(:)=0


! read in continuous minute rainfall data
open (5, FILE=pfadin_interstorm(1:pfadi_interstorm)//cont_minute_rain_file , status = 'unknown')
rewind(5)
read (5,*); read(5,*);read(5,*);
do k=1,file_length
    read(5,*) dummy1, dummy2, dummy3, dummy4, dummy5
        rain_minute_t(1,k)=dummy1   !year
        rain_minute_t(2,k)=dummy2   !Julian day
        rain_minute_t(3,k)=dummy3   !hour
        rain_minute_t(4,k)=dummy4   !minute
        rain_minute(k)=dummy5
    if (dummy1.eq.-9999) exit
enddo
close(5)

!calculate daily rainfall
rain_daily(:)=0.
j=0
rday=0; dtotal=0;
dummy1=tstart
do k=1,file_length

! get current year in rain file
    t=rain_minute_t(1,k)
! check for leap years and no. of days in that year
    call calcyear
! if next year begins
    if (rain_minute_t(1,k).eq.dummy1+1) then
        j=j+1
        dtotal=dtotal+daylastyear
        dummy1=dummy1+1
    endif
! daily calculation for first year (not including the months without data)
    if (rain_minute_t(1,k).eq.tstart) then
        rday=rain_minute_t(2,k)-dayoutsim
        rain_daily(rday)=rain_daily(rday)+rain_minute(k)
!       total_days=dayyear-dayoutsim
    elseif(rain_minute_t(1,k).eq.tstart+j) then
        rday= dtotal + rain_minute_t(2,k)
        if (rday.gt.total_days) then
            write(*,*) 'Rainfall with minute data is longer than chosen simulation period'
            exit
        endif
        rain_daily(rday)=rain_daily(rday)+rain_minute(k)
!   elseif(dummy1.eq.tstart+m+1) then
!       m=m+1
!       dtotal=dtotal+dayyear
        ! leave loop at end of simulation period
    elseif(rain_minute_t(1,k).eq.-9999) then
        exit
    else
        write(*,*) 'there seems to be a year without any rain?'
        stop
    endif
enddo
dtotal=dtotal+dayyear

deallocate (rain_minute_t, rain_minute)

!write file with daily rainfall generated from 1-min resolution time series
t = tstart
call calcyear
Julian=dayoutsim+1
CALL calcyear   !check for leap years, number of days for year
open(7, FILE=pfadout_interstorm(1:pfado_interstorm)//'daily_rain_calculated.dat' , status = 'unknown')
rewind(7)
write(7,*) 'Year, Julian_day, running_day, daily_rain_in mm/day'
do rday=1,total_days
    write(7,*) t, Julian,rday, rain_daily(rday)
        Julian=Julian+1     ! to inlcude information on leap years etc.
    if (t.eq.tstart) then
        if (Julian.eq.dayyear+dayoutsim+1) then
            t=tstart+1
            Julian=1
            call calcyear
        endif
    else
        if (Julian.eq.dayyear+1) then
            t=t+1
            Julian=1
            call calcyear
        endif
    endif
enddo
close(7)


!rewrite the block above, so that it only reads in arrays where it actually rains!
minimal_storm=0.2   !minimal storm intensity in mm/min
m=1.
open (5, FILE=pfadin_interstorm(1:pfadi_interstorm)//cont_minute_rain_file , status = 'unknown')
rewind(5)
read (5,*); read(5,*);read(5,*);
do k=1,file_length
    read(5,*) dummy1, dummy2, dummy3, dummy4, dummy5
    if (dummy5.gt.minimal_storm) then
        m=m+1
    endif
enddo
close(5)

file_length_rain=m-1
allocate (rain_minute_t(4,file_length_rain), rain_minute(file_length_rain))
rain_minute_t(:,:)=0; rain_minute(:)=0


open (5, FILE=pfadin_interstorm(1:pfadi_interstorm)//cont_minute_rain_file , status = 'unknown')
rewind(5)
read (5,*); read(5,*);read(5,*);
m=1
do k=1,file_length
    read(5,*) dummy1, dummy2, dummy3, dummy4, dummy5
    if (dummy5.gt.minimal_storm) then
        rain_minute_t(1,m)=dummy1   !year
        rain_minute_t(2,m)=dummy2   !Julian day
        rain_minute_t(3,m)=dummy3   !hour
        rain_minute_t(4,m)=dummy4   !minute
        rain_minute(m)=dummy5*60    !!rainfall rate for mahleran storm with intensity in mm/h
        m=m+1
    endif
    if (dummy5.eq.-9999) exit
enddo
close(5)

open(6, FILE=pfadout_interstorm(1:pfado_interstorm)//'rainstorm_events.dat' , status = 'unknown')
rewind(6)
do k=1,file_length_rain
    write(6,*)  rain_minute_t(1,k), rain_minute_t(2,k), rain_minute_t(3,k),rain_minute_t(4,k), rain_minute(k)/60
enddo

close(6)
write(*,*) 'Rainfall time series (minutes) read in'







end
