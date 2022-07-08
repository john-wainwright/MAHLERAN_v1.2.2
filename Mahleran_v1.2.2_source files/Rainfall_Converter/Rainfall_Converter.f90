!  Rainfall_Converter.f90 
!
!  FUNCTIONS:
!	Rainfall_Converter      - Entry point of console application.
!
!	Example of displaying 'Hello World' at execution time.
!

!****************************************************************************
!
!  PROGRAM: Rainfall_Converter
!
!  PURPOSE:  Entry point for 'Hello World' sample console application.
!
!****************************************************************************

program Rainfall_Converter

use utils_h

integer counter, i, dummy1, dummy2, dummy3
real dummy4, dummy5, temp_max_dummy, temp_min_dummy
integer year(19000), julian(19000), hourmin(19000)
real temp_min_hourly(19000), temp_max_hourly(19000), temp_min_daily(19000),temp_max_daily(19000),temp_av_daily(19000),temp_av_hourly(19000)
real intensity(19000)	
CHARACTER (LEN=1000) :: cdummy

open (2, file = 'Met49_2005_06_rain.dat', status = 'unknown')
rewind (2)

dummy5=0
marker=0
counter=1

!read in rainfall time series from Seviellta (delete unnecessary columns in file)
!check the length of original data file (no header!),this one had 14982 rows
do i=1,18137
 READ(2,'(a)') cdummy
  dummy1=GetNumberOfSubstrings(cdummy)
  if (dummy1.eq.4) then
	READ(cdummy,*) year(counter), julian(counter), hourmin(counter), intensity(counter) 
	counter=counter+1
  endif
enddo
counter=counter-1
close(2)
write(*,*) 'Minute data successful read in, counter = ', counter

	
! write file in Mahleran interstorm format
! note: you still need to divide the 3rd column in two columns, using excel
open (3, file = 'R49_2005_06_rain.dat', status = 'unknown')
rewind (3)
do i=1,counter
	write(3,'(i4,x,i3,x,i4.4,x,f7.3)') year(i), julian(i), hourmin(i), intensity(i)
enddo
close(3)


!!!
!!! for temperature data
!!!
open (2, file = 'Met49_2010_11_temp.dat', status = 'unknown')
rewind (2)

dummy5=0
marker=0
counter=1

!read in temp time series from Seviellta (delete unnecessary columns in file)
!check the length of original data file (no header!),this one had 14608 rows
do i=1,14608
 READ(2,'(a)') cdummy
  dummy1=GetNumberOfSubstrings(cdummy)
  if (dummy1.eq.6) then
	READ(cdummy,*) year(counter), julian(counter), hourmin(counter), 
                       temp_av_hourly(counter), &
	               temp_max_hourly(counter), temp_min_hourly(counter) 
	counter=counter+1
  endif
enddo
counter=counter-1
close(2)
write(*,*) 'Hourly temperature data successful read in, counter = ', counter

!calculate daily min, max and average temperature
do i=1,counter/24
	temp_min_dummy=50.
	temp_max_dummy=-10.
	do k=1,24
		temp_min_daily(i)=min(temp_min_hourly(k+24*i-24),temp_min_dummy)
		temp_min_dummy=temp_min_daily(i)
		temp_max_daily(i)=max(temp_max_hourly(k+24*i-24),temp_max_dummy)
		temp_max_dummy=temp_max_daily(i)
	enddo
	temp_av_daily(i)=(temp_min_daily(i)+temp_max_daily(i))/2
enddo

open (3, file = 'R49_2010_11_temp.dat', status = 'unknown')
rewind (3)
write(3,*) '"Temperature of Station 49(rday, Tmean, Tmax, Tmin)"'
do i=1,counter/24
	write(3,'(i4,x,f7.3,x,f7.3,x,f7.3)') i, temp_av_daily(i), temp_max_daily(i), temp_min_daily(i)
enddo
close(3)




end program Rainfall_Converter

