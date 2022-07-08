!**********************************************************************
!  subroutine to calculate annual statistics of all fluxes and soil moisture
!**********************************************************************
subroutine Annual_statistics

use shared_data
use interstorm_shared_data
use time_h
use parameters_from_xml

implicit none

real, allocatable :: keeper(:,:), keeper2(:,:)

real :: Rain (50000), Water (50000), Sediment (50000), Moisture1 (50000), Moisture2 (500000), dummy, dummy1 (13)
real :: StatOut (4, 2, 100), Annual_rain (500), rainkeeper (500)

integer :: runday(50000), Julianday(50000), Year(50000)
integer :: size_array (100)
integer :: i, j, n, stat, no_events, k, no_days

character (len = 160) :: filename

!open storm_flux file and read in all data
filename = output_folder (1:output_folder_length) // 'storm_fluxes000.dat'
if (iout.lt.10) then
   write (filename (15 + output_folder_length: 15 + output_folder_length), '(i1)') iout
elseif (iout.lt.100) then
   write (filename (14 + output_folder_length: 15 + output_folder_length), '(i2)') iout
elseif (iout.lt.1000) then
   write (filename (13 + output_folder_length: 15 + output_folder_length), '(i3)') iout
endif

open (125, file = filename, status = 'unknown') !actually something wrong if this file doesn't exist
rewind (125)
read (125,*)

no_events = 0
do i = 1, 50000
   !n.b. this 9999 format statement MUST be the same as the one in output_interstorm_fluxes
   read (125, 9999, iostat = stat) runday (i), Julianday (i), Year (i), Rain (i), Water (i), Sediment(i), dummy, dummy, dummy
   if (stat /= 0) then
      exit
   endif
   no_events = no_events + 1
enddo

close (125)

!Calculate annual summary statistics for water and sediment fluxes
size_array (:) = 0
n = 1
j = 1

do i = 1, no_events
   if (Year (i).eq.tstart + n - 1) then
      j = j + 1
! arrange array until next year starts
      if (Year (i + 1).gt.Year (i).or.i.eq.no_events) then
         size_array (n) = j - 1         !checks how many events happened per year
         n = n + (Year (i + 1) - Year (i))
         j = 1
      endif
   endif
enddo

n = 1
j = 1

allocate (keeper (2, size_array (1)))
keeper (:, :) = 0.

do i = 1, no_events
   if (Year (i).eq.tstart + n - 1) then
      keeper (1, j) = Water (i)
      keeper (2, j) = Sediment (i)
      j = j + 1
! arrange array until next year starts
      if (Year (i + 1).gt.Year (i).or.i.eq.no_events) then
         StatOut (1, :, n) = sum (keeper, dim = 2)   !annual sum value for water and sediment fluxes
         StatOut (2, :, n) = maxval (keeper, dim = 2)    !annual max value for water and sediment fluxes
         StatOut (3, :, n) = minval (keeper, dim = 2)    !annual min value for water and sediment fluxes
         StatOut (4, :, n) = StatOut (1, :, n) / real (j - 1) !annual average value for water and sediment fluxes
!         write(*,*) StatOut(2,:,n), StatOut(3,:,n), StatOut(4,:,n)
         deallocate (keeper)
         n = n + (Year (i + 1) - Year (i))
         if (i.ne.no_events) then
            allocate (keeper (2, size_array (n)))
            j = 1
         endif
      endif
   endif
enddo

! Write annual summary statistics in new output file: first the fluxes, then the soil moisture dynamics
filename = output_folder (1:output_folder_length) // 'longterm_statistics000.dat'
if (iout.lt.10) then
   write (filename (22 + output_folder_length: 22 + output_folder_length), '(i1)') iout
elseif (iout.lt.100) then
   write (filename (21 + output_folder_length: 22 + output_folder_length), '(i2)') iout
elseif (iout.lt.1000) then
   write (filename (20 + output_folder_length: 22 + output_folder_length), '(i3)') iout
endif
open (125, file = filename, status = 'unknown')
rewind (125)
write (125, '(a)') '_________________________________________________________________________________'
write (125, '(a)') 'Annual values for water (litres) and sediment (kg) fluxes: sum, max, min and mean'
write (125, '(a)') '_________________________________________________________________________________'

!water statistics
write (125, '(a)') 'Water statistics (in litres): sum, max, min and mean'
do n = 1, no_years
   write (125, '(1i4, 4(1x, e10.4))') tstart + n - 1, ((StatOut (i, 1, n)), i = 1, 4)
enddo
write (125, *)

!sediment statistics
write (125, '(a)') '_________________________________________________________________________________'
write (125, '(a)') 'Sediment statistics (in kg): sum, max, min and mean'
do n = 1, no_years
   write (125, '(1i4, 4(1x, e10.4))') tstart + n - 1, ((StatOut (i, 2, n)), i = 1, 4)
enddo
write (125, *)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!! soil moisture dynamics
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!open soil moisture flux file and read in all data
filename = output_folder (1:output_folder_length) // 'interstorm_moisture000.dat'
if (iout.lt.10) then
   write (filename (22 + output_folder_length: 22 + output_folder_length), '(i1)') iout
elseif (iout.lt.100) then
   write (filename (21 + output_folder_length: 22 + output_folder_length), '(i2)') iout
elseif (iout.lt.1000) then
   write (filename (20 + output_folder_length: 22 + output_folder_length), '(i3)') iout
endif
open (124, file = filename, status = 'unknown')
rewind (124)
read (124, *)   !read title

no_events = 0
do i = 1, 50000
   !n.b. this 9998 format statement MUST be the same as the one in output_interstorm_xml
   read (124, 9998, iostat = stat) runday (i), Julianday (i), Year (i), Rain (i), dummy, dummy, &
                                   Moisture1 (i), Moisture2 (i), (dummy1 (n), n = 1, 12)
   if (stat /= 0) then
      exit
   endif
   no_events = no_events + 1
enddo
close (124)

!Calculate annual summary statistics for soil moisture (upper and lower layer) and rain
n = 1
j = 1

!check for leap years
if (mod (tstart, 4).eq.0) then
   no_days = 366
else
   no_days = 365
endif
allocate (keeper2 (2, no_days - 1)) !note: the very first day starts at day 2
keeper2 (:, :) = 0.

do i = 1, no_events
   if (Year (i).eq.tstart + n - 1) then
      keeper2 (1, j) = Moisture1 (i)
      keeper2 (2, j) = Moisture2 (i)
      j = j + 1
! arrange array until next year starts
      if (Year (i + 1).gt.Year (i).or.i.eq.no_events) then
         StatOut (1, :, n) = sum (keeper2, dim = 2)      !annual sum value for upper and lower soil layer
         StatOut (2, :, n) = maxval (keeper2, dim = 2)   !annual max value for upper and lower soil layer
         StatOut (3, :, n) = minval (keeper2, dim = 2)   !annual min value for upper and lower soil layer
         StatOut (4, :, n) = StatOut (1, :, n) / real (j - 1) !annual average value for upper and lower soil layer
!         write (*, *) StatOut (2,:,n), StatOut (3, :, n), StatOut (4, :, n)
         deallocate (keeper2)
         n = n + (Year (i + 1) - Year (i))
         !check for leap years
     if (mod (tstart + n - 1, 4).eq.0) then
            no_days = 366
         else
        no_days = 365
         endif

         if (i.ne.no_events) then
            allocate (keeper2 (2, no_days))
            keeper2 (:, :) = 0.
         endif
         j = 1
!         keeper (:, :) = 0
      endif
   endif
enddo

!calculate annual rainfall
n = 1
j = 1

rainkeeper (:) = 0.0
do i = 1, no_events
   if (Year (i).eq.tstart + n - 1) then
      rainkeeper (j) = Rain (i)
      j = j + 1
! arrange array until next year starts
      if (Year (i + 1).gt.Year (i).or.i.eq.no_events) then
         Annual_rain (n) = sum (rainkeeper) !annual sum value for upper and lower soil layer
         n = n + (Year (i + 1) - Year (i))
         j = 1
         rainkeeper (:) = 0
      endif
   endif
enddo

!Write annual summary statistics of soil moisture,

!soil moisture statistics
write (125, '(a)') '_________________________________________________________________________________'
write (125, '(a)') 'Annual rain (mm) and soil moisture statistics (%) for layer 1 and 2: max, min and mean'
do n = 1, no_years
   write (125, '(1i4, 7(1x, e10.4))') tstart + n - 1, Annual_rain (n), &
                                      ((StatOut (i, 1, n)), i = 2, 4), ((StatOut (i, 2, n)), i = 2, 4)
enddo

close (125)

!n.b. this 9999 format statement MUST be the same as the one in output_interstorm_fluxes
9999   format (i6, 1x, i3, 1x, i4, 1x, f7.3, 7(1x, e10.4))
!n.b. this 9998 format statement MUST be the same as the one in output_interstorm_xml
9998   format (i6, 1x, i3, 1x, i4, 1x, f7.3, 16(1x, e10.4))

end
