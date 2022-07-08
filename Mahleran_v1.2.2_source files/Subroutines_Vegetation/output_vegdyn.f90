!****************************************************************
!  subroutine to output vegetation dyanmics
!****************************************************************
subroutine output_vegdyn

use vegdynamics_shared_data
use shared_data
use interstorm_shared_data
use parameters_from_xml

use time_h
implicit none

real :: dummy (4, 10)

integer :: i, j, k, n

character (len = 120) :: filename
character (len = 50) :: fmtstring

n = 0

! output file for vegetation dynamics
if (rday.eq.0) then
   filename = output_folder (1:output_folder_length) // 'vegetation_dynamics000.dat'
   if (iout.lt.10) then
      write (filename (22 + output_folder_length: 22 + output_folder_length), '(i1)') iout
   elseif (iout.lt.100) then
      write (filename (21 + output_folder_length: 22 + output_folder_length), '(i2)') iout
   elseif (iout.lt.1000) then
      write (filename (20 + output_folder_length: 22 + output_folder_length), '(i3)') iout
   endif
   open (127, FILE = filename, status = 'unknown')
   rewind (127)
   !JW May2017 assumes no_species = 2, and inconsistent with output below
   !TODO make general for any no_species
   write (127, '(a)') 'rday Julian Year Cover_grass[%] Cover_shrub[%] Growth_grass[%] Growth_shrub[%] '// &
                      'Mort_grass[%] Mort_shrub[%] Disp_grass[%] Disp_shrub[%]'
elseif (rday.gt.0.and.rday.lt.total_days) then
! calculate summary statistics for all cells within rainmask
   dummy (:, :) = 0.
   do i = 2, nr
      do j = 2, nc
         if (rmask (i, j).ge.0.0d0) then
            do k = 1, no_species
               dummy (1, k) = dummy (1, k) + c_veg (2, k, i, j) !sum value to calculate average veg parameters
               dummy (2, k) = dummy (2, k) + gr (k, i, j)
               dummy (3, k) = dummy (3, k) + mort (k, i, j)
               dummy (4, k) = dummy (4, k) + disp (k, i, j)
            enddo
            n = n + 1
         endif
      enddo
   enddo
!   write(*,*) n
   do i = 1, 4
      do k = 1, no_species
         dummy (i, k) = dummy (i, k) / real (n) !area_averaged veg variables
      enddo
   enddo
!   write(*,*) c_veg(2,1,5,8), gr(2,5,8), dummy(1,1)
   write (fmtstring, '(a, i0, a)') '(3i6, ', (8 * no_species), 'f14.3)'
   write (127, fmtstring) rday, Julian, t, ((dummy (n, k), k = 1, no_species), n = 1, 4)
!   write (127, '(3i6,<8*no_species>f14.3)') rday, Julian, t, ((dummy (n, k), k = 1, no_species), n = 1, 4)
endif

!close file at end of simulation
if (rday.eq.total_days + 1) then
    close (127)
!JW Apr 2017 ignores different species types - just assumes grass=1 and shrub=2
!output vegetation cover maps for grass
   filename = output_folder (1:output_folder_length) // 'grass_cover_new000.asc'
   if (iout.lt.10) then
      write (filename (18 + output_folder_length: 18 + output_folder_length), '(i1)') iout
   elseif (iout.lt.100) then
      write (filename (17 + output_folder_length: 18 + output_folder_length), '(i2)') iout
   elseif (iout.lt.1000) then
      write (filename (16 + output_folder_length: 18 + output_folder_length), '(i3)') iout
   endif
   open (99, file = filename, status = 'unknown')
   rewind (99)
   write (99, 9999) nc1
   write (99, 9998) nr1
   write (99, 9997) xmin
   write (99, 9996) ymin
   write (99, 9995) dx / 1.d3   ! converts back to m from mm
   write (99, 9994)
   do i = 1, nr1
      write (99, 9991) (c_veg (1, 1, i, j) * 100., j = 1, nc1)
   enddo
   close (99)

!output vegetation cover maps for shrub
   filename = output_folder (1:output_folder_length) // 'shrub_cover_new000.asc'
   if (iout.lt.10) then
      write (filename (18 + output_folder_length: 18 + output_folder_length), '(i1)') iout
   elseif (iout.lt.100) then
      write (filename (17 + output_folder_length: 18 + output_folder_length), '(i2)') iout
   elseif (iout.lt.1000) then
      write (filename (16 + output_folder_length: 18 + output_folder_length), '(i3)') iout
   endif
   open (99, file = filename, status = 'unknown')
   rewind (99)
   write (99, 9999) nc1
   write (99, 9998) nr1
   write (99, 9997) xmin
   write (99, 9996) ymin
   write (99, 9995) dx / 1.d3   ! converts back to m from mm
   write (99, 9994)
   do i = 1, nr1
      write (99, 9991) (c_veg (1, 2, i, j) * 100., j = 1, nc1)
   enddo
   close (99)
 !output net annual erosion of last simulation year
   filename = output_folder (1:output_folder_length) // 'net_erosion_season000.asc'
   if (iout.lt.10) then
      write (filename (21 + output_folder_length: 21 + output_folder_length), '(i1)') iout
   elseif (iout.lt.100) then
      write (filename (20 + output_folder_length: 21 + output_folder_length), '(i2)') iout
   elseif (iout.lt.1000) then
      write (filename (19 + output_folder_length: 21 + output_folder_length), '(i3)') iout
   endif
   open (99, file = filename, status = 'unknown')
   rewind (99)
   write (99, 9999) nc1
   write (99, 9998) nr1
   write (99, 9997) xmin
   write (99, 9996) ymin
   write (99, 9995) dx / 1.d3   ! converts back to m from mm
   write (99, 9994)
   do i = 1, nr1
      write (99, 9991) (net_erosion_season (i, j), j = 1, nc1)
   enddo
   close (99)
endif

9999   format ('ncols ', i6)
9998   format ('nrows ', i6)
9997   format ('xllcorner ', f15.8)
9996   format ('yllcorner ', f15.8)
9995   format ('cellsize ', f15.8)
9994   format ('nodata_value -9999')
9993   format (10000 (i6, 1x))
9992   format (10000 (a1, 1x))
9991   format (10000 (e10.4, 1x))

end



