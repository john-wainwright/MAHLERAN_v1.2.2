!****************************************************************
!  subroutine to output data at the end of the marker-in-cell
!  simulation
!
!  JC May 2011
!  JW May 2014 modified to account for xml input changes
!
!****************************************************************

subroutine output_markers_xml

use shared_data
use parameters_from_xml

implicit none

!    double precision marker_cell (nr1, nc1)
integer *4 :: marker_cell (nr1, nc1)
integer :: i, k

double precision :: m_num

logical :: exists

character (len = 1800) markerfileimg, markerofffile, MXYfinalfile

!    Define file directory and file names


!    Convert total number of markers from integer to real number
!    Needed for marker_cell
m_num = dble (mnum)

!    Set up arrays

do i = 1, nr1
   do k = 1, nc1
      marker_cell (i, k) = 0.0d0
   enddo
enddo

!    Determine the number of markers in each cell at the end of the simulation
do mi = 1, mnum
!       First check if the particle has been eroded off the slope
   if (MXY (mi, 2).gt.xmax - dx_m.or.MXY (mi, 2).lt.xmin + dx_m.or.MXY (mi, 1).gt.ymax - dx_m.or.MXY (mi, 1).lt.ymin + dx_m) then

   else
!           Determine which the cell the marker is in
      call marked_cell_xml
!           Count the number of markers in each cell
      marker_cell (mycell, mxcell) =  marker_cell (mycell, mxcell) + 1
   endif
enddo

!    Convert marker_cell to percentage of markers in each cell
!    do i = 2, nr1 - 1
!       do j = 2, nc1 - 1
!           if (marker_cell (i, j).gt.0.0d0) then
!               marker_cell (i, j) = marker_cell (i, j) / m_num
!           else
!               marker_cell (i, j) = 0.0d0
!           endif
!       enddo
!    enddo

!    Define file directory and file names
markerfileimg = output_folder (1: output_folder_length) // 'marker_map001.asc'
markerofffile = output_folder (1: output_folder_length) // 'marker_runoff001.dat'
MXYfinalfile = output_folder (1: output_folder_length) // 'MXY_final001.dat'

!    Open files for output based on value of iout calculated at start of run
if (iout.lt.10) then
   write (markerfileimg (13 + output_folder_length:13 + output_folder_length), '(i1)') iout
   write (markerofffile (16 + output_folder_length:16 + output_folder_length), '(i1)') iout
   write (MXYfinalfile (12 + output_folder_length:12 + output_folder_length), '(i1)') iout
elseif (iout.lt.100) then
   write (markerfileimg (12 + output_folder_length:13 + output_folder_length), '(i2)') iout
   write (markerofffile (15 + output_folder_length:16 + output_folder_length), '(i2)') iout
   write (MXYfinalfile (11 + output_folder_length:12 + output_folder_length), '(i2)') iout
elseif (iout.lt.1000) then
   write (markerfileimg (11 + output_folder_length:13 + output_folder_length), '(i3)') iout
   write (markerofffile (14 + output_folder_length:16 + output_folder_length), '(i3)') iout
   write (MXYfinalfile (10 + output_folder_length:12 + output_folder_length), '(i3)') iout
endif

!    Check that this needs to be an ouput
if (f_markerfileimg) then
   open (121, file = markerfileimg, status = 'unknown')
endif
if (f_markerofffile) then
   open (122, file = markerofffile, status = 'unknown')
endif
if (f_MXYfinalfile) then
   open (123, file = MXYfinalfile, status = 'unknown')
endif

!    Mask out values beyond the edges of the simulation
do i = 1, nr1
   do k = 1, nc1
      if (rmask (i, k).lt.0.0d0) then
!                marker_cell (i, k) = 0.0d0
         marker_cell (i, k) = 0
      endif
   enddo
enddo

!    Write header lines

inquire (UNIT = 121, OPENED = exists)
if (exists) then
   write (121, 9999) nc1
   write (121, 9998) nr1
   write (121, 9997) xmin
   write (121, 9996) ymin
   write (121, 9995) dx_m
   write (121, 9994)
endif

!    Write .asc files
if (f_markerfileimg) then
   do i = 1, nr1
      write (121, 9993) (marker_cell (i, k), k = 1, nc1)
   enddo
endif
if (f_markerofffile) then
   do iter = 1, nit
      write (122, '(2(e10.4, 1x))') (marker_runoff (iter, i), i = 1, 2)
   enddo
endif
if (f_MXYfinalfile) then
   write (123, '(3(e10.4, 1x))') ((MXY (k, i), i = 1, 3), k = 1, mnum)
endif

!    Close file units for use next time
do i = 121, 123
   close (i)
enddo

9999   format ('ncols ', i6)
9998   format ('nrows ', i6)
9997   format ('xllcorner ', f15.8)
9996   format ('yllcorner ', f15.8)
9995   format ('cellsize ', f15.8)
9994   format ('nodata_value -9999')
! 9993   format (10000 (e10.4, 1x))
9993   format (10000 (i6))
9992   format (10000 (e10.4,1x))

return
end
