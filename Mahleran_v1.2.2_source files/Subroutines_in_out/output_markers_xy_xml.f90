!****************************************************************
!  subroutine to output markers x,y coordinates and status during
!  the simulation, as well as maps of flow depth
!
!  JC May 2011
!  JW May 2014 modified to account for xml input changes
!  JW May 2017 modified units to 2xx from 1xx to avoid clashes with continuous output
!
!****************************************************************

subroutine output_markers_xy_xml

use shared_data
use parameters_from_xml

implicit none

integer :: i, j, k

logical exists

!    Define file directory and file names
MXYfile = output_folder (1: output_folder_length) // 'MXY0000.dat'
dmapfile = output_folder (1: output_folder_length) // 'depth0000.asc'
mstatusfile = output_folder (1: output_folder_length) // 'mstatus0000.dat'

!    Open files for output based on timestep iter
if (iter.lt.10) then
   write (MXYfile (7 + output_folder_length: 7 + output_folder_length), '(i1)') iter
   write (dmapfile (9 + output_folder_length: 9 + output_folder_length), '(i1)') iter
   write (mstatusfile (11 + output_folder_length: 11 + output_folder_length), '(i1)') iter
elseif (iter.lt.100) then
   write (MXYfile (6 + output_folder_length: 7 + output_folder_length), '(i2)') iter
   write (dmapfile (8 + output_folder_length: 9 + output_folder_length), '(i2)') iter
   write (mstatusfile (10 + output_folder_length: 11 + output_folder_length), '(i2)') iter
elseif (iter.lt.1000) then
   write (MXYfile (5 + output_folder_length: 7 + output_folder_length), '(i3)') iter
   write (dmapfile (7 + output_folder_length: 9 + output_folder_length), '(i3)') iter
   write (mstatusfile (9 + output_folder_length: 11 + output_folder_length), '(i3)') iter
elseif (iter.lt.10000) then
   write (MXYfile (4 + output_folder_length: 7 + output_folder_length), '(i4)') iter
   write (dmapfile (6 + output_folder_length: 9 + output_folder_length), '(i4)') iter
   write (mstatusfile (8 + output_folder_length: 11 + output_folder_length), '(i4)') iter
endif

if (f_MXYfile) then
   open (224, file = MXYfile, status = 'unknown')
endif
if (f_dmapfile) then
!    Only create .asc file for every 30 iterations
!       do j = 1, nit / 30.0d0
!           if (iter.eq.30.0d0 * j) then
   open (225, file = dmapfile, status = 'unknown')
!           endif
!       enddo
endif
if (f_mstatusfile) then
   open (226, file = mstatusfile, status = 'unknown')
endif

inquire (unit = 225, opened = exists)
if (exists) then
   write (225, 9999) nc1
   write (225, 9998) nr1
   write (225, 9997) xmin
   write (225, 9996) ymin
   write (225, 9995) dx_m
   write (225, 9994)
endif

!    Write .asc files
if (f_MXYfile) then
   write (224, '(3(e10.4, 1x))') ((MXY (j, i), i = 1, 3), j = 1, mnum)
endif
if (f_dmapfile) then
   do i = 1, nr1
      write (225, 9993) (d (2, i, k), k = 1, nc1)
   enddo
endif
if (f_mstatusfile) then
   write (226, '(6(e10.4, 1x))') ((marker_status (j, i), i = 1, 6), j = 1, mnum)
endif

!    Close file units for use next time
do i = 224, 226
   close (i)
enddo

9999   format ('ncols ', i6)
9998   format ('nrows ', i6)
9997   format ('xllcorner ', f15.8)
9996   format ('yllcorner ', f15.8)
9995   format ('cellsize ', f15.8)
9994   format ('nodata_value -9999')
9993   format (10000 (e10.4, 1x))
9992   format (10000 (i1, 1x))

return
end
