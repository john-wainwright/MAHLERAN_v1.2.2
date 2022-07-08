subroutine read_spatial_data (filename, data_array, n_cols, n_rows, xllcorner, yllcorner, cellsize, nodata_value)
!     
! File:   read_spatial_data.for
! Author: John Wainwright
!
! Created on 21 February 2014, 12:06
!
! General input of .asc files to replace the individual files currently set up for each dataset
!
! Parameters of subroutine:
!   filename:     name of input file (full path and with .asc extension included)
!   data_array:   array of [n_cols, n_rows] double precision values read in from the file specified
!   n_cols:        output of number of columns in the .asc file read in (equivalent to x direction in the model)
!   n_rows:        output of number of rows in the .asc file read in (equivalent to y direction in the model)
!   xllcorner:    output of the x coordinate [xmin] of the lower left-hand corner of the data
!   yllcorner:    output of the y coordinate [ymin] of the lower left-hand corner of the data
!   cellsize:     output of the cell size of the data [dx]
!   nodata_value: output of the value used to specify no data in the input file 
!
use parameters_from_xml

implicit none

double precision, allocatable, intent(inout) :: data_array (:,:)
double precision, intent (out) :: xllcorner, yllcorner, cellsize
double precision, intent (out) :: nodata_value
       
integer, intent (out) :: n_cols
integer, intent (out) :: n_rows
integer :: i, k
       
logical :: fexist
       
character (len=*) :: filename
character *14 :: a_col, a_row, a_dx, a_minx, a_miny, a_nodata
character *80 :: a_line

fexist = .FALSE.
!
!     check if filename passed to the subroutine exists and throw and error if not
!
inquire (file = filename, exist = fexist, err = 1)

1   continue
if (.not.fexist) then
    write (6, *) ' Error in read_spatial_data when trying to ', &
                        'open file: ', filename
    write (6, *) ' Check that the file is present in the ', &
                        'Input folder and is not open in another program'
    stop
endif
       
open (3, FILE=filename, status = 'old')
rewind (3)
write (6, *) 'File opened for reading: ', filename
!
!   read header
!
read (3, 10000) a_line
a_col = a_line (1:5)
read (a_line (6:80), *) i ! n_cols
read (3, 10000) a_line
a_row = a_line (1:5)
read (a_line (6:80), *) k  !n_rows
read (3, 10000) a_line
a_minx = a_line (1:9)
read (a_line (10:80), *) xllcorner
read (3, 10000) a_line
a_miny = a_line (1:9)
read (a_line (10:80), *) yllcorner
read (3, 10000) a_line
a_dx = a_line (1:8)
read (a_line (9:80), *) cellsize
read (3, 10000) a_line
a_nodata = a_line (1:12)
read (a_line (13:80), *) nodata_value
       
n_cols = i !for some reason, reading straight into n_rows creates
n_rows = k !an error, so read into dummy variables first
write (6, 9999) a_col, n_cols
write (6, 9999) a_row, n_rows
write (6, 9998) a_minx, xllcorner
write (6, 9998) a_miny, yllcorner
write (6, 9997) a_dx, cellsize
write (6, 9996) a_nodata, nodata_value

!       if (allocated (data_array)) then
!           write (6, *) size (data_array, 1), size (data_array, 2)
!           allocate (temp (size (data_array, 1), size (data_array, 2)))
!       else
!          allocate (temp (n_cols, n_rows))
!          write (6, *) ' Allocated temp'
!          write (6, *) ' Error with variable allocated for data ',
!     &                 'from file: ', filename
!       endif
write (6, *) 'data_array size: ', size (data_array, 1), size (data_array, 2)
if (.not.allocated (data_array)) then
    write (6, *) ' Allocating new space for data '
    allocate (data_array (n_rows, n_cols))
    write (6, *) n_rows, n_cols
else
    if (size (data_array, 1).ne.n_rows.and.size (data_array, 2).ne.n_cols) then
        write (6, *) ' Error reading in file: ', filename
        write (6, *) ' File has ', n_cols, ' columns and ', n_rows, &
                          ' rows,\n compared to already allocated ', &
                          ' variable which has ', size (data_array, 1), &
                          ' columns and ', size (data_array, 2), &
                          ' rows'
        stop
    endif
    write (6, *) ' Space already allocated for data '
endif
          
do i = 1, n_rows
   read (3, *) (data_array (i, k), k = 1, n_cols) ! implied loop has to be used as 
                                                        ! otherwise only 1st value would be read in
!   write (6, 9995) (data_array (i, k), k = 1, n_cols)
enddo
close (3)

!
!  added JW Feb 2017 to allow output to screen to be turned on/off
!
if (verbose_output) then
   write (6, *) ' Data read in from file: ', filename
   write (6, *) n_rows, n_cols
   do i = 1, n_rows
      write (6, 9995) (data_array (i, k), k = 1, n_cols)
   enddo
endif
!n_cols = n_cols + 1 !return actual size of data_array with edge cells
!n_rows = n_rows + 1 !

10000  format (a80)
9999   format (a6, i10)
!9998   format (a10, f15.8)
!9997   format (a9, f15.8)
!9996   format (a13, f15.8)
!9999   format (a14, i6)
9998   format (a14, f15.8)
9997   format (a14, f15.8)
9996   format (a14, f15.8)
9995   format (1000 (f9.2, 1x))

return
end subroutine

subroutine read_int_spatial_data (filename, int_data_array, n_cols, n_rows, xllcorner, yllcorner, cellsize, nodata_value)
!     
! File:   read_int_spatial_data.for
! Author: John Wainwright
!
! Created on 21 February 2014, 12:06
!
! General input of .asc files to replace the individual files currently set up for each dataset
!
! Parameters of subroutine:
!   filename:     name of input file (full path and with .asc extension included)
!   int_data_array:   array of [n_cols, n_rows] integer values read in from the file specified
!   n_cols:        output of number of columns in the .asc file read in (equivalent to x direction in the model)
!   n_rows:        output of number of rows in the .asc file read in (equivalent to y direction in the model)
!   xllcorner:    output of the x coordinate [xmin] of the lower left-hand corner of the data
!   yllcorner:    output of the y coordinate [ymin] of the lower left-hand corner of the data
!   cellsize:     output of the cell size of the data [dx]
!   nodata_value: output of the value used to specify no data in the input file 
!

use parameters_from_xml

implicit none

integer *4, allocatable, intent(inout) :: int_data_array (:,:)
double precision, intent (out) :: xllcorner, yllcorner, cellsize
double precision, intent (out) :: nodata_value
       
integer, intent (out) :: n_cols
integer, intent (out) :: n_rows
integer i, k
       
logical fexist
       
character (len=*) :: filename
character *14 a_col, a_row, a_dx, a_minx, a_miny, a_nodata
character *80 a_line

fexist = .FALSE.
!
!     check if filename passed to the subroutine exists and throw and error if not
!
inquire (file = filename, exist = fexist, err = 1)

1   continue
if (.not.fexist) then
    write (6, *) ' Error in read_spatial_data when trying to ', &
                        'open file: ', filename
    write (6, *) ' Check that the file is present in the ', &
                        'Input folder and is not open in another program'
    stop
endif
       
open (3, FILE=filename, status = 'old')
rewind (3)
write (6, *) 'File opened for reading: ', filename
!
!   read header
!
read (3, 10000) a_line
a_col = a_line (1:5)
read (a_line (6:80), *) i ! n_cols
read (3, 10000) a_line
a_row = a_line (1:5)
read (a_line (6:80), *) k  !n_rows
read (3, 10000) a_line
a_minx = a_line (1:9)
read (a_line (10:80), *) xllcorner
read (3, 10000) a_line
a_miny = a_line (1:9)
read (a_line (10:80), *) yllcorner
read (3, 10000) a_line
a_dx = a_line (1:8)
read (a_line (9:80), *) cellsize
read (3, 10000) a_line
a_nodata = a_line (1:12)
read (a_line (13:80), *) nodata_value
       
n_cols = i !for some reason, reading straight into n_rows creates
n_rows = k !an error, so read into dummy variables first
write (6, 9999) a_col, n_cols
write (6, 9999) a_row, n_rows
write (6, 9998) a_minx, xllcorner
write (6, 9998) a_miny, yllcorner
write (6, 9997) a_dx, cellsize
write (6, 9996) a_nodata, nodata_value

!       if (allocated (data_array)) then
!           write (6, *) size (data_array, 1), size (data_array, 2)
!           allocate (temp (size (data_array, 1), size (data_array, 2)))
!       else
!          allocate (temp (n_cols, n_rows))
!          write (6, *) ' Allocated temp'
!          write (6, *) ' Error with variable allocated for data ',
!     &                 'from file: ', filename
!       endif
write (6, *) 'data_array size: ', size (int_data_array, 1), size (int_data_array, 2)
if (.not.allocated (int_data_array)) then
    write (6, *) ' Allocating new space for data '
    allocate (int_data_array (n_rows, n_cols))
else
    if (size (int_data_array, 1).ne.n_rows.and.size (int_data_array, 2).ne.n_cols) then
        write (6, *) ' Error reading in file: ', filename
        write (6, *) ' File has ', n_cols, ' columns and ', n_rows, &
                          ' rows,\n compared to already allocated ', &
                          ' variable which has ', size (int_data_array, 1), &
                          ' columns and ', size (int_data_array, 2), &
                          ' rows'
        stop
    endif
    write (6, *) ' Space already allocated for data '
endif
          
do i = 1, n_rows
   read (3, *) (int_data_array (i, k), k = 1, n_cols) ! implied loop has to be used as 
                                                      ! otherwise only 1st value would be read in
enddo
close (3)

!
!  added JW Feb 2017 to allow output to screen to be turned on/off
!
if (verbose_output) then
   write (6, *) ' Data read in from file: ', filename
   write (6, *) n_rows, n_cols
   do i = 1, n_rows
       write (6, 9994) (int_data_array (i, k), k = 1, n_cols)
   enddo
   !n_cols = n_cols + 1 !return actual size of data_array with edge cells
   !n_rows = n_rows + 1 !
endif

10000  format (a80)
9999   format (a6, i10)
!9998   format (a10, f15.8)
!9997   format (a9, f15.8)
!9996   format (a13, f15.8)
!9999   format (a14, i6)
9998   format (a14, f15.8)
9997   format (a14, f15.8)
9996   format (a14, f15.8)
9995   format (1000 (f9.2, 1x))
9994   format (1000 (i5, 1x))

return
end subroutine