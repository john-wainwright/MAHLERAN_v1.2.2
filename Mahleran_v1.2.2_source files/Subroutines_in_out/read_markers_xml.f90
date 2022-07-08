!****************************************************************
!  subroutine to read initial marker x,y coordinates
!
!  JC Dec 2011
!  JW modified to f90 format on change to xml input format May 2014
!
!****************************************************************

subroutine read_markers_xml

use shared_data
use parameters_from_xml

implicit none

character (len = 14) :: a_mnum
character (len = 160) :: filename
character (len = 30) :: a_line

integer :: i, k

filename = input_folder (1: input_folder_length) // marker_file
!    Open the *.asc file
open (3, file = filename, status = 'unknown')
rewind (3)

!    Read header of file
read (3, 10000) a_line
a_mnum = a_line (1:4)
read (a_line (5:30), *) mnum
write (6, *) ' File should have (', trim (a_mnum), '): ',  mnum, ' markers'

!    Allocate size of arrays

write (6, *) ' Going to allocate and initialize markers'
include 'allocat_markers.var'
call initialize_markers_xml
write (6, *) ' Back from allocating and initializing markers'

!    Read in data
do i = 1, mnum
    write (6, *) 'Reading marker: ', i
   read (3, *) (MXY (i, k), k = 1, 3)
enddo
close (3)

write (6, *)
write (6, *) ' Number of markers read in: ', mnum

9999   format (a6, i10)
10000  format (a30)

return
end

