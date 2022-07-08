!****************************************************************
!  subroutine to determine which cell the marker is within
!
!  JC May 2011
!
!  JW May 2014 modified to account for xml input changes
!
!****************************************************************

subroutine marked_cell_xml

use shared_data
use parameters_from_xml

implicit none

integer :: i, j

!JW not sure what these lines are doing -- will stop model run, so don't seem helpful!
!write (6, *) xmax - dx_m
!write (6, *) xmin + dx_m
!write (6, *) ymax - dx_m
!write (6, *) ymin + dx_m
!   stop

do i = 2, nr1 - 1
!    ((i - 1) * dx_m) + ymin is the y coordinate of the upper left node of the cell
   if (MXY (mi, 1).ge.((i - 1) * dx_m) + ymin.and.MXY (mi, 1).lt.(i * dx_m) + ymin) then
      mycell = i
      exit
   endif
enddo

do j = 2, nc1 - 1
!    ((j - 1) * dx_m) + xmin is the x coordinate of the upper left node of the cell
   if (MXY (mi, 2).ge.((j - 1) * dx_m) + xmin.and.MXY (mi, 2).lt.(j * dx_m) + xmin) then
      mxcell = j
      exit
   endif
enddo

return
end
