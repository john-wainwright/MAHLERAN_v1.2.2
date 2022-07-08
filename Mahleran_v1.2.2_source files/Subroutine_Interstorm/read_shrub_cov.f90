!****************************************************************
!subroutine to read shrub cover
!
!
!***************************************************************
subroutine read_shrub_cov

!
!       ncols [x]
!       nrows [y]
!       xllcorner [xmin]
!       yllcorner [ymin]
!       cellsize [dx]
!       nodata_value [nodata]
!
!  followed by row 1 etc. of the data

use shared_data
use interstorm_shared_data
use parameters_from_xml

implicit none



open (4, FILE=pfadin_interstorm(1:pfadi_interstorm)//shrub_cov_file , status = 'unknown')
rewind (4)

!
!   read header
!
read (4, 9999) a_col, ncol1
read (4, 9999) a_row, nrw1
read (4, 9998) a_minx, xmin1
read (4, 9998) a_miny, ymin1
read (4, 9997) a_dx, dx1
read (4, 9996)
dx1 = dx1 * 1000.0
if (ncol1.ne.nc1.or.nrw1.ne.nr1.or.xmin1.ne.xmin.or.ymin1.ne.ymin.or.dx1.ne.dx) then
    write (6, *) ' Warning -- shrub cover file header in ', &
    shrub_cov_file, ' does not match topography file ','header in ', topo_file
    endif

 do i = 1, nr1
     read (4, *) (shrub_cover (i, k), k = 1, nc1)
 enddo

 close (4)

write (6, *)
write (6, *) ' shrub cover data read in '

9999   format (a6, i10)
9998   format (a10, f15.8)
9997   format (a9, f15.8)
9996   format (a13)

       return
       end
