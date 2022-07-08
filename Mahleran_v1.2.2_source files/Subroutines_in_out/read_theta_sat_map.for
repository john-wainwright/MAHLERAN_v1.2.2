*****************************************************************
c  subroutine to read in theta_sat_map
c
c
c LTOct2007
c
*****************************************************************
      subroutine read_theta_sat_map
c
c uses .asc format which uses single input file with following header lines:
c
c       ncols [x]
c       nrows [y]
c       xllcorner [xmin]
c       yllcorner [ymin]
c       cellsize [dx]
c       nodata_value [nodata]
c
c  followed by row 1 etc. of the data
c
       use shared_data
       implicit double precision (a - h, o - z)
       implicit integer (i - n)

       character *14 a_col, a_row, a_dx, a_miny, a_minx

c
c opens the *.asc file of the theta_sat map
c


       open (3, FILE=pfadin(1:pfadi) // theta_sat_file,
     &         status = 'unknown')
         rewind (3)


c   read header
c
       read (3, 9999) a_col, ncol1
       read (3, 9999) a_row, nrw1
       read (3, 9998) a_minx, xmin1
       read (3, 9998) a_miny, ymin1
       read (3, 9997) a_dx, dx1
       read (3, 9996)
       dx1 = dx1 * 1000.0
       if (ncol1.ne.nc1.or.nrw1.ne.nr1.or.xmin1.ne.xmin.or.
     &     ymin1.ne.ymin.or.dx1.ne.dx) then
          write (6, *) ' Warning -- theta_sat file header in ',
     &              theta_sat_file, ' does not match topography file ',
     &                 'header in ', topo_file
       endif
cLTOct2007  Added theta_sat into shared_data.f90
       do i = 1, nr1
          read (3, *) (theta_sat (i, k), k = 1, nc1)
       enddo
       close (3)

       write (6, *)
       write (6, *) ' theta_sat data read in '

9999   format (a6, i10)
9998   format (a10, f15.8)
9997   format (a9, f15.8)
9996   format (a13)

       return
       end
