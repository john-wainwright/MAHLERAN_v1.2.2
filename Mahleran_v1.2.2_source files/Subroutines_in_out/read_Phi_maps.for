c****************************************************************
c  subroutine to read in proportion of soil in each phi class
c
c  initial vales are calculated using Scoging et al., 1992
c  relationship between stone cover and inf. rate to
c  parameterise Green and Ampt equation
c
c  REB 02/05/02
c
c****************************************************************
       subroutine read_PHI_maps
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
c opens the *.asc file of the PHI maps
c goes through each phi class in turn


       open (3, FILE=pfadin(1:pfadi)// phi_1_file, status = 'unknown')
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
          write (6, *) ' Warning -- soil moisture file header in ',
     &              phi_1_file, ' does not match topography file ',
     &                 'header in ', topo_file
       endif

cLTOct2007  Added theta_0 into shared_data.f90 since the other spatially distributed parameters that are read in feature here.
cLTDEC2007 - reading otno the arry like this isn;t working -  need to read in value to a dummy name fpr ohi = 1, for i,j, then allocatate to phi,i,j.


       do i = 1, nr1
            read (3, *) (ps_init_ave (1, i, j), j = 1, nc1)
       enddo
       close (3)



       write (6, *)
       write (6, *) ' ps(1) read in '

c      do i = 1, nr1
c       do j = 1, nc1
c           ps_init_ave (1, i, j) = ps_init_1 (i, j)
c       enddo
c      enddo


9999   format (a6, i10)
9998   format (a10, f15.8)
9997   format (a9, f15.8)
9996   format (a13)

       open (3, FILE=pfadin(1:pfadi)// phi_2_file, status = 'unknown')
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
          write (6, *) ' Warning -- soil moisture file header in ',
     &              phi_2_file, ' does not match topography file ',
     &                 'header in ', topo_file
       endif

cLTOct2007  Added theta_0 into shared_data.f90 since the other spatially distributed parameters that are read in feature here.
       do i = 1, nr1
          read (3, *) (ps_init_ave (2, i, k), k = 1, nc1)
       enddo
       close (3)

       write (6, *)
       write (6, *) ' ps(2) read in '

       open (3, FILE=pfadin(1:pfadi)//phi_3_file, status = 'unknown')
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
          write (6, *) ' Warning -- soil moisture file header in ',
     &              phi_3_file, ' does not match topography file ',
     &                 'header in ', topo_file
       endif

cLTOct2007  Added theta_0 into shared_data.f90 since the other spatially distributed parameters that are read in feature here.
       do i = 1, nr1
          read (3, *) (ps_init_ave (3, i, k), k = 1, nc1)
       enddo
       close (3)

       write (6, *)
       write (6, *) ' ps(3) read in '

       open (3, FILE=pfadin(1:pfadi)// phi_4_file, status = 'unknown')
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
          write (6, *) ' Warning -- soil moisture file header in ',
     &              phi_4_file, ' does not match topography file ',
     &                 'header in ', topo_file
       endif

cLTOct2007  Added theta_0 into shared_data.f90 since the other spatially distributed parameters that are read in feature here.
       do i = 1, nr1
          read (3, *) (ps_init_ave (4, i, k), k = 1, nc1)
       enddo
       close (3)

       write (6, *)
       write (6, *) ' ps(4) read in '

       open (3, FILE=pfadin(1:pfadi)// phi_5_file, status = 'unknown')
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
          write (6, *) ' Warning -- soil moisture file header in ',
     &              phi_5_file, ' does not match topography file ',
     &                 'header in ', topo_file
       endif

cLTOct2007  Added theta_0 into shared_data.f90 since the other spatially distributed parameters that are read in feature here.
       do i = 1, nr1
          read (3, *) (ps_init_ave (5, i, k), k = 1, nc1)
       enddo
       close (3)

       write (6, *)
       write (6, *) ' ps(5) read in '

       open (3, FILE=pfadin(1:pfadi)// phi_6_file, status = 'unknown')
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
          write (6, *) ' Warning -- soil moisture file header in ',
     &              phi_6_file, ' does not match topography file ',
     &                 'header in ', topo_file
       endif

cLTOct2007  Added theta_0 into shared_data.f90 since the other spatially distributed parameters that are read in feature here.
       do i = 1, nr1
          read (3, *) (ps_init_ave (6, i, k), k = 1, nc1)
       enddo
       close (3)

       write (6, *)
       write (6, *) ' particle sizes (6) read in '

       return
       end
