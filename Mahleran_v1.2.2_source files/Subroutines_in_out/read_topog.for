c****************************************************************
c  subroutine to read surface topography for simulation
c****************************************************************
       subroutine read_topog
c
cJWMay 2005 now uses .asc format which uses single input file with following header lines:
c
c       ncols [x]
c       nrows [y]
c       xllcorner [xmin]
c       yllcorner [ymin]
c       cellsize [dx]
c       nodata_value [nodata]
c
c  followed by row 1 etc. of the data
cJWMay 2005

       use shared_data
       implicit double precision (a - h, o - z)
       implicit integer (i - n)

       character *14 a_col, a_row, a_dx, a_miny, a_minx

c
c opens the *.asc file of the dem
c
       write(*,*) pfadin(1:pfadi), topo_file
       open (3, FILE=pfadin(1:pfadi)//topo_file, status = 'unknown')
       rewind (3)
         write (6, *) ' file opened '

c
c   read header
c
       read (3, 9999) a_col, ncol
       read (3, 9999) a_row, nrw
       read (3, 9998) a_minx, xmin
       read (3, 9998) a_miny, ymin
       read (3, 9997) a_dx, dx
       read (3, 9996)
c
c   *1000 converts from m to mm
c
       xmax = xmin + dx * ncol
       ymax = ymin + dx * nrw
       dx_m = dx
       dx = dx * 1000.0d0
       dy = dx

       nc = ncol - 1
       nr = nrw - 1
       nc1 = ncol
       nr1 = nrw
       nc2 = ncol + 1
       nr2 = nrw + 1
       ncell = nr1 * nc1

c
c   use information from header file to allocate space for model variables
c
         write (6, *) ' about to allocate '
       include 'allocat.var'
         write (6, *) ' done allocate '
       call initialize_allocatables
         write (6, *) ' allocatables initialized '
c
c   read in data
c
       do i = 1, nr1
          read (3, *) (z (i, k), k = 1, nc1) ! implied loop has to be used as
c otherwise only 1st value would be read in
c   convert to mm
c
          do k = 1, nc1
             z (i, k) = z (i, k) * 1000.
          enddo
       enddo
       close (3)

       write (6, *) ' Topographic data read in '

9999   format (a6, i10)
9998   format (a10, f15.8)
9997   format (a9, f15.8)
9996   format (a13)

       return
       end
