c****************************************************************
c  subroutine to output data at the end of the marker-in-cell
c  simulation
c
c  JC May 2011
c
c****************************************************************

       subroutine output_markers

       use shared_data
       implicit double precision (a - h, o - z)
       implicit integer (i - n)

c      double precision marker_cell (nr1, nc1)
       integer *4 marker_cell (nr1, nc1)
       logical exists
       character *50 markerfileimg, markerofffile, MXYfinalfile

c      Define file directory and file names


c      Convert total number of markers from integer to real number
c      Needed for marker_cell
       m_num = dble (mnum)

c      Set up arrays

       do i = 1, nr1
        do j = 1, nc1
            marker_cell (i, j) = 0.0d0
        enddo
       enddo

c      Determine the number of markers in each cell at the end of the simulation
       do mi = 1, mnum
c       First check if the particle has been eroded off the slope
        if (MXY (mi, 2).gt.xmax - dx_m.or.MXY (mi, 2).lt.
     &     xmin + dx_m.or.MXY (mi, 1).gt.ymax - dx_m.or.
     &     MXY (mi, 1).lt.ymin + dx_m) then
        else
c           Determine which the cell the marker is in
            call marked_cell
c           Count the number of markers in each cell
            marker_cell (mycell, mxcell) =
c     &                 marker_cell (mycell, mxcell) + 1.0d0
     &              marker_cell (mycell, mxcell) + 1
        endif
       enddo

c      Convert marker_cell to percentage of markers in each cell
c      do i = 2, nr1 - 1
c       do j = 2, nc1 - 1
c           if (marker_cell (i, j).gt.0.0d0) then
c               marker_cell (i, j) = marker_cell (i, j) / m_num
c           else
c               marker_cell (i, j) = 0.0d0
c           endif
c       enddo
c      enddo

c      Define file directory and file names
       markerfileimg = pfadout(1:pfado)//'marker_map001.asc'
       markerofffile = pfadout(1:pfado)//'marker_runoff001.dat'
       MXYfinalfile = pfadout(1:pfado)//'MXY_final001.dat'

c      Open files for output based on value of iout calculated at start of run
       if (iout.lt.10) then
        write (markerfileimg (13+pfado:13+pfado), '(i1)') iout
          write (markerofffile (16+pfado:16+pfado), '(i1)') iout
        write (MXYfinalfile (12+pfado:12+pfado), '(i1)') iout
       elseif (iout.lt.100) then
          write (markerfileimg (12+pfado:13+pfado), '(i2)') iout
          write (markerofffile (15+pfado:16+pfado), '(i2)') iout
        write (MXYfinalfile (11+pfado:12+pfado), '(i2)') iout
       elseif (iout.lt.1000) then
          write (markerfileimg (11+pfado:13+pfado), '(i3)') iout
          write (markerofffile (14+pfado:16+pfado), '(i3)') iout
        write (MXYfinalfile (10+pfado:12+pfado), '(i3)') iout
       endif

c      Check that this needs to be an ouput
       if (f_markerfileimg) open (121, file = markerfileimg,
     &                    status = 'unknown')
       if (f_markerofffile) open (122, file = markerofffile,
     &                    status = 'unknown')
       if (f_MXYfinalfile) open (123, file = MXYfinalfile,
     &                  status = 'unknown')

c      Mask out values beyond the edges of the simulation
       do i = 1, nr1
          do k = 1, nc1
             if (rmask (i, k).lt.0.0d0) then
c                marker_cell (i, k) = 0.0d0
            marker_cell (i, k) = 0
             endif
          enddo
       enddo

c      Write header lines

       inquire (UNIT=121,OPENED=exists)
       if (exists) then
          write (121, 9999) nc1
          write (121, 9998) nr1
          write (121, 9997) xmin
          write (121, 9996) ymin
          write (121, 9995) dx_m
          write (121, 9994)
       endif

c      Write .asc files
       do i = 1, nr1
       if (f_markerfileimg) write (121, 9993) (marker_cell (i, k),
     &                                     k = 1, nc1)
       enddo
       do iter = 1, nit
       if (f_markerofffile) write (122, '(2(e10.4, 1x))')
     &  (marker_runoff (iter, i), i = 1, 2)
       enddo
       if (f_MXYfinalfile) then
        write (123, '(3(e10.4, 1x))') ((MXY (j, i), i = 1, 3),
     &        j = 1, mnum)
       endif

c      Close file units for use next time
       do i = 121, 123
          close (i)
       enddo

9999   format ('ncols ', i6)
9998   format ('nrows ', i6)
9997   format ('xllcorner ', f15.8)
9996   format ('yllcorner ', f15.8)
9995   format ('cellsize ', f15.8)
9994   format ('nodata_value -9999')
c 9993   format (10000 (e10.4, 1x))
9993   format (10000 (i6))
9992   format (10000 (e10.4,1x))

       return
       end
