c****************************************************************
c  subroutine to output markers x,y coordinates and status during
c  the simulation, as well as maps of flow depth
c
c  JC May 2011
c
c****************************************************************

       subroutine output_markers_xy

       use shared_data
       implicit double precision (a - h, o - z)
       implicit integer (i - n)

       logical exists

c      Define file directory and file names
       MXYfile = pfadout(1:pfado)//'MXY0000.dat'
       dmapfile = pfadout(1:pfado)//'depth0000.asc'
       mstatusfile = pfadout(1:pfado)//'mstatus0000.dat'

c      Open files for output based on timestep iter
       if (iter.lt.10) then
          write (MXYfile (7+pfado:7+pfado), '(i1)') iter
        write (dmapfile (9+pfado:9+pfado), '(i1)') iter
        write (mstatusfile (11+pfado:11+pfado), '(i1)') iter
       elseif (iter.lt.100) then
          write (MXYfile (6+pfado:7+pfado), '(i2)') iter
        write (dmapfile (8+pfado:9+pfado), '(i2)') iter
        write (mstatusfile (10+pfado:11+pfado), '(i2)') iter
       elseif (iter.lt.1000) then
          write (MXYfile (5+pfado:7+pfado), '(i3)') iter
        write (dmapfile (7+pfado:9+pfado), '(i3)') iter
        write (mstatusfile (9+pfado:11+pfado), '(i3)') iter
       elseif (iter.lt.10000) then
          write (MXYfile (4+pfado:7+pfado), '(i4)') iter
        write (dmapfile (6+pfado:9+pfado), '(i4)') iter
        write (mstatusfile (8+pfado:11+pfado), '(i4)') iter
       endif

       if (f_MXYfile) open (124, file = MXYfile, status = 'unknown')
       if (f_dmapfile) then
c      Only create .asc file for every 30 iterations
c       do j = 1, nit / 30.0d0
c           if (iter.eq.30.0d0 * j) then
                open (125, file = dmapfile, status = 'unknown')
c           endif
c       enddo
       endif
       if (f_mstatusfile) then
             open (126, file = mstatusfile, status = 'unknown')
         endif

       inquire (UNIT=125, OPENED=exists)
       if (exists) then
        write (125, 9999) nc1
          write (125, 9998) nr1
          write (125, 9997) xmin
          write (125, 9996) ymin
          write (125, 9995) dx_m
          write (125, 9994)
       endif

c      Write .asc files
       if (f_MXYfile) then
        write (124, '(3(e10.4, 1x))') ((MXY (j, i), i = 1, 3),
     &        j = 1, mnum)
       endif
       do i = 1, nr1
       if (f_dmapfile) write (125, 9993) (d (2, i, k),
     &               k = 1, nc1)
       enddo
       if (f_mstatusfile) then
        write (126, '(6(e10.4, 1x))') ((marker_status (j, i),
     &        i = 1, 6), j = 1, mnum)
       endif

c      Close file units for use next time
       do i = 124, 126
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
