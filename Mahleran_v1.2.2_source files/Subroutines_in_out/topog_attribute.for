cJWMar2017
c WARNING -- not sure why this subroutine largely duplicates topog_attrib.for yet doesn't
c            seem to be called in the present version of the model.  More recent changes have
c            been made to topog_attrib for this reason, so ignore this subroutine for the moment.
c
c
c ************************************************************
c  subroutine to define topographic attributes
c ************************************************************
       subroutine topog_attribute
       use shared_data
       use parameters_from_xml
       implicit double precision (a - h, o - z)
       implicit integer (i - n)
c
c      local variables
       integer *4 sdir (4, 2)
       data sdir /-1, 0, 1, 0, 0, 1, 0, -1 /
c      sdir() contains four vectors in the N[-1,0], E[0,1],
c      S[1,0] and W[0,-1] directions
c
       write(6, *) 'In topographic attribute routine'
       write(6, *) 'sdir'
       do n = 1, 4
           write (6, 9990) ( sdir (n, j), j = 1, 2 )
       enddo
c
c      define aspect and slope
c
       do i = 2, nr
          do k = 2, nc
             aspect (i, k) = 0
c
cCJMH        Aspect here refers to flow direction which is assigned values
cCJMH        between 1 and 4 (flow occurs in the four cardinal cells)
c
             if (ndirn.eq.4) then
c
c                flow routed along four cardinal directions
c                (1='N', 2='E', 3='S', 4='W')
c
cJWJun05         zmin1 is lowest of neighbouring cells for dynamic overtopping
cJWJun05         algorithm, zmin includes centre cell
c
                 zmin = z (i, k)
               zmin1 = 1.d36

c                do for each cardinal direction
                 do j = 1, ndirn

c                    the following if ... then block determines which cell of
c                    the four neighbouring cardinal cells has the lowest
c                    elevation and assigns the current cell a flow direction
c                    of j (1 to 4), stored in aspect() array
c
                     if (z (i + sdir (j, 1), k + sdir (j, 2)).lt.zmin)
     &                 then
                         aspect (i, k) = j
                         zmin = z (i + sdir (j, 1), k + sdir (j, 2))
                     endif
                     if (z (i + sdir (j, 1), k + sdir (j, 2)).lt.zmin1)
     &                 then
                         zmin1 = z (i + sdir (j, 1), k + sdir (j, 2))
                     endif
                 enddo
c                Note: if cell is a sink, aspect(i,k) = 0 and zmin = z(i,k)
c
               slope (i, k) = (z (i, k) - zmin) / dx
c
cJWJun05         calculate threshold depth value if cell is a sink and dynamic
c                routing is used
c
               if (iroute.eq.6.and.aspect (i, k).eq.0) then
                   d_thresh (i, k) = zmin1 - z (i, k)
               else
                   d_thresh (i, k) = 0.0d0
               endif
             else
c                flow routed along eight cardinal directions
c                (1='N', 5='NE', 2='E', 6='SE', 3='S', 7='SW', 4='W', 8='NW')
c
                 write(6, *) 'ERROR - attempting to route flow '
     &                 //'in 8 directions - code not available'
                 stop
             endif
          enddo
       enddo
c
c      mask off external slope areas
c
       do i = 2, nr
           do k = 2, nc
               if ( (rmask (i, k).lt.0.0d0) .or.
     &            (slope (i, k).gt.1000.d0) ) then
                   slope (i, k) = 0.0d0
               endif
           enddo
       enddo
c
c CJMH if water routing method used depends on putting cells in order from
c CJMH highest to lowest ...
       if (iroute.gt.1) then
c
c          Calculate how big order array should be and allocate it accordingly
           ncell1 = 0
           do i = 2, nr
               do k = 2, nc
                   if (rmask (i, k).ge.0.0d0) then
                       ncell1 = ncell1 + 1
                   endif
               enddo
           enddo
           allocate (order (ncell1, 3))
c
c          Initialize values of order array to zero
           do i = 1, ncell1
               do k = 1, 3
                   order (i, k) = 0
               enddo
           enddo

cCJMH      Calculate number of cells contributing flow to cell: contrib(i,k)
cbq        Contributing area is dependent on the values of sdir(4,2).
c
           call contrib_area (sdir)
c
c          write values to file
c
           if (f_contrib) then
              open (4, file=output_folder (1:output_folder_length) //
     &              'contrib.asc', status ='unknown')
c               open (4, file=pfadout(1:pfado)//'contrib.asc',
c     &          status ='unknown')
               rewind (4)
               write (4, 9999) nc2
             write (4, 9998) nr2
             write (4, 9997) xmin
             write (4, 9996) ymin
             write (4, 9995) dx / 1.d3   ! converts back to m from mm
             write (4, 9994)
               do i = 1, nr2
                   write (4, 9993) (contrib (i, j), j = 1, nc2)
               enddo
               close (4)
           endif

cCJMH      Set up order array and sort
           ncell1 = 0
           do i = 2, nr
               do k = 2, nc
cJWFeb05
cJWFeb05           ignore areas outside plot
cJWFeb05
                   if (rmask (i, k).ge.0.0d0) then
                        ncell1 = ncell1 + 1
                        order (ncell1, 1) = i
                        order (ncell1, 2) = k
                        order (ncell1, 3) = contrib (i, k)
                   endif
               enddo
           enddo

           write (6, *) ' Entering sort'
           write (6, *)
c
cCJMH      call subroutine to sort cells in order from up- to down-slope
           call sort (ncell1, order)
c
           write (6, *) ' Finished sort'
           write (6, *)
c
       endif
c CJMH endif for iroute>1 i.e. implicit routing schemes only
c
c      Prevent oversteepening of edge cells by setting their slope
c      equal to that of the adjacent cell
       nedge = 0
       do i = 2, nr
           do k = 2, nc
               if ( rmask (i,k).gt.0.0d0 ) then
                   j = aspect (i, k)
                   i1 = sdir(j,1)
                   k1 = sdir(j,2)
                   if (rmask( i + i1, k + k1 ).lt.0.0d0) then
                       if (slope (i, k).gt.slope (i - i1, k - k1)
     &                  .or. slope (i, k).eq.0.d0) then
                            slope (i, k) = slope (i - i1, k - k1)
                          nedge = nedge + 1
                     endif
                   endif
               endif
          enddo
       enddo
c
       write (6, *) nedge, ' edge cells had slope modified'
       write (6, *)
c
       if (f_order) then
           open (120, file = output_folder (1:output_folder_length)
     &           // 'order.dat', status = 'unknown')
c           open (120, file=pfadout(1:pfado)//'order.dat',
c     &        status = 'unknown')
           rewind (120)
           do icell = 1, ncell1
               ii = order (icell, 1)
               kk = order (icell, 2)
               write (120, '(3 (i4, 1x), i1, 7 (1x, e10.4))')
     &          (order (icell, j), j = 1, 3),
     &          aspect (ii, kk),
     &          slope (ii, kk),
     &          z (ii, kk),
     &          (z (ii + sdir (l, 1), kk + sdir (l, 2)), l = 1, 4),
     &          rmask (ii, kk)
           enddo
         close (120)
       endif
c
c      output slope
c
       if (f_slope) then
          open (99, file = output_folder (1:output_folder_length)
     &            // 'slope.asc', status = 'unknown')
c         open (99, file=pfadout(1:pfado)//'slope.asc',
c     &            status = 'unknown')
            rewind (99)
            write (99, 9999) nc2
            write (99, 9998) nr2
            write (99, 9997) xmin
            write (99, 9996) ymin
            write (99, 9995) dx / 1.d3   ! converts back to m from mm
            write (99, 9994)
          do i = 1, nr2
                write (99, 9991) (slope (i, j), j = 1, nc2)
            enddo
            close (99)
       endif


9999   format ('ncols ', i6)
9998   format ('nrows ', i6)
9997   format ('xllcorner ', f15.8)
9996   format ('yllcorner ', f15.8)
9995   format ('cellsize ', f15.8)
9994   format ('nodata_value -9999')
9993   format (10000 (i10, 1x))
9992   format (10000 (a1, 1x))
9991   format (10000 (e10.4, 1x))
9990   format ( '[',I2,',',I2,']' )

       return
       end
