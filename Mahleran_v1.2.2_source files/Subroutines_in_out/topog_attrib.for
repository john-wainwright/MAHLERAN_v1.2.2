c****************************************************************
c  subroutine to define topographic attributes
c****************************************************************
       subroutine topog_attrib

       use shared_data
       use parameters_from_xml
       implicit double precision (a - h, o - z)
       implicit integer (i - n)

c
c   local variables
c
c
       double precision z9 (9)
       double precision z_temp

       integer iz (9), kz (9), ia (9)
       integer *4 sdir (4, 2)

       data iz / 3 * -1, 3 * 0, 3 * 1 /
       data kz / -1, 0, 1, -1, 0, 1, -1, 0, 1 /
       data ia / 8, 1, 5, 4, 0, 2, 7, 3, 6 /
       data sdir /-1, 0, 1, 0, 0, 1, 0, -1 /


c
c   initialize array
c

       do i = 1, nr1
          do j = 1, nc1
             contrib (i, j) = 0
          enddo
       enddo
c
c   define aspect and surface curvatures
c
cDefined in MAHLERAN_storm_setting
c  nc = n_cols - 1
c  nr = n_rows - 1
c
c
       do i = 2, nr
          do k = 2, nc
             aspect (i, k) = 0
c
c   determine values of submatrix
c
             do j = 1, 9
                z9 (j) = z (i + iz (j), k + kz (j))
             enddo

c
c   define values of polynomial coefficients
c
             dee = (((z9 (4) + z9 (6)) / 2.) - z9 (5)) / (dx * dx)
             e = (((z9 (2) + z9 (8)) / 2.) - z9 (5)) / (dx * dx)
             f = (-z9 (1) + z9 (3) + z9 (7) - z9 (5)) / (4. * dx * dx)
             g = (-z9 (4) + z9 (6)) / (2. * dx)
             h = (z9 (2) - z9 (8)) / (2. * dx)
             g2 = g * g
             h2 = h * h
             g2h2 = g2 + h2

c
cb  where are panc and profc used in the model?  These are to be used to determine flow cb  divergence and convergence
c   calculate topographic parameters
c
cb the following if ... else block has been added by BF to assign nodata cells with a null value of -9999.
c JWMar2017 now changed -9999 to nodata_value_from_topog for flexibility
             if (z (i, k).le.nodata_value_from_topog) then
                slope (i, k) = 0.0d0
           else
        slope (i, k) = sqrt (g2 + h2)
           endif

             slp_temp = slope (i, k)


cb  Aspect in this code refers to flow direction which is assinged values between 1 and 4
cb  when flow is assumed to occur in the four cardinal cells or between 1 and 8
cb  for 8 directions.
c
             if (ndirn.eq.4) then
c
c   flow routed along four cardinal directions (1='N', 2='E', 3='S', 4='W')

c
cJWJun05
cJWJun05  zmin1 is lowest of neighbouring cells for dynamic overtopping algorithm,
cJWJun05  zmin includes centre cell
cJWJun05
                zmin = z (i, k)
              zmin1 = 1.d36
                do j = 1, 4
cb  the following if ... then block determines the cell out of the four neighbouring cardinal
cb  cells that has the lowest elevation and assigns the processing cell a flow direction of j (1-4)
c
c JWMar2017 added term about nodata_value_from_topog to stop routing off edge of catchment unless there
c           are "dummy" elevations beyond the edge (i.e. lower cells to define routing out of the
c           catchment, or higher/same elevation cells to force routing towards the catchment)
c
                   z_temp = z (i + sdir (j, 1), k + sdir (j, 2))
                   if (z_temp.ne.nodata_value_from_topog.and.
     &                 z_temp.lt.zmin) then
                      aspect (i, k) = j
                      zmin = z_temp
                   endif
                   if (z_temp.ne.nodata_value_from_topog.and.
     &                 z_temp.lt.zmin1) then
                      zmin1 = z_temp
                   endif
                enddo
                asp_temp = aspect (i, k)

              slope (i, k) = (z (i, k) - zmin) / dx
cJWJun05
cJWJun05   calculate threshold depth value if cell is a sink and dynamic routing is used
cJWJun05
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
c   Calculate contributing areas to each cell
cbq  Contributing area is dependent on the values of sdir(4,2).
c
       write (6, *)
       write (6, *) ' Putting cells into order '
       write (6, *)
       mincontrib = ncell
       maxcontrib = 0
       do i = 2, nr
          do j = 2, nc
c
c   moved here for efficiency -- do nothing if current cell is no_data
c
             if (z (i, j).le.nodata_value_from_topog) then
              contrib (i, j) = 0
             else
                inow = i
                jnow = j
                ir2 = aspect (inow, jnow)
                contrib (inow, jnow) = contrib (inow, jnow) + 1
                do while (aspect (inow, jnow).ne.0)
                   ir1 = aspect (inow, jnow)
                   inow = inow + sdir (ir1, 1)
                   jnow = jnow + sdir (ir1, 2)
c
c  The following if .. then block checks whether or not the cell is out of the boundary of
c  interest.
                   if (inow.le.1.or.
     &                 inow.ge.nr2.or.jnow.le.1.or.jnow.ge.nc2) then
                      exit
                   endif
                   if (abs (ir1 - ir2).eq.2) then
c
c   If ir1 and ir2 are in opposing directions, their difference will = 2
c      check for this condition to avoid infinite loop
c
                      exit
                   endif
                   contrib (inow, jnow) = contrib (inow, jnow) + 1
                   ir2 = ir1
                enddo
           endif
          enddo
       enddo
c
c   sort cells into calculation order

c
c   write values to file
c
c
c   mask off external contributing and slope areas
c
       do i = 1, nr2
          do k = 1, nc2
           if (rmask (i, k).lt.0) then
                contrib (i, k) = int (nodata_value_from_topog)
                slope (i, k) = 0.0d0
           endif
          enddo
       enddo
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
c
c   prevent very steep slopes
c
       do i = 2, nr
            do k = 2, nc
             if (slope (i, k).gt.1000.d0) then
          slope (i, k) = 1.0d0
             endif
            enddo
       enddo
       j2 = nc - 1
       nedge = 0
cJWFeb05
cJWFeb05   ignore areas outside plot
cJWFeb05   -- calculate how big order should be and allocate
cJWFeb05      it accordingly
cJWFeb05
       ncell1 = 0
       do i = 2, nr
          do j = 2, nc
             if (rmask (i, j).ge.0.0d0) then
                ncell1 = ncell1 + 1
             endif
          enddo
       enddo
       allocate (order (ncell1, 3))
       do i = 1, ncell1
          do k = 1, 3
             order (i, k) = 0
          enddo
       enddo
       ncell1 = 0
       do i = 2, nr
          do j = 2, nc
cJWFeb05
cJWFeb05   ignore areas outside plot
cJWFeb05
             if (rmask (i, j).ge.0.0d0) then
                ncell1 = ncell1 + 1
                order (ncell1, 1) = i
                order (ncell1 , 2) = j
                order (ncell1, 3) = contrib (i, j)
cJWFeb05             order ((i - 2) * j2 + j - 1, 1) = i
cJWFeb05             order ((i - 2) * j2 + j - 1 , 2) = j
cJWFeb05             order ((i - 2) * j2 + j - 1, 3) = contrib (i, j)
             endif
c
c  prevent oversteepening of edge cells by setting their slope
c     equal to that of the adjacent cell
c
           if (aspect (i, j).eq.1.and.rmask (i,j).gt.0.0d0.and.
     &           rmask (i - 1, j).lt.0.0d0) then
              if (slope (i, j).gt.slope (i + 1, j).or.
     &              slope (i, j).eq.0.d0) then
                   slope (i, j) = slope (i + 1, j)
                 nedge = nedge + 1
              endif
           elseif (aspect (i, j).eq.2.and.rmask (i,j).gt.0.0d0.
     &               and.rmask (i, j + 1).lt.0.0d0) then
              if (slope (i, j).gt.slope (i, j - 1).or.
     &              slope (i, j).eq.0.d0) then
                   slope (i, j) = slope (i, j - 1)
                 nedge = nedge + 1
              endif
           elseif (aspect (i, j).eq.3.and.rmask (i,j).gt.0.0d0.
     &               and.rmask (i + 1, j).lt.0.0d0) then
              if (slope (i, j).gt.slope (i - 1, j).or.
     &              slope (i, j).eq.0.d0) then
                   slope (i, j) = slope (i - 1, j)
                 nedge = nedge + 1
              endif
           elseif (aspect (i, j).eq.4.and.rmask (i,j).gt.0.0d0.
     &               and.rmask (i, j - 1).lt.0.0d0) then
              if (slope (i, j).gt.slope (i, j + 1).or.
     &              slope (i, j).eq.0.d0) then
           slope (i, j) = slope (i, j + 1)
                 nedge = nedge + 1
              endif
           endif
          enddo
       enddo

       write (6, *) nedge, ' edge cells had slope modified'
       write (6, *)
cJWFeb05  already set above
cJWFeb05       ncell1 = (nr - 1) * (nc - 1)
       write (6, *) ' Entering sort '
       write (6, *)
       call sort (ncell1, order)
       write (6, *) ' Finished '
       write (6, *)

       if(f_order) then
           open (120, file = output_folder (1:output_folder_length)
     &           // 'order.dat', status = 'unknown')
c           open (120, file=pfadout(1:pfado)//'order.dat',
c     &        status = 'unknown')
           rewind (120)
           do icell = 1, ncell1
               ii = order (icell, 1)
               kk = order (icell, 2)
               write (120, '(2 (i4, 1x), i7, 1x, i1, 7 (1x, e10.4))')
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
c     output slope
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

       return
       end
