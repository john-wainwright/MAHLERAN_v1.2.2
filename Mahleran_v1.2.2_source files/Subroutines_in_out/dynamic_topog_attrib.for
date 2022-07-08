c*******************************************************************
cJWFeb05  subroutine to define topographic attributes using surface
cJWFeb05     topography and water-surface so that sinks can be
cJWFeb05     overtopped automatically
cJWFeb05     Only called if iroute=6.
c*******************************************************************
       subroutine dynamic_topog_attrib

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
       do i = 2, nr
          do k = 2, nc
             aspect (i, k) = 0
             if (z (i, k).le.nodata_value_from_topog) then
              slope (i, k) = 0.0d0
             endif

cJWFeb05  Algorithm only defined for ndirn=4
cJWFeb05
cJWFeb05  flow routed along four cardinal directions (1='N', 2='E', 3='S', 4='W')
cJWFeb05  routing based on water-surface elevation
cJWFeb05
             zmin = z (i, k) + d (1, i, k)
             do j = 1, 4
cJWFeb05  the following if ... then block determines the cell out of the four neighbouring cardinal
cJWFeb05  cells that has the lowest elevation and assigns the processing cell a flow direction of j (1-4)
cJWFeb05
cJWFeb05  routing based on water-surface elevation
cJWFeb05
                z_temp = z (i + sdir (j, 1), k + sdir (j, 2))
                if (z_temp.ne.nodata_value_from_topog.and.
     &              z_temp +
     &              d (1, i + sdir (j, 1), k + sdir (j, 2)).lt.zmin)
     &              then
                   aspect (i, k) = j
                   zmin = z_temp +
     &                    d (1, i + sdir (j, 1), k + sdir (j, 2))
                endif
             enddo
             asp_temp = aspect (i, k)
           slope (i, k) = (z (i, k) + d (1, i, k) - zmin) / dx
cJWFeb05
cJWFeb05   calculate threshold depth value if cell is a sink
cJWFeb05
cJWJun05           if (aspect (i, k).eq.0) then
cJWJun05              d_thresh (i, k) = zmin - z (i, k) - d (1, i, k)
cJWJun05               else
cJWJun05              d_thresh (i, k) = 0.0d0
cJWJun05           endif
          enddo
       enddo
cJWFeb05 ---<<<<<

c
c   Calculate contributing areas to each cell
c
cJWFeb05       write (6, *)
cJWFeb05       write (6, *) ' Putting cells into order '
cJWFeb05       write (6, *)
       mincontrib = ncell
       maxcontrib = 0
       do i = 2, nr
          do j = 2, nc
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
cb  The following if .. then block checks whether or not the cell is out of the boundary of
cb  interest.
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
cJWMay05**************n.b. change these to .asc if needed in future (see above)
cJWFeb05       open (4, file = 'contrib.img', status = 'unknown')
cJWFeb05       rewind (4)
cJWFeb05       do i = 1, nr1
cJWFeb05          do j = 1, nc1
cJWFeb05        write (4, *) contrib (i, j)
cJWFeb05             mincontrib = min (mincontrib, contrib (i, j))
cJWFeb05             maxcontrib = max (maxcontrib, contrib (i, j))
cJWFeb05          enddo
cJWFeb05       enddo
cJWFeb05       close (4)
cJWFeb05       ifour = 4
cJWFeb05       open (4, file = 'contrib.doc', status = 'unknown')
cJWFeb05       rewind (4)
cJWFeb05       call write_doc_file (ifour, 'contributing area        ', nc1,
cJWFeb05     &                      nr1, xmin, xmax, ymin, ymax,
cJWFeb05     &                      dble (mincontrib), dble (maxcontrib))
cJWFeb05       close (4)

       do i = 2, nr
          do k = 2, nc
           if (rmask (i, k).lt.0) then
                slope (i, k) = 0.0d0
           endif
           if (slope (i, k).gt.1000.d0) then
              slope (i, k) = 0.0d0
           endif
        enddo
       enddo

c     output slope
cJWMay05**************n.b. change these to .asc if needed in future (see above)
cJWFeb05     open (99, file = 'slope.img', status = 'unknown')
cJWFeb05       rewind (99)
cJWFeb05       do i = 1, nr1
cJWFeb05          do j = 1, nc1
cJWFeb05           write (99, *) slope (i, j)
cJWFeb05          enddo
cJWFeb05       enddo
cJWFeb05       close (99)
cJWFeb05       ifour = 99
cJWFeb05       open (99, file = 'slope.doc', status = 'unknown')
cJWFeb05       rewind (99)
cJWFeb05       call write_doc_file (ifour, 'contributing area        ', nc1,
cJWFeb05     &                      nr1, xmin, xmax, ymin, ymax,
cJWFeb05     &                      dble(0), dble(1))
cJWFeb05       close (99)

       j2 = nc - 1
       nedge = 0
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
             endif
c
c  prevent oversteepening of edge cells by setting their slope
c     equal to that of the adjacent cell
c
           if (aspect (i, j).eq.1.and.rmask (i,j).gt.0.0d0.and.
     &        rmask (i - 1, j).lt.0.0d0) then
                slope (i, j) = slope (i + 1, j)
                nedge = nedge + 1
             elseif (aspect (i, j).eq.2.and.rmask (i,j).gt.0.0d0.
     &               and.rmask (i, j + 1).lt.0.0d0) then
                slope (i, j) = slope (i, j - 1)
                nedge = nedge + 1
             elseif (aspect (i, j).eq.3.and.rmask (i,j).gt.0.0d0.
     &               and.rmask (i + 1, j).lt.0.0d0) then
                slope (i, j) = slope (i - 1, j)
                nedge = nedge + 1
             elseif (aspect (i, j).eq.4.and.rmask (i,j).gt.0.0d0.
     &               and.rmask (i, j - 1).lt.0.0d0) then
              slope (i, j) = slope (i, j + 1)
                nedge = nedge + 1
           endif
          enddo
       enddo

cJWFeb05     write (6, *) nedge, ' edge cells had slope modified'
cJWFeb05     write (6, *)
cJWFeb05  already set above
cJWFeb05       ncell1 = (nr - 1) * (nc - 1)
cJWFeb05       write (6, *) ' Entering sort '
cJWFeb05       write (6, *)
       call sort (ncell1, order)
cJWFeb05       write (6, *) ' Finished '
cJWFeb05       write (6, *)
cJWFeb05     open (120, file = 'order.dat', status = 'unknown')
cJWFeb05     rewind (120)
cJWFeb05       do k = 1, ncell1
cJWFeb05        ii = order (k, 1)
cJWFeb05        jj = order (k, 2)
cJWFeb05        write (120, '(3 (i4, 1x), i1, 7 (1x, e10.4))')
cJWFeb05     &          (order (k, l), l = 1, 3),
cJWFeb05     &          aspect (ii, jj),
cJWFeb05     &          slope (ii, jj),
cJWFeb05     &          z (ii, jj),
cJWFeb05     &          (z (ii + sdir (l, 1), jj + sdir (l, 2)), l = 1, 4),
cJWFeb05     &          rmask (ii, jj)
cJWFeb05     enddo
cJWFeb05     close (120)


9999   format ('DSAA')

       return
       end
