c ************************************************************
c  subroutine to update topography and water surface
c ************************************************************
       subroutine update_top_surface

       use shared_data
       implicit double precision (a - h, o - z)
       implicit integer (i - n)

c      local variables
       integer *4 sdir (4, 2)
       data sdir /-1, 0, 1, 0, 0, 1, 0, -1 /
c
       write(51, 900) dt * iter
       write(51,*) 'updating topography and surface'
       write(51,*) 'z before'
       write(52, 900) dt * iter
       do i = 1, nr1
           write(51, 100) (z (i, k), k = 1, nc1)
           write(52, 100) (z_change (i, k), k = 1, nc1)
           do k = 1, nc1
c              update cell elevation
               z (i, k) = z (i, k) + z_change (i, k)
c              update water depth
               d (2, i, k) = d (2, i, k) - z_change (i, k)
c              reset z_change to zero
               z_change (i, k) = 0.0d0
           enddo
       enddo
       write(51,*) 'z after'
       do i = 1, nr1
           write(51, 100) (z (i, k), k = 1, nc1)
       enddo
       reorder = .false.
c
c      update aspect and slope
       do i = 2, nr
          do k = 2, nc
             newaspect = 0
             zmin = z (i, k)
c
c            do for each cardinal direction
             do j = 1, 4
                 if (z (i + sdir (j, 1), k + sdir (j, 2)).lt.zmin) then
                     newaspect = j
                     zmin = z (i + sdir (j, 1), k + sdir (j, 2))
                 endif
             enddo
c            Note: if cell is a sink, newaspect = 0 and zmin = z(i,k)
             if( newaspect.ne.aspect(i, k) ) then
                 if (iroute.gt.1) then
                     reorder = .true.
                 endif
                 aspect(i, k) = newaspect
             endif
           slope (i, k) = (z (i, k) - zmin) / dx
           enddo
       enddo

       if (reorder) then
c          Calculate number of cells contributing flow to each cell (contrib)
           call contrib_area (sdir)
c          set up order array
           ncell1 = 0
           do i = 2, nr
               do k = 2, nc
                   if (rmask (i, k).ge.0.0d0) then
                       ncell1 = ncell1 + 1
                       order (ncell1, 1) = i
                       order (ncell1 , 2) = k
                       order (ncell1, 3) = contrib (i, k)
                   endif
               enddo
           enddo
c          Error trap
           if ( iroute.eq.1 ) then
               write(6, *) 'Error - order array set up when not'
     &           //' necessary'
               stop
           end if
c          Sort cells in order from up- to down-slope
           call sort (ncell1, order)
       endif

 100   format (10000 (e17.10, 1x))
 900   format ('Time: ',e20.14,' seconds')
       return
       end
