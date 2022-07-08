c ************************************************************
c  subroutine to define topographic attributes
c ************************************************************
       subroutine aspect_z_4 (sdir)
       use shared_data
       implicit double precision (a - h, o - z)
       implicit integer (i - n)
c
       integer *4 sdir (4, 2)
c      sdir() contains four vectors in the N[-1,0], E[0,1],
c      S[1,0] and W[0,-1] directions
c
       write(6, *) 'In aspect_z_4 routine'
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
cCJMH        between 1 and 4 (flow occurs in the four cardinal directions:
c            (1='N', 2='E', 3='S', 4='W')
c
cJWJun05     zmin1 is lowest of neighbouring cells for dynamic overtopping
cJWJun05     algorithm, zmin includes centre cell
c
             zmin = z (i, k)
           zmin1 = 1.d36

c            do for each cardinal direction
             do j = 1, ndirn

c                the following if ... then block determines which cell of
c                the four neighbouring cardinal cells has the lowest
c                elevation and assigns the current cell a flow direction
c                of j (1 to 4), stored in aspect() array
c
                 if (z (i + sdir (j, 1), k + sdir (j, 2)).lt.zmin) then
                     aspect (i, k) = j
                     zmin = z (i + sdir (j, 1), k + sdir (j, 2))
                 endif
                 if (z (i + sdir (j, 1), k + sdir (j, 2)).lt.zmin1) then
                     zmin1 = z (i + sdir (j, 1), k + sdir (j, 2))
                 endif
             enddo
c            Note: if cell is a sink, aspect(i,k) = 0 and zmin = z(i,k)
c
           slope (i, k) = (z (i, k) - zmin) / dx
c
cJWJun05     calculate threshold depth value if cell is a sink and dynamic
c            routing is used
c
           if (iroute.eq.6.and.aspect (i, k).eq.0) then
                   d_thresh (i, k) = zmin1 - z (i, k)
           else
                   d_thresh (i, k) = 0.0d0
             endif
          enddo
       enddo
c
9990   format ( '[',I2,',',I2,']' )
       return
       end
