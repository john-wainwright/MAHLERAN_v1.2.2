

c****************************************************************
c  subroutine to update sediment flow routing variables
c****************************************************************
       subroutine update_sediment_flow


       use shared_data
       implicit double precision (a - h, o - z)
       implicit integer (i - n)

       sed_temp2 = 0.0d0
       dep_sum = 0.0d0
       do im = 1, nr2
          do jm = 1, nc2
             if (rmask (im, jm).gt.0.0d0.and.iter.gt.1) then
c
c   reset proportion (taking into account proportion at start of timestep,
c     detachment and deposition)
              if (r2 (im, jm).ge.0.0d0.or.d(2,im,jm).gt.0) then
                 sedsum = 0.0d0
                 do phi = 1, 6
c     sed_temp: height in [mm]
                    sed_temp (phi, im, jm) = sed_temp (phi, im, jm)
     &                                   + depos_soil (phi, im, jm)

                    sedsum = sedsum + sed_temp (phi,im,jm)
                    depsum = depsum + depos_soil (phi,im,jm)
                 enddo
c
c  commented lines used for error checking
c
c                   if (sedsum.le.0.0d0) then
c                   write (6, *) ' WARNING -- sedsum <=0: = ', sedsum
c                     write (6, 9999) iter, im, jm, phi,
c     &                          (sed_propn (phi, im, jm), phi = 1, 6),
c     &                          (sed_temp (phi, im, jm), phi = 1, 6),
c     &                          (depos_soil (phi, im, jm), phi = 1, 6)
c                     write (6, 9998) d (2, im, jm), v (im, jm),
c     &                                q (2, im, jm)
c                  endif
c                  sed_sum_temp = 0.0d0
c                do phi = 1, 6
c                   sed_propn (phi, im, jm) = sed_temp (phi, im, jm)
c     &                                          / sedsum
c                     sed_sum_temp = sed_sum_temp + sed_propn (phi, im, jm)
c                enddo
c                  sed_temp1 = abs (sed_sum_temp - 1.0d0)
c                  sed_temp2 = sed_temp2 + sed_temp1
c                  if (sed_temp1.gt.1.0d-10) then
c                     write (6,  *) ' WARNING -- sed_sum_temp <>1: = ',
c     &                                sed_sum_temp, sed_temp1
c                     write (6, 9999) iter, im, jm, phi,
c     &                          (sed_propn (phi, im, jm), phi = 1, 6),
c     &                          (sed_temp (phi, im, jm), phi = 1, 6),
c     &                          (depos_soil (phi, im, jm), phi = 1, 6)
c                     write (6, 9998) d (2, im, jm), v (im, jm),
c     &                                q (2, im, jm)
c                  endif
              endif

           endif
c
c     set sediment velocity to zero after each time step
           do phi = 1, 6
cJWFeb05 not soil velocity so that at end of run, sediment movement
cJWFeb05    declines slowly rather than abruptly
              v_soil (phi, im, jm) = 0.9d0 * v_soil (phi, im, jm)
              detach_soil (phi, im, jm) = 0.0d0
              depos_soil (phi, im, jm) = 0.0d0
           enddo
          enddo
       enddo
c
c   output size classes for error checking
c
c       write (6, 9996) iter, sedsum
c       do phi = 1, 6
c          write (6, 9995) phi
c          do im = 1, nr2
c          write (6, 9994) (sed_propn (phi, im, jm), jm = 1, nc2)
c          enddo
c          write (6, *)
c       enddo


c9996   format ('iteration: ', i5, ', sedsum = ', f8.4)
c9995   format ( /'size class: ', i1)
c9994   format (1000 (f9.2, 1x))
c
c   used for error checking
c
c      write (6, 9997) iter, sed_temp2, depsum
c9999   format ('iteration: ', i5, ' cell: ', (2 (i3, 1x)), ' phi: ',
c     &         i1, ', sed_propn(1-6): ', 6 (e10.4, 1x),
c     &         ', sed_temp(1-6): ', 6 (e10.4, 1x),
c     &         ', depos_soil (1-6): ', 6 (e10.4, 1x))
c9998   format (' flow depth: ', f8.4, ', velocity: ', f8.4,
c     &         ', discharge: ', f8.4)
c9997   format ('iteration: ', i5, ', sed_temp2: ', f8.4,
c     &              ', depsum: ', f8.2)

       return
       end
