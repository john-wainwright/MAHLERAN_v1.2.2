c****************************************************************
c  subroutine to update water flow routing
c****************************************************************
       subroutine update_water_flow
       use shared_data
       implicit double precision (a - h, o - z)
       implicit integer (i - n)

c           d_soil(phi,1,im,jm): soil depth within water flow in current cell [mm]
c           v_soil(phi,im,jm): virtual travel velocity [mm/s]
c           q_soil(phi,1,im,jm): unit sediment discharge [mm2/s]
c           detach_soil(phi,im,jm): detachment at a point [mm/s]
c           depos: deposition at a point [mm/s]
c                       qsedin(phi,1,im,jm): sediment inflow to current cell [mm2/s]

       do i = 2, nr
          do j = 2, nc
           if (rmask (i, j).le.0) then
              d (2, i, j) = 0.0d0
              q (2, i, j) = 0.0d0
        do phi=1,6
           d_soil (phi, 2, i, j) = 0.0d0
           q_soil (phi, 2, i, j) = 0.0d0
           v_soil (phi, i, j) = 0.0d0
c                   write (6, *) 1, phi, i, j
                   qsedin (phi, 2, i, j) = 0.0d0
        enddo
           endif

             d (1, i, j) = d (2, i, j)
             q (1, i, j) = q (2, i, j)
             do phi = 1, 6
        d_soil (phi, 1, i, j) = d_soil (phi, 2, i, j)
        q_soil (phi, 1, i, j) = q_soil (phi, 2, i, j)
c                write (6, *) 2, phi, i, j
c     &                       , qsedin (phi, 1, i, j),
c     &                       qsedin (phi, 2, i, j)
                qsedin (phi, 1, i, j) = qsedin (phi, 2, i, j)
             enddo
             qin (1, i, j) = qin (2, i, j)
             v1 (i, j) = v (i, j)
             qsum (i, j) = qsum (i, j) + q (2, i, j)
          enddo
       enddo

       return
       end
