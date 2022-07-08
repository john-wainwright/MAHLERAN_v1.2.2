c****************************************************************
c  subroutine to initialise variables at start of run
c
c  JC May 2011
c
c****************************************************************

       subroutine initialise_markers

       use shared_data
       use mt95
       implicit double precision (a - h, o - z)
       implicit integer (i - n)

       do mi = 1, mnum
        MXY (mi, 1) = 0.0d0
        MXY (mi, 2) = 0.0d0
        MXY (mi, 3) = 0.0d0
        MZ (mi, 1) = 0.0d0
        motion_susp (mi) = 0.0d0
        vel_susp (mi) = 0.0d0
        motion_bl (mi) = 0.0d0
        rest_bl (mi) = 0.0d0
        status_bl (mi) = 0.0d0
        vel_bl (mi) = 0.0d0
        motion_diffuse (mi) = 0.0d0
        vel_diffuse (mi) = 0.0d0
        marker_status (mi, 1) = 0.0d0
       enddo

       do i = 1, 2
        do iter = 1, nit
            marker_runoff (iter, i) = 0.0d0
        enddo
       enddo

       do i = 1, nr2
        do j = 1, nc2
            detach_tot_m (i, j) = 0.0d0
            do phi = 1, 6
                detach_soil_m (phi, i, j) = 0.0d0
            enddo
        enddo
       enddo

c      Set up the seed value for the random number generators based on the processor clock
       call system_clock (count=seed)
c      Initialize the state of the Mersenne Twister random number generator
       call genrand_init (seed)
c      Initialize the state of the other random number generators
       call ZBQLINI (seed)

       return
       end
