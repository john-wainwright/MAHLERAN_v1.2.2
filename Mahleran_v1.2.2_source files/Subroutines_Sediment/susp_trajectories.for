c****************************************************************
c  subroutine to determine the trajectory of particles in
c  suspension
c
c  JC May 2011
c
c****************************************************************

c      This is a modified version of the model developed by Anderson (1987). It was originally developed for eoilian sediment transport
c      The code was first written by Banti Fenti (v1).

c      v1.1 (JRC), modifications from v1:
c      1. Temporal fluctuation added for intitial vertical fluid velocity for each simulated particle trajectory
c      2. Travel distance taken to be the mean of the distance when particle is just above and below the bed - before it was overestimating this distance
c      3. Preallocation of sizes of variables growing inside a loop to speed up computation time

c      v1.2 (JRC), modifications from v1.1:
c      1. Horizontal fluid velocity changes with height
c      2. Horizontal fluid velocity flutuates in time
c      3. Vertical fluid velocity flutuates in time according to variation in rms with height
c      4. A reduction in the varaibles growing inside a loop to speed up computation time
c      5. Fluctutations in fluid velcoity only occur if Re>2500.0d0 - this is left in but not used because this subroutine is now only called if Re>2500.0d0

c      The makers trajectory is not subsequently influenced by the horizontal velocity in the cells it covers in its path.
c      The slope of the plot/cell is not accounted for when resolving the transported distance.

c      Possible modifications:
c      1. To track which cell the particle is within during its trajectory and modify the horizontal fluid velocity acting on the particle.
c      2. Replace the random number (rand) approach for the temporal fluctuations in vertical fluid velocity if they are not normally distributed or based on a some other suitably distributed random number [see Nezu and Nakagawa (1993)].
c      3. Add in a lateral component to its movement either from a measured pdf (unlikely), a (randomly) assigned initial lateral fluid velocity or adjust its direction based on flow direction (resolved from slope of surrounding cells)

c      Description of symbols used:
c      x_p0 = position of the particle in the horizontal direction at ejection (m)
c      z_p0 = position of the particle in the vertical direction at ejection (m)
c      v_p0 = particle ejection speed (m/s)
c      u_p0 = the horizontal component of particle ejection speed (m/s)
c      w_p0 = the vertical component of particle ejection speed (m/s)
c      alpha_susp = ejection angle (degrees)
c      delta_t = time-step (s)
c      n_k = number of time-steps
c      w_p(i) = particle velocity in the vertical direction at the ith time-step (m/s)
c      z_p(i) = position of the particle in the vertical direction at the ith time-step (m)
c      u_p(i) = particle velocity in the horizontal direction at the ith time-step (m/s)
c      x_p(i) = position of the particle in the horizontal direction at the ith time-step (m)
c      w_s = particle settling velocity (m/s)
c      u(i) = horizontal fluid velocity (m/s)
c      w(i) = vertical fluid velocity (m/s)
c      tau_r = characteristic response time (s) representing the sensitivity of a particle to change in the fluid velocity
c      z_0 = bed height (m)
c      k_s = Nikuradse roughness height (m)
c      ustar = shear velocity (m/s)
c      sigma_u = the standard deviation in horizontal fluid velocity
c      sigma_w = the standard deviation in vertical fluid velocity
c      t_l = Lagrangian time scale (s)
c      b1_susp = dimensionless constant
c      t_star_l = Modified Lagrangian time scale
c      dist_susp = length of trajectory (m)
c      t_motion_susp = time in suspension (s)
c      kappa = von Karman constant

       subroutine susp_trajectories

       use shared_data
       implicit double precision (a - h, o - z)
       implicit integer (i - n)

       double precision t_l, k_s, z_0, z_0_plus, ksplus, Rstar
       double precision delta_Uplus, tau_r, delta_t, exp_dist_susp
       double precision z_p_plus, alpha_susp, u_p_star
       double precision sigma_u_p, w_p_star, u, sigma_u, w, sigma_w
       double precision var_u, log_u, log_sigma_u
       double precision z_p_d, z_int, t_motion_susp, vel1_susp
       double precision z_u_plus (1000), dy_susp
       double precision z_p (10000), x_p (10000)
       double precision u_p (10000), w_p (10000)
       double precision b1_susp, kappa, n_k, x_p0, sigma_u_p_star
       double precision, allocatable :: uf (:)
       data b1_susp / 1.373d0 /
       data kappa / 0.41d0 /
       data n_k / 1.0d4 /
       data x_p0 / 0.0d0 /

c      Compute the Lagrangian time scale (s)
       t_l = b1_susp * (d (1, mycell, mxcell) / 1.0d3) / ustar

c      Estimate Nikuradse roughness height
c      Based on Schlichting's (1979) definition in order for his relationship for the roughness parameter to hold
       k_s = d50 * 2

c      Define the bed height, assuming that it is equal to the height of the median bed particle.
c      This is used to ensure that a velocity can be estimated for the first time step, else z=0 and u=0.
c      Reference bed height is the mean roughness trough.  If the bed composes predominately gravel consider a different definition
       z_0 = d50

c      Define the dimensionless bed height
       z_0_plus = (z_0 * ustar) / viscosity

c      Define the vertical position of the particle at ejection
       z_p0 = z_0

c      Determine the roughness Reynolds number
       ksplus = (k_s * ustar) / viscosity

c      Determine the Reynolds number based on ustar
       Rstar = (ustar * (d (1, mycell, mxcell) / 1.0d3)) / viscosity

c      Determine the roughness parameter. Based on Schlichting (1979) and shown to hold for sand beds by Schultz and Flack (2003)
c      It accounts for the shift in the log-law due to roughness.
c      I have only checked that this is valid for transitionally rough regimes where 5<ksplus<70 (which applies to overland flows).
c      If the bed composes predominately gravel consider if the relationship still holds - it was first developed for fully rough flows so it should.
       delta_Uplus = (1 / kappa) * log (ksplus) + 5.75 - 8.5

c      Compute the response time of the particle to a change in vertical fluid velocity
       tau_r = (diameter (INT (MXY (mi, 3))) ** 2) * (density * 1.0d3) /
     &         (18 * 1.0d3 * viscosity)

c      Estimate a suitable time step given that (delta_t/tau_r)<1 must hold
c      i.e. the time step must be less than the response time of the particle to change in fluid velocity
       delta_t = 0.9 * tau_r

c      Exponential function for resolving particle and fluid velocities within k loop
       exp_dist_susp = 1.0 - (exp (-(delta_t / tau_r)))

       if (ksplus.gt.70) then
        stop 'WARNING: flow regime is hydraulically rough,
     &          assess whether the log-law is appropriate'
       endif

c      Define the vertical position of the particle at ejection
       z_p (1) = z_p0

c      Define the dimensionless vertical position of the particle at ejection
       z_p_plus = z_0_plus

c      Define the horizontal position of the particle at ejection
       x_p (1) = x_p0

c      Determine the ejection angle (degrees)
c      Kang et al's (2008) angles fitted an exponential distribution. My analysis of the presented mean values revealed that
c      it has little correlation with horizontal fluid velocity and grain size
c      Angle of 38.8 is the mean of all the results collected by Kang et al (2008)
       alpha_susp = ZBQLEXP (38.8d0)

c      Determine the ejection velocity of the particle in the horizontal direction:
c      Kang et al's (2008) velocities fitted a normal distribution. My analysis revealed that the mean and std. dev. values,
c      when scaled by the free stream velocity, were highly correlated with grain size.
c      First determine the free stream velocity uf. This is assumed to be equivalent to the maximum fluid velocity in the vertical
       do n = 1, 1000
        z_int = 1.0d-4 * n
        if (z_int.le.d (1, mycell, mxcell) / 1.0d3) then
            z_u_plus (n) = (z_int * ustar)/ viscosity
          else
            exit
        endif
        if (n.eq.1000) then
            stop 'WARNING: in suspension, the number of heights for
     &           resolving the velocity profile is too small,
     &             increase the number of time-steps'
        endif
       enddo

c      Explanation of method for resolving the horizontal fluid velocty can be found further below
       allocate (uf (n - 1))
       do j = 1, n - 1
          uf (j) = ((1.0d0 / kappa) * log (z_u_plus (j)) +
     &             (4.9d0 - delta_Uplus)) * ustar
       enddo

c      Determine the scaled mean horizonal ejection velocity based on Kang et al (2008)
c      u_p_star = (-65.9 * diameter (INT (MXY (mi, 3)))) + 0.0611 - no longer used because it gave negative values for larger grain sizes.
c      Now based on mean value for all grain sizes
       u_p_star = 4.00d-2
c      Determine the scaled std. dev in horizonal ejection velocity
c      sigma_u_p_star = (-39.7 * diameter (INT (MXY (mi, 3)))) + 0.0475 - no longer used because it gave negative values for larger grain sizes.
c      Now based on mean value for all grain sizes
       data sigma_u_p_star / 3.46d-2 /
c      Determine the dimensional equivalents
       u_p (1) = u_p_star * maxval (uf)
       sigma_u_p = sigma_u_p_star * maxval (uf)
       u_p (1) = ZBQLNOR (u_p (1), sigma_u_p)

c      Determine the ejection velocity of the marker in the vertical direction
c      Kang et al's (2008) velocities fitted an exponential distribution. My analysis revealed that the mean values,
c      when scaled by the free stream velocity, were highly correlated with grain size
c      Determine the scaled mean vertical ejection velocity
c      w_p_star=(-35.2*diameter(INT(MXY (mi, 3))))+0.0398; - no longer used because it gave negative values for larger grain sizes.
c      Now based on mean value for all grain sizes
       w_p_star = 2.83d-2
c      Determine the dimensional equivalent
       w_p (1) = w_p_star * maxval (uf)
       w_p (1) = ZBQLEXP (w_p (1))

c      Define the initial horizontal fluid velocity.
c      Shown to hold for sand beds by Schultz and Flack (2003) and Bigillon et al (2006).
c      B=4.9 for sand beds (Bigillon et al., 2006). May not hold for yplus<10, no measurements are available this close to the bed.
c      I have only been checked that this is valid for transitionally rough regimes where 5<ksplus<70, which applies to overland flows.
c      If the bed composes predominately gravel consider if the parameter values are correct, especially B=4.9
       u = ((1 / kappa) * log (z_0_plus) + (4.9 - delta_Uplus)) * ustar

c      Determine the horizontal turbulence intensity u_rms
c      Based on semi-theoretical curve of Nezu (1977) and experimental values for coefficients of Bigillon et al (2006)
       sigma_u = (2* exp (-z_0_plus / Rstar) * (1 - exp (-z_0_plus / 8))
     &           + (0.34 * z_0_plus) * exp (-z_0_plus / 8)) * ustar

c      Instigate a temporal fluctuation in the initial horizontal fluid  velocity.
c      Shape of distributions based on results in Bigillon et al (2006) and theoretical argument of Wu and Lin (2002)
       if (re.gt.2500.0d0) then
c       if (z_0 / ( d (1, mycell, mxcell) / 1.0d3).lt.1.0d-1.or.
c     &    z_0 / ( d (1, mycell, mxcell)/ 1.0d3).gt.2.0d-1) then
c           u is log-normally distributed
c           Convert mean and variance of log normal distribution to mean and std. dev of normal distribution
c           If original u and sigma_u are used the resulting and mean and std. dev of the generated log normal
c           distribution are not as specified (i.e. not u(1) and sigma_u)
              var_u = sigma_u ** 2
              log_u = log ((u ** 2) / sqrt (var_u + u ** 2))
              log_sigma_u = sqrt (log (var_u / (u ** 2) + 1))
c           First generate a normally distributed random number
            u = ZBQLNOR (log_u, log_sigma_u)
c           Then convert to a lognormally distributed random number
            u = exp (u)
c          else
c           u is normally distributed
c           u = ZBQLNOR (u, sigma_u)
c          endif
       endif

c      Determine the vertical turbulence intensity
c      Based on semi-theoretical curve of Nezu (1977) and experimental values for coefficients of Bigillon et al (2006)
       if (re.gt.2500.0d0) then
c       if (z_0 / ( d(1,mxcell,mycell) / 1.0d3).lt.2.0d-1) then
c           Based on renewal model of Nakagawa and Nezu (1978) which has been shown to fit sand beds by Bigillon et al (2006)
c              sigma_w = (1.27 * exp (-z_0_plus / Rstar)) * ustar
c          else
c           Based on empirical fit of Kironoto and Graf (1994) and shown to fit sand beds by Bigillon et al (2006)
              sigma_w = (1.14 * exp (-0.76 * (z_0 /
     &                (d (1, mycell, mxcell) / 1.0d3)))) * ustar
c          endif
c       Define the initial vertical fluid velocity by instigating a temporal fluctuation
c         Assumption of 2-D flow in which the time-averaged vertical fluid velocity is zero
c         Shape of distribution based on results in Bigillon et al (2006)
        w = ZBQLNOR (0.0d0, sigma_w)
       else
        w = 0.0d0
       endif

c      Start LOOP for simulating trajectory - n_k must be large enough to ensure that the particle hits the bed
       do k = 2, n_k

c       Determine the the dimensionless vertical position ofthe marker
          z_p_plus = (z_p (k-1) * ustar) / viscosity

c       Determine the relative vertical position of the marker at kth time step
          z_p_d = z_p (k-1) / (d (1, mycell, mxcell) / 1.0d3)

c       Determine the vertical turbulence intensity w_rms
c       NB. This has to be based on z_p(k-1) because w(k) is needed for z_p(k)
c       Based on semi-theoretical curve of Nezu (1977) and experimental values for coefficients of Bigillon et al (2006)
        if (re.gt.2500.0d0) then
c           if (z_p_d.lt.2.0d-1) then
c               Based on renewal model of Nakagawa and Nezu (1978) which has been shown to fit sand beds by Bigillon et al (2006)
c               sigma_w = (1.27 * exp (-z_p_plus / Rstar)) * ustar
c           else
c               Based on empirical fit of Kironoto and Graf (1994) and shown to fit sand beds by Bigillon et al (2006)
                sigma_w = (1.14 * exp (-0.76 * (z_p (k - 1) /
     &                    (d (1, mycell, mxcell) / 1.0d3)))) * ustar
c           endif
c           Define the initial vertical fluid velocity by instigating a temporal fluctuation
c           Assumption of 2-D flow in which the time-averaged vertical fluid velocity is zero
c             Shape of distribution based on results in Bigillon et al (2006)
            w = ZBQLNOR (0.0d0, sigma_w)
        else
            w = 0.0d0
        endif

c       Compute vertical velocity component of the marker at the kth time-step
          w_p (k) = w_p (k - 1) + ((w - w_p (k - 1)) * exp_dist_susp)

c       Compute vertical position of the marker at the kth time-step
          z_p (k) = z_p (k - 1) - (settling_vel (INT (MXY (mi, 3))) *
     &            delta_t) + (w * delta_t) - (w - w_p (k - 1)) *
     &            tau_r * exp_dist_susp

c       If the vertical position of the marker at k=2 is less than z_0 then the marker has not been transported
          if (z_p (2).lt.z_0) then
c           Update motion duration
              motion_susp (mi) = 0.0d0
              exit
        else
c           Ensure the vertical position of the marker is less than the depth of water
c           Min function returns the smaller of the two values d and z_p(k)
            z_p (k) = min ((d (1, mycell, mxcell) / 1.0d3), z_p (k))

c           Ensure that the vertical position of the marker is greater than z0 (else you get a -ve u and complex numbers
c           for dist_susp). Makes the assumption that the marker in its final time step hits the bed.
c           Max function returns the greater of the two values z_0 and z_p(k)
              z_p (k) = max (z_0, z_p (k))

c           Determine the dimensionless vertical position of the marker at kth time step
            z_p_plus = (z_p (k) * ustar) / viscosity

c           Determine the relative vertical position of the marker at kth time step
            z_p_d = z_p (k) / (d (1, mycell, mxcell) / 1.0d3)

c           Determine the horizontal fluid velocity
c           Shown to hold for sand beds by Schultz and Flack (2003) and Bigillon et al (2006).
c           B=4.9 for sand beds (Bigillon et al., 2006). II/kappa=0.5 for sand beds
c           I have only been checked that this is valid for transitionally rough regimes where 5<ksplus<70, which applies to overland flows.
c           If the bed composes predominately gravel consider if the parameter values are correct, especially B=4.9 and 2II/kappa=0.5.
              if (z_p_d.ge.2.0d-1) then
c               Use the log-wake law
                u = ((1 / kappa) * log (z_p_plus) +
     &              (4.9 - delta_Uplus)) * ustar
            else
c               Use the log-law
                u = ((1 / kappa) * log (z_p_plus) +
     &              (4.9-delta_Uplus) + (0.5 * ((sin (pi * z_p (k)
     &              / 2 * (d (1, mycell, mxcell) / 1.0d3)))
     &              ** 2))) * ustar
              endif

c           Determine the horizontal turbulence intensity
c           Based on semi-theoretical curve by Nezu (1977) and experimental values for coefficients of Bigillon et al (2006)
              sigma_u = (2 * exp (-z_p_plus / Rstar) *
     &                (1 - exp (-z_p_plus /8)) + (0.34 * z_p_plus)
     &                * exp (-z_p_plus / 8)) * ustar

c           Instigate a temporal fluctuation in the initial horizontal fluid velocity.
              if (re.gt.2500.0d0) then
c               if (z_p_d.lt.1.0d-1.or.z_p_d / (d (1, mycell, mxcell) /
c     &            1.0d3).gt.2.0d-1) then
c                   u is log-normally distributed
c                   Convert mean and variance of log normal distribution to mean and std. dev of normal distribution
c                   If original u and sigma_u are used the resulting and mean and std. dev of the generated log normal
c                   distribution are not as specified (i.e. not u(1) and sigma_u)
                    var_u = sigma_u ** 2
                    log_u = log ((u ** 2) / sqrt (var_u + u ** 2))
                    log_sigma_u = sqrt (log (var_u / (u ** 2) + 1))
c                   First generate a normally distributed random number
                    u = ZBQLNOR (log_u, log_sigma_u)
c                   Then convert to a lognormally distributed random number
                    u = exp (u)
c                  else
c                   u is normally distributed
c                   u = ZBQLNOR (u, sigma_u)
c                  endif
              endif

c           Compute the horizontal velocity of the marker
              u_p (k) = u_p (k-1) + (u - u_p (k-1)) * exp_dist_susp

c           Compute horizontal position of the marker at the kth time-step
            x_p (k) = x_p (k-1) + (u * delta_t) - (u - u_p (k-1))
     &                * tau_r * exp_dist_susp

c           If the vertical position of the marker is less than z_0 as it descendes,
c           the horizontal distance dy_susp of the trajectory is the mean of the distance at k and k-1 time-step
c           Particle is assumed to move with a mean velocity rather than using u_p (k) so it is treated in the same way as splash and bedload

            if (z_p (k - 1).gt.z_p (k).and.z_p (k).eq.z_0) then
                dy_susp = (x_p (k) + x_p (k - 1)) / 2

c               Compute the time in suspension (s)
                  t_motion_susp = ((k + k - 1) / 2) * delta_t

c               Determine mean particle velocity (m/s)
                  vel1_susp = dy_susp / t_motion_susp
c               Below is used for when it is already in motion - the above line speeds up the code
                  vel_susp (mi) = vel1_susp

c               Now in motion, update motion duration
                  motion_susp (mi) = t_motion_susp

c               Move markers - transport assumed to only occur in downslope direction
c               For movement of markers +ve y is downslope and x is across the slope. In MAHLERAN(0,0) is at top
c               right of plot - same convention is used here
c               slope to E
                if (aspect (mycell, mxcell).eq.2) then
                    MXY (mi, 2) = MXY (mi, 2) -
     &                                 (dt * vel1_susp)
c               slope to S
                elseif (aspect (mycell, mxcell).eq.3) then
                    MXY (mi, 1) = MXY (mi, 1) +
     &                                 (dt * vel1_susp)
c               slope to W
                elseif (aspect (mycell, mxcell).eq.4) then
                    MXY (mi, 2) = MXY (mi, 2) +
     &                                 (dt * vel1_susp)
c               slope to N
                else
                    MXY (mi, 1) = MXY (mi, 1) -
     &                                 (dt * vel1_susp)
                  endif

                exit

            endif

            if (k.eq.n_k) then
                stop 'WARNING: in suspension, the marker has not hit
     &               the bed, increase the number of time-steps'
            endif

        endif

       enddo

       return
       end
