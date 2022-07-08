c****************************************************************
c  subroutine to distribute markers via bedload transport
c
c  JC May 2011
c
c****************************************************************

c      This is a 2-D random-walk model with exponential distribution for rest period, and lognormal
c      distributions for motion period and motion distance. Formulation comes from Lisle et al (1998),
c      Papanicolaou et al (2002) and Ancey et al (2006). Resting and motion periods could be replaced by a time lag between states -
c      see Figure 8a of Ancey et al (2006). Motion period could be removed by assuming motion occurs in instantaneous
c      step - see Lisle et al (1998) p227. Need to consider not making the rest and motion distributions independent of one another -
c      see discussion in Lajeunesse et al (2010) and email from Clive Anderson

       subroutine bedload_markers

       use shared_data
       implicit double precision (a - h, o - z)
       implicit integer (i - n)

       double precision t_rest_bl, ustar_crit, t_motion_bl, mode_dist_bl
       double precision mean_dist_bl, dy_bl, vel1_bl
       double precision mean_motion_bl, sigma_motion_bl
c      Dimensionless modal motion duration from fit to data in Lajeunesse et al (2010)
       data mode_motion_bl / 10.6d0 /
c      Standard deviation in dimensionless motion duration from fit to data in Lajeunesse et al (2010)
       data sigma_motion_bl / 4.955d-1 /
c      Standard deviation in dimensionless motion distance from fit to data in Lajeunesse et al (2010)
       data sigma_dist_bl / 6.44d-1 /

c      Check the status of the marker: resting or in in motion?

       if (status_bl (mi).eq.0.and.rest_bl (mi)
     &  .gt.0.and.motion_bl (mi).le.0.0d0) then
c       Marker within resting period, update rest and motion durations
c       rest_bl can take a negative value but will be ignored and taken to be zero
        rest_bl (mi) = rest_bl (mi) - dt
        motion_bl (mi) = 0.0d0

       elseif (status_bl (mi).eq.1.and.rest_bl (mi)
     &      .le.0.0d0.and.motion_bl (mi).gt.0.0d0) then
c       Marker in motion

c       First check there is a slope for motion to continue
          if (slope (mycell, mxcell).ne.0.0d0) then
c           Update rest and motion durations
              rest_bl (mi) = 0.0d0
c           motion_bl can take a negative value but will be ignored and taken to be zero
              motion_bl (mi) = motion_bl (mi) - dt
c           Next move markers - transport assumed to only occur in downslope direction
c           slope to E
              if (aspect (mycell, mxcell).eq.2) then
                MXY (mi, 2) = MXY (mi, 2) -
     &                             (dt * vel_bl (mi))
c           slope to S
              elseif (aspect (mycell, mxcell).eq.3) then
                MXY (mi, 1) = MXY (mi, 1) +
     &                             (dt * vel_bl (mi))
c           slope to W
              elseif (aspect (mycell, mxcell).eq.4) then
                MXY (mi, 2) = MXY (mi, 2) +
     &                             (dt * vel_bl (mi))
c           slope to N
              else
                MXY (mi, 1) = MXY (mi, 1) -
     &                             (dt * vel_bl (mi))
              endif
          else
c           No motion, update status, rest and motion durations
              status_bl (mi) = 0
              rest_bl (mi) = 0.0d0
              motion_bl (mi) = 0.0d0
          endif

       elseif (status_bl (mi).eq.1.and.rest_bl (mi)
     &      .le.0.0d0.and.motion_bl (mi).le.0.0d0) then
c           Marker was in motion in t-dt but now deposited

c           Determine rest duration (s)
c           Distribution shape based on results in Heays et al (2010)
            t_rest_bl = ZBQLEXP (14.0d0)

c           Now check there is a rest period
              if (t_rest_bl.gt.0.0d0) then
c               Now resting, update status, rest and motion durations
                status_bl (mi) = 0
                rest_bl (mi) = t_rest_bl
                motion_bl (mi) = 0.0d0
              else
c               No resting but not in motion, update status, rest and motion durations
                status_bl (mi) = 0
                rest_bl (mi) = 0.0d0
                motion_bl (mi) = 0.0d0
              endif

       elseif (status_bl (mi).eq.0.and.rest_bl (mi)
     &      .le.0.0d0.and.motion_bl (mi).le.0.0d0) then
c       Marker is available for entrainment

c       First check there is a slope for motion to occur
          if (slope (mycell, mxcell).ne.0.0d0) then

c           Next determine if excess shear stress is available to entrain marker
c           Estimate critical shear velocity (m/s), assuming Shields number = 0.045
              ustar_crit = sqrt ((4.5d-2 * (density - 1.0d0) * 1.0d3 *
     &          9.81d0 * diameter (INT (MXY (mi, 3)))) / 1.0d3)

            if (ustar.gt.ustar_crit) then
c               Excess shear stress is available to entrain marker

c               Determine motion duration (s)
c               This distribution function is based on a fit to the data of Lajeunesse et al (2010).
c               First determine dimensionless duration
c               Generate a normally distributed random number
                t_motion_bl = ZBQLNOR (mean_motion_bl, sigma_motion_bl)
c               Then convert to a lognormally distributed random number
                t_motion_bl = exp (t_motion_bl)

c               Next determine motion duration (s).
c               Note that they found the mean duration to be unrelated to ustar
                t_motion_bl = t_motion_bl * ((diameter
     &              (INT (MXY (mi, 3))) / sigma * 9.81d0) ** 5.0d-1)

c               Now check there is a motion period
                  if (t_motion_bl.gt.0.0d0) then
c                   Now in motion, update status, rest and motion durations
                    status_bl (mi) = 1
                      rest_bl (mi) = 0.0d0
                      motion_bl (mi) = t_motion_bl

c                   Determine new position of marker
c                   Determine downslope distance (m)
c                   This distribution function is based on a fit to the data of Lajeunesse et  al (2010)
c                   Scale mode according to excess shear stress
                      mode_dist_bl = 7.0d1 * ((ustar - ustar_crit) /
     &                             sqrt (sigma * 9.81d0 * diameter
     &                             (INT (MXY (mi, 3)))))
c                   Determine dimensionless mean distance
                      mean_dist_bl = log (mode_dist_bl) +
     &                             (sigma_dist_bl ** 2)
c                   Assuming sigma_dist_bl is the same for all flow conditions, determine dimensionless distance
                    dy_bl = ZBQLNOR (mean_dist_bl, sigma_dist_bl)
c                   Then convert to a lognormally distributed random number
                    dy_bl = exp (dy_bl)
c                   Next determine motion distance (m).
                      dy_bl = dy_bl * diameter (INT (MXY (mi, 3)))

c                   Determine mean marker velocity (m/s)
                      vel1_bl = dy_bl / t_motion_bl
c                   Below is used for when it is  already in motion - the above line speeds up the code
                      vel_bl (mi) = vel1_bl

c                   Move markers - transport assumed to only occur in downslope direction
c                   For movement of markers +ve y is downslope and x is across the slope
c                   In MAHLERAN(0,0) is at top right of plot - same convention is used here
c                   slope to E
                      if (aspect (mycell, mxcell).eq.2) then
                        MXY (mi, 2) = MXY (mi, 2) -
     &                                     (dt * vel1_bl)
c                   slope to S
                      elseif (aspect (mycell, mxcell).eq.3) then
                        MXY (mi, 1) = MXY (mi, 1) +
     &                                   (dt * vel1_bl)
c                   slope to W
                      elseif (aspect (mycell, mxcell).eq.4) then
                        MXY (mi, 2) = MXY (mi, 2) +
     &                                     (dt * vel1_bl)
c                   slope to N
                      else
                        MXY (mi, 1) = MXY (mi, 1) -
     &                                     (dt * vel1_bl)
                      endif

                else
c                   No motion, update status, rest and motion durations
                      status_bl (mi) = 0
                      rest_bl (mi) = 0.0d0
                      motion_bl (mi) = 0.0d0
                  endif

            else
c               No excess shear stress is available, no motion, update status, rest and motion durations
                  status_bl (mi) = 0
                  rest_bl (mi) = 0.0d0
                  motion_bl (mi) = 0.0d0
              endif

        else
c           No slope so no motion, update status, rest and motion durations
              status_bl (mi) = 0
              rest_bl (mi) = 0.0d0
              motion_bl (mi) = 0.0d0
          endif

       endif

       return
       end
