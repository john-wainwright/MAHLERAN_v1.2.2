!****************************************************************
!  subroutine to distribute markers via bedload transport
!
!  JC May 2011
!  JW May 2014 modified to account for xml input changes
!
!****************************************************************

!      This is a 2-D random-walk model with exponential distribution for rest period, and lognormal
!      distributions for motion period and motion distance. Formulation comes from Lisle et al (1998),
!      Papanicolaou et al (2002) and Ancey et al (2006). Resting and motion periods could be replaced by a time lag between states -
!      see Figure 8a of Ancey et al (2006). Motion period could be removed by assuming motion occurs in instantaneous
!      step - see Lisle et al (1998) p227. Need to consider not making the rest and motion distributions independent of one another -
!      see discussion in Lajeunesse et al (2010) and email from Clive Anderson

subroutine bedload_markers_xml

use shared_data
use parameters_from_xml

implicit none

double precision :: t_rest_bl, ustar_crit, t_motion_bl, mode_dist_bl
double precision :: mean_dist_bl, dy_bl, vel1_bl
double precision :: mean_motion_bl, sigma_motion_bl
double precision :: mode_motion_bl, sigma_dist_bl
double precision :: ZBQLEXP
double precision :: ZBQLNOR

!    Dimensionless modal motion duration from fit to data in Lajeunesse et al (2010)
data mode_motion_bl / 10.6d0 /
!    Standard deviation in dimensionless motion duration from fit to data in Lajeunesse et al (2010)
data sigma_motion_bl / 4.955d-1 /
!      Standard deviation in dimensionless motion distance from fit to data in Lajeunesse et al (2010)
data sigma_dist_bl / 6.44d-1 /

!    Check the status of the marker: resting or in in motion?

if (status_bl (mi).eq.0.and.rest_bl (mi).gt.0.and.motion_bl (mi).le.0.0d0) then
!       Marker within resting period, update rest and motion durations
!       rest_bl can take a negative value but will be ignored and taken to be zero
   rest_bl (mi) = rest_bl (mi) - dt
   motion_bl (mi) = 0.0d0
elseif (status_bl (mi).eq.1.and.rest_bl (mi).le.0.0d0.and.motion_bl (mi).gt.0.0d0) then
!       Marker in motion

!       First check there is a slope for motion to continue
   if (slope (mycell, mxcell).ne.0.0d0) then
!           Update rest and motion durations
      rest_bl (mi) = 0.0d0
!           motion_bl can take a negative value but will be ignored and taken to be zero
      motion_bl (mi) = motion_bl (mi) - dt
!           Next move markers - transport assumed to only occur in downslope direction
!           slope to E
      if (aspect (mycell, mxcell).eq.2) then
     MXY (mi, 2) = MXY (mi, 2) - (dt * vel_bl (mi))
!           slope to S
      elseif (aspect (mycell, mxcell).eq.3) then
         MXY (mi, 1) = MXY (mi, 1) + (dt * vel_bl (mi))
!           slope to W
      elseif (aspect (mycell, mxcell).eq.4) then
     MXY (mi, 2) = MXY (mi, 2) + (dt * vel_bl (mi))
!           slope to N
      else
     MXY (mi, 1) = MXY (mi, 1) - (dt * vel_bl (mi))
      endif
   else
!           No motion, update status, rest and motion durations
      status_bl (mi) = 0
      rest_bl (mi) = 0.0d0
      motion_bl (mi) = 0.0d0
   endif

elseif (status_bl (mi).eq.1.and.rest_bl (mi).le.0.0d0.and.motion_bl (mi).le.0.0d0) then
!           Marker was in motion in t-dt but now deposited

!           Determine rest duration (s)
!           Distribution shape based on results in Heays et al (2010)
   t_rest_bl = ZBQLEXP (14.0d0)

!           Now check there is a rest period
   if (t_rest_bl.gt.0.0d0) then
!               Now resting, update status, rest and motion durations
      status_bl (mi) = 0
      rest_bl (mi) = t_rest_bl
      motion_bl (mi) = 0.0d0
   else
!               No resting but not in motion, update status, rest and motion durations
      status_bl (mi) = 0
      rest_bl (mi) = 0.0d0
      motion_bl (mi) = 0.0d0
   endif

elseif (status_bl (mi).eq.0.and.rest_bl (mi).le.0.0d0.and.motion_bl (mi).le.0.0d0) then
!       Marker is available for entrainment

!       First check there is a slope for motion to occur
   if (slope (mycell, mxcell).ne.0.0d0) then

!           Next determine if excess shear stress is available to entrain marker
!           Estimate critical shear velocity (m/s), assuming Shields number = 0.045
      ustar_crit = sqrt ((4.5d-2 * (density - 1.0d0) * 1.0d3 * 9.81d0 * diameter (int (MXY (mi, 3)))) / 1.0d3)

      if (ustar.gt.ustar_crit) then
!               Excess shear stress is available to entrain marker

!               Determine motion duration (s)
!               This distribution function is based on a fit to the data of Lajeunesse et al (2010).
!               First determine dimensionless duration
!               Generate a normally distributed random number
         t_motion_bl = ZBQLNOR (mean_motion_bl, sigma_motion_bl)
!               Then convert to a lognormally distributed random number
     t_motion_bl = exp (t_motion_bl)

!               Next determine motion duration (s).
!               Note that they found the mean duration to be unrelated to ustar
         t_motion_bl = t_motion_bl * ((diameter (int (MXY (mi, 3))) / sigma * 9.81d0) ** 5.0d-1)

!               Now check there is a motion period
         if (t_motion_bl.gt.0.0d0) then
!                   Now in motion, update status, rest and motion durations
            status_bl (mi) = 1
            rest_bl (mi) = 0.0d0
            motion_bl (mi) = t_motion_bl

!                   Determine new position of marker
!                   Determine downslope distance (m)
!                   This distribution function is based on a fit to the data of Lajeunesse et  al (2010)
!                   Scale mode according to excess shear stress
            mode_dist_bl = 7.0d1 * ((ustar - ustar_crit) / sqrt (sigma * 9.81d0 * diameter (int (MXY (mi, 3)))))
!                   Determine dimensionless mean distance
            mean_dist_bl = log (mode_dist_bl) + (sigma_dist_bl ** 2)
!                   Assuming sigma_dist_bl is the same for all flow conditions, determine dimensionless distance
            dy_bl = ZBQLNOR (mean_dist_bl, sigma_dist_bl)
!                   Then convert to a lognormally distributed random number
            dy_bl = exp (dy_bl)
!                   Next determine motion distance (m).
            dy_bl = dy_bl * diameter (INT (MXY (mi, 3)))

!                   Determine mean marker velocity (m/s)
            vel1_bl = dy_bl / t_motion_bl
!                   Below is used for when it is  already in motion - the above line speeds up the code
            vel_bl (mi) = vel1_bl

!                   Move markers - transport assumed to only occur in downslope direction
!                   For movement of markers +ve y is downslope and x is across the slope
!                   In MAHLERAN(0,0) is at top right of plot - same convention is used here
!                   slope to E
            if (aspect (mycell, mxcell).eq.2) then
               MXY (mi, 2) = MXY (mi, 2) - (dt * vel1_bl)
!                   slope to S
            elseif (aspect (mycell, mxcell).eq.3) then
           MXY (mi, 1) = MXY (mi, 1) + (dt * vel1_bl)
!                   slope to W
            elseif (aspect (mycell, mxcell).eq.4) then
               MXY (mi, 2) = MXY (mi, 2) + (dt * vel1_bl)
!                   slope to N
            else
           MXY (mi, 1) = MXY (mi, 1) - (dt * vel1_bl)
            endif
         else
!                   No motion, update status, rest and motion durations
            status_bl (mi) = 0
            rest_bl (mi) = 0.0d0
            motion_bl (mi) = 0.0d0
         endif
      else
!               No excess shear stress is available, no motion, update status, rest and motion durations
         status_bl (mi) = 0
         rest_bl (mi) = 0.0d0
         motion_bl (mi) = 0.0d0
      endif
   else
!           No slope so no motion, update status, rest and motion durations
      status_bl (mi) = 0
      rest_bl (mi) = 0.0d0
      motion_bl (mi) = 0.0d0
   endif

endif

return
end
