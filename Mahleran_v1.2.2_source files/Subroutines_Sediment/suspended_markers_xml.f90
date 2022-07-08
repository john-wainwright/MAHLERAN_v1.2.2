!****************************************************************
!  subroutine to distribute markers via suspended transport
!
!  JC May 2011
!  JW May 2014 modified to account for xml input changes
!
!****************************************************************

subroutine suspended_markers_xml

use shared_data
use mt95
use parameters_from_xml

implicit none

double precision :: dimless_shear, p_const, p_susp
double precision :: randn_entrain

!    First check there is a slope for motion to occur
if (slope (mycell, mxcell).ne.0.0d0) then

!       Check the status of the marker: already in motion?
   if (motion_susp (mi).gt.0.0d0) then
!           Marker in motion, update motion duration and x,y position

!           motion_susp can take a negative value but will be ignored and taken to be zero
      motion_susp (mi) = motion_susp (mi) - dt

!           Next move markers - transport assumed to only occur in downslope direction
!           slope to E
      if (aspect (mycell, mxcell).eq.2) then
         MXY (mi, 2) = MXY (mi, 2) - (dt * vel_susp (mi))
!           slope to S
      elseif (aspect (mycell, mxcell).eq.3) then
         MXY (mi, 1) = MXY (mi, 1) + (dt * vel_susp (mi))
!           slope to W
      elseif (aspect (mycell, mxcell).eq.4) then
         MXY (mi, 2) = MXY (mi, 2) + (dt * vel_susp (mi))
!           slope to N
      else
         MXY (mi, 1) = MXY (mi, 1) - (dt * vel_susp (mi))

      endif
   elseif (motion_susp (mi).le.0.0d0) then
!           Marker is available for entrainment

!           Estimate probability of marker being entrained p_susp
      dimless_shear = ustar ** 2 / (sigma * 9.81 * diameter (int (MXY (mi, 3))))
      p_const = log (0.049d0 / (dimless_shear * 0.25d0))
      p_susp = 0.5d0 - (0.5d0 * (p_const / abs (p_const)) * sqrt (1.d0 - exp (p_par * (p_const / 0.702d0) ** 2)))

!           Determine if it is entrained into suspension
      call genrand_real1 (randn_entrain)

      if (randn_entrain.le.p_susp) then
!               Estimate travel distance, time and velocity (m) based on modified version of Anderson (1987) model
         call susp_trajectories_xml
      else
!               No motion, update motion duration
         motion_susp (mi) = 0.0d0
      endif
   endif
else
!       No slope so no motion, update motion duration
   motion_susp (mi) = 0.0d0
endif

return
end
