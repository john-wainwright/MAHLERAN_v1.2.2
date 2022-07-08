c****************************************************************
c  subroutine to calculate transport of splashed markers by
c  unconcentrated overland flow
c
c  JC May 2011
c
c***************************************************************

       subroutine diffuse_transport_markers

       use shared_data
       implicit double precision (a - h, o - z)
       implicit integer (i - n)

       double precision flow_energy, marker_mass
       double precision med_v_diffuse, mean_v_diffuse, vel1_diffuse
       double precision mean_dy_diffuse, dy_diffuse

c      First check there is a slope for motion to occur
       if (slope (mycell, mxcell).ne.0.0d0) then

c       Check the status of the marker: already in motion?
        if (motion_diffuse (mi).gt.0.0d0) then
c           Marker in motion, update motion duration and x,y position
c           motion_diffuse can take a negative value but will be ignored and taken to be zero
            motion_diffuse (mi) = motion_diffuse (mi) - dt

c           Next move markers - transport assumed to only occur in downslope direction
c           slope to E
              if (aspect (mycell, mxcell).eq.2) then
                MXY (mi, 2) = MXY (mi, 2) - (dt *
     &                             vel_diffuse (mi))
c           slope to S
              elseif (aspect (mycell, mxcell).eq.3) then
                  MXY (mi, 1) = MXY (mi, 1) + (dt *
     &                            vel_diffuse (mi))
c           slope to W
              elseif (aspect (mycell, mxcell).eq.4) then
                  MXY (mi, 2) = MXY (mi, 2) + (dt *
     &                             vel_diffuse (mi))
c           slope to N
              else
                  MXY (mi, 1) = MXY (mi, 1) - (dt *
     &                             vel_diffuse (mi))

            endif

        elseif (motion_diffuse (mi).le.0.0d0) then
c           Marker is available for entrainment

c           This subroutine is only used if the marker has been splashed so there is no
c           need to add in a probability of entrainment

c           Determine kinetic energy [J/(m2 * per timestep dt)]
c           -- 3600 converts from mm/s to mm/h rainfall
c           -- term on second line reduces energy according to vegetation cover
c           based on Wainwright et al. Journal of Arid Environments 43, 111-20.
            if (veg (mycell, mxcell).ge.0.0d0) then
                ke = (11.9d0 + 8.73d0 * log10 (r2 (mycell, mxcell)
     &               * 3.6d3)) * (1.0d0 - 8.1d-3 *
     &               veg (mycell, mxcell))
            else
                ke = (11.9d0 + 8.73d0 * log10 (r2 (mycell, mxcell)
     &               * 3.6d3))
            endif

c           Determine flow energy [J/(m2 * s)]
            flow_energy = 9.81d-3 * d (1, mycell, mxcell) *
     &                    v (mycell, mxcell) * slope (mycell, mxcell)

c           Determine the mass of the marker (g)
c           1.0d2 is used to convert radius from m to cm
c           if loop stops virtual velocities from becoming too high for smallest particle sizes
            if (MXY (mi, 3).le.3) then
                marker_mass = density * ((4.0d0 / 3.0d0) * pi *
     &              (radius (4) * 1.0d2) ** 3)
            else
                marker_mass = density * ((4.0d0 / 3.0d0) * pi *
     &              (radius (INT (MXY (mi, 3))) * 1.0d2) ** 3)
            endif

c           Determine the median virtual velocity (cm/min) based on Parsons et al. (1998) eq. 5.
            med_v_diffuse = 0.525 * ke ** 2.35 * flow_energy **
     &                      0.981 / marker_mass
c           Convert velocity from cm/min to m/s
            med_v_diffuse = (med_v_diffuse / 1.0d2) / 60.0d0

c           Determine mean virtual velocity (m/s) based on an exponential distribution
            mean_v_diffuse = med_v_diffuse / log (2.0d0)

c           Determine the virtual velocity (m/s) of the marker based on an exponential distribution
            vel1_diffuse = ZBQLEXP (mean_v_diffuse)
c           Below is used for when it is  already in motion - vel1_diffuse speeds up the code
            vel_diffuse (mi) = vel1_diffuse

c           Determine the mean transport distance (m) based on reanalysis of Parsons et al. (1998) data
            mean_dy_diffuse = 5.0d-2 * ke ** 1.85d0 * flow_energy
     &                        ** 4.81d-1 * marker_mass ** (-4.25d-1)

c           Determine the virtual velocity (m/s) of the marker based on an exponential distribution
            dy_diffuse = ZBQLEXP (mean_dy_diffuse)

c           Now in motion, update motion duration
              motion_diffuse (mi) = dy_diffuse / vel1_diffuse

c           Move markers - transport assumed to only occur in downslope direction
c           For movement of markers +ve y is downslope and x is across the slope. In MAHLERAN(0,0) is at top
c           right of plot - same convention is used here
c           slope to E
            if (aspect (mycell, mxcell).eq.2) then
                MXY (mi, 2) = MXY (mi, 2) -
     &                             (dt * vel1_diffuse)
c           slope to S
            elseif (aspect (mycell, mxcell).eq.3) then
                MXY (mi, 1) = MXY (mi, 1) +
     &                             (dt * vel1_diffuse)
c           slope to W
            elseif (aspect (mycell, mxcell).eq.4) then
                MXY (mi, 2) = MXY (mi, 2) +
     &                             (dt * vel1_diffuse)
c           slope to N
            else
                MXY (mi, 1) = MXY (mi, 1) -
     &                             (dt * vel1_diffuse)
              endif
        endif

       else
c       No slope so no motion, update motion duration
        motion_diffuse (mi) = 0.0d0
       endif

       return
       end
