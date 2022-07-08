c****************************************************************
c  subroutine to define the mode of transport of the markers
c  Detachment under transitionally flow conditions is now based
c  on the probability of combined raindrop and flow detachment
c
c  JC November 2011
c
c****************************************************************

       subroutine route_markers

       use shared_data
       use mt95

       implicit double precision (a - h, o - z)
       implicit integer (i - n)

       double precision dimless_shear, p_const
       double precision p_diffuse, randn_detach, m_num
       integer burdep
       integer *4 m_runoff

c      marker_status is the detachment and transport status of the marker:
c      1=splash, 2=diffuse, 3=transitional, 4=bedload, 5=suspension
c      This is used for determining active layer depth for flux estimates

c      Convert total number of markers from integer to real number
c      Needed for marker_runoff
       m_num = dble (mnum)

c   MAIN LOOP to define mode of transport for each marker

c      Reset counter for number of markers transported off the end of the slope
       m_runoff = 0

       do mi = 1, mnum

c       First check if the marker has already been eroded off the slope
        if (MXY (mi, 2).gt.xmax - dx_m.or.MXY (mi, 2).lt.
     &     xmin + dx_m.or.MXY (mi, 1).gt.ymax - dx_m.or.
     &     MXY (mi, 1).lt.ymin + dx_m) then
c           Marker is off the slope, no further movement, marker ignored
c           Check if the marker has been transported off the end of the slope
            if (MXY (mi, 1).gt.ymax - dx_m) then
                m_runoff = m_runoff + 1
            endif
        else
c           Determine which the cell the marker is in
            call marked_cell

c           Check whether the marker is completely buried, if not assuming all markers remain on surface
c           NB. burial has not yet been enacted in the model
c           If it not buried...
c           if (MZ (mi, 1).ge.z (mycell, mxcell)) then
c           This is included in case you wish to adjust the probability of detachment
c           burdep = 1

c           Only calculate if rmask not set to blank (-9999)
            if (rmask (mycell, mxcell).ge.0.0d0) then

c               Is there overland flow in the current cell?
                if (d (1, mycell, mxcell).gt.0.0d0) then

c                   ... then prepare for bedload/suspended flow by calculating the Reynolds number
c                   1.0d-6 is used to convert v from mm/s to m/s and d from mm to m
                    re = (1.0d-6 * v (mycell, mxcell) *
     &                   d (1, mycell, mxcell)) / viscosity

                    if (re.gt.2500.0d0) then
c                       ...if so, detachment is by the flow

c                       Determine if transport is by bedload or suspension

c                       First determine the bed shear velocity for the marked cell
c                       9.81d-3 is used to multiply by g and convert d from mm to m
                        ustar = sqrt (9.81d-3 * d (1, mycell, mxcell)
     &                          * slope (mycell, mxcell))
                        dstar = diameter (INT (MXY (mi, 3)))
     &                          * dstar_const

c                       from van Rijn (1984): Sediment transport, Part II: Suspended Load Transport
                        if (dstar.le.10) then
                            susp_crit = (4.d0 * settling_vel
     &                                  (INT (MXY (mi, 3)))) / dstar
                        else
                            susp_crit = 0.4d0 * settling_vel
     &                                    (INT (MXY (mi, 3)))
                        endif
                        if (ustar.ge.susp_crit) then
c                           ...and if so, cause transport by suspension
                            call suspended_markers
                            marker_status (mi, 1) = 5.0d0
                        else
c                           ...otherwise, transport is by bedload
                            call bedload_markers
                            marker_status (mi, 1) = 4.0d0
                        endif

                    elseif (re.lt.2500.0d0.and.r2
     &                     (mycell, mxcell).gt.0.0d0) then
c                       ...flow is present but not concentrated

                        if (re.gt.500.d0) then
c                           ...add in transitional flow conditions where detachment is by rainsplash
c                           and flow

c                           Determine the probability of detachment
                            call trans_detach_markers
c                           Determine if detachment will occur
                            call genrand_real1 (randn_detach)
                            if (randn_detach.le.p_trans) then
c                               ...marker is detached
                                call diffuse_transport_markers
                            endif
                            marker_status (mi, 1) = 3.0d0

                        else
c                           ...detachment is only via rainsplash but transport is by flow (diffuse flow)

c                           Determine the probability of detachment by rainsplash
                            call splash_detach_markers
c                           Determine if detachment will occur
                            call genrand_real1 (randn_splash)
                            if (randn_splash.le.p_splash) then
c                               ...marker is detached
                                call diffuse_transport_markers
                            endif
                            marker_status (mi, 1) = 2.0d0

                        endif

                    elseif (re.lt.2500.and.r2 (mycell, mxcell)
     &                     .eq.0) then
c                       ...transitional flow conditions with flow detachment and transport via bedload
                        call bedload_markers
                        marker_status (mi, 1) = 4.0d0

                    endif
c
                elseif (r2 (mycell, mxcell).gt.0.0d0) then
c                   ...no flow in marked cell thus detachment and transport is by rainsplash

c                   Determine the probability of detachment by rainsplash
                    call splash_detach_markers
c                   Determine if detachment will occur
                    call genrand_real1 (randn_splash)
                    if (randn_splash.le.p_splash) then
c                       ...marker is detached
                        call splash_transport_markers
                    endif
                    marker_status (mi, 1) = 1.0d0

                endif
            endif
        endif
c       endif - use for burial
       enddo

c      Output for number of markers which have been transported off the end of the slope
       marker_runoff (iter, 1) = iter * dt
       marker_runoff (iter, 2) = m_runoff / m_num

       call output_markers_xy

       return
       end
