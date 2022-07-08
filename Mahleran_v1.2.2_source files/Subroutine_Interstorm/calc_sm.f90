!****************************************************************
!  subroutine to calculate soil moisture during interstorm period
!****************************************************************

subroutine calc_sm

use shared_data
use interstorm_shared_data
use time_h

implicit none

real :: theta_av1, theta_av2

integer :: i, j

!****************************************************************
! variables used in this subroutine listed in order of their occurrence
! local
!****************************************************************
! i is running variable for row [-]
! j is running variable for column [-]
!****************************************************************
! global
!****************************************************************
! nr
! nc
! sm_1(:,:,:,:)
! current_year
! inf_1(:,:,:,:)
! sm_1to2(:,:,:,:)
! drain_1(:,:,:,:)
! et_1(:,:,:,:)
! sm_2(:,:,:,:)
! inf_2(:,:,:,:)
! rain_daily
! drain_2(:,:,:,:)
! et_2(:,:,:,:)
!****************************************************************

! calculate infiltration in upper two layer
call calc_inf

!****************************************************************
!       calculation loops for soil moisture
!****************************************************************
do i = 2, nr
   do j = 2, nc
      if (rmask (i, j).ge.0.0d0) then ! only calculate areas within catchment
         if (r_event.gt.2.and.Julian.eq.store_rainevents(1,r_event-1).and.t.eq.store_rainevents(2,r_event-1)) then
     !!! don't calculate sm in upper layer for days without overland flow!
             continue  !do nothing -- marker to clarify code
         else
! sm_1 [mm] per timestep, differential equation with dt=1 day
            sm (1, rday, i, j) = sm (1, rday - 1, i, j) + inf_1 (i, j) - sm_1to2 (i, j) - &
                         drain_1 (i, j) - et_1 (i, j)
         endif
!if soil moisture gets negative, set to zero
         if (sm (1, rday, i, j).lt.theta_min) then !JW changed from <= to .lt. as no need to change if already equal
            sm (1, rday, i, j) = theta_min
         endif
!if soil moisture larger than theta_sat, set to theta_sat
         if (sm (1, rday, i, j).ge.theta_sat (i, j) * depth (1)) then
            sm (1, rday, i, j) = theta_sat (i, j) * depth (1)
     endif
! sm_2
         sm (2, rday, i, j) = sm (2, rday - 1, i, j) + inf_2 (i, j) + sm_1to2 (i, j) + &
                              drain_1 (i, j) - drain_2 (i, j) - et_2 (i, j)
!if soil moisture gets negative, set to zero
         if (sm (2, rday, i, j).lt.theta_min) then !JW changed from <= to .lt. as no need to change if already equal
            sm (2, rday, i, j) = theta_min
         endif
!if soil moisture larger than theta_sat, set to theta_sat
     if (sm (2, rday, i, j).ge.theta_sat (i, j) * depth (2)) then
            sm (2, rday, i, j) = theta_sat (i, j) * depth (2)
         endif
      endif
   enddo
enddo

! calculate areal average of soil moisture for entire model domain (within rainmask)
call theta_areal (rday, theta_av1, theta_av2)
theta_areal_average (rday, 1) = theta_av1
theta_areal_average (rday, 2) = theta_av2

! write moisture data to file
call output_interstorm_xml

end
