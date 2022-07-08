!****************************************************************
!  subroutine to calculate infiltration during interstorm period
!****************************************************************

subroutine calc_inf

use shared_data
use interstorm_shared_data
use time_h

implicit none

real g1, k_mean
real sm1, sm2

integer i, j

!****************************************************************
! parameters used in this subroutine listed in order of their occurrence
!****************************************************************
! inf_rate_bare_2 is the infiltration rate from bare surface to layer 2 [mm]
! inf_rate_2 is the infiltration rate from shrub covered surface to layer 2 [mm]
! k_s is contant for saturated hydraulic conductivity [mm/h]
! d_const is diffusion coefficient [-]
!****************************************************************
! variables used in this subroutine listed in order of their occurrence
! local
!****************************************************************
! i is running variable for row [-]
! j is running variable for column [-]
! g1 is variable for a saturation function of shrub_cover/100, [%/100] values between 0 and 1
! k_mean is variable for mean hydraulic conductivity [mm/h]
!****************************************************************
! global
!****************************************************************
! shrub_cover(:,:) is value for shrub cover for row,column; has to be between 0 and 100 [%]
! inf_2(:,:,:,:)
! rday
! rain_daily
! inf_1(:,:,:,:)
! sm_1(:,:,:,:)
! theta_sat(:,:)
! depth_1
! drain_1(:,:,:,:)
! sm_2(:,:,:,:)
! depth_2
! drain_2(:,:,:,:)
! sm_1to2(:,:,:,:)
!****************************************************************
! further variables necessary, if using methods of infiltration calculations used in Tietjen et al. 2009
!
! we don't need them here as we use the assumption that the entire amount of rainfall will infiltrate, hence rainfall - inf_2 = inf_1
! if there's surface water that wouldn't infiltrate we would have overlandflow, thus MAHLERAN_storm would run
!****************************************************************
!            n_a = fc - sm_1 ! [m3/m3]
!            t_s = (n_a * s_f/((sm_sf/k_mean-1)*sm_sf))*g2 ! [h]
!            inf_sat = t_s * sm_sf ! [mm]
!            a = k_mean * (dt-t_s) [-]
!            b = f_s + 2 * n_a * s_f [-]
!            inf_tot (a/2 + (a**2 + 2* a * b)/4 * (a**2/4 + a * b + f_s**2))* g2 ! [mm/h]
!****************************************************************

!define constant to avoid duplicating calculations during every iteration and cell
!n.b. added parentheses for precedence
k_mean = ((k_s / (1.0 + (-30. / (-26.)))) ** 3 * k_s) ** 0.5 ! [mm/h], geometric mean of saturated and unsaturated hydraulic conductivity where k_unsaturated is = (k_s/(1+(-30/-26))**3)


!****************************************************************
!       calculation loops for infiltration
!****************************************************************
do i = 2, nr
   do j = 2, nc
      if (rmask (i, j).ge.0.0d0) then ! only calculate areas within catchment
!Eva: What should be calculated here, if Mahleran storm calculated the actual soil moisture
!Eva: here should be a term quantifying the part of overland flow which did not infiltrate
!maybe use directly the drain parameter of Mahleran_storm??

! g1(cs): Eq 3 saturation function of shrub cover, between 0 and 1
         g1 = (inf_rate_bare_2 + shrub_cover (i, j) / 100.) / (1 + shrub_cover (i, j) / 100.) ! [-]
! FL2: inf_2, fast infiltration from surface into layer 2 via cracks and macropores
         inf_2 (i, j) = rain_daily (rday) * inf_rate_2 * g1 ! assumption that there's no overland flow and the entire amount of daily rainfall can evaporate or infiltrate
! inf_1
         inf_1 (i, j) = rain_daily (rday) - inf_2 (i, j)
! drain_1 in [mm]
         if (sm (1, rday - 1, i, j).le.(field_cap (i, j) * depth (1))) then
        drain_1 (i, j) = 0.0
         else
            drain_1 (i, j) = sm (1, rday - 1, i, j)- field_cap (i, j) * depth (1)
         endif
! drain_2 in [mm]
         if (sm (2, rday - 1, i, j).le.(field_cap (i, j) * depth (2))) then
            drain_2 (i, j) = 0.0
         else
!Eva: shouldn't the lower layer have a lower field capacity than the upper layer?
            drain_2 (i, j) = sm (2, rday - 1, i, j) - field_cap (i, j) * depth (2)
         endif
! sm_1to2 in [mm]
! following line taken out of loop as doesn't change within it see line 65 above
!         k_mean = (k_s / (1.0 + (-30. / (-26.))) ** 3 * k_s) ** (1/2) ! [mm/h], geometric mean of saturated and unsaturated hydraulic conductivity where k_unsaturated is = (k_s/(1+(-30/-26))**3)
         if (sm (1, rday - 1, i, j) / depth (1).ne.sm (2, rday - 1, i, j) / depth (2)) then !calculate only, if there is a different moisture in both layers
            sm_1to2 (i, j) = (24. * k_mean * d_const * (depth (1) + depth (2))) * &
                             ((sm (1, rday - 1, i, j) / depth (1)) - (sm (2, rday - 1, i, j) / depth (2))) / &
                             (0.5 * (depth (1) + depth (2))) ! assumption that both layers have similar water retention curves
         else
            sm_1to2 (i, j) = 0.
         endif
      endif
   enddo
enddo

end
