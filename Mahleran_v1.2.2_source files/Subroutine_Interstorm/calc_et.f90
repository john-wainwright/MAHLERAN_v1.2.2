!**********************************************************************
!  subroutine to calculate evapotranspiration during interstorm period
!**********************************************************************
subroutine calc_et

use shared_data
use interstorm_shared_data
use time_h

implicit none

real :: dist_earth_sun, solar_declin, sunset_angle, rad_extraterr

integer i, j


!****************************************************************
! parameters used in this subroutine listed in order of their occurrence
!****************************************************************
! latitude is latitude of the site in radians
! lat_deg when latitude not needed in rad (which is 0.593) in calculations, but in degree
! rw is residual water content value, soil type specific [m**3/m**3]
! wsc is water content of beginning stomatal closure [m**3/m**3]
! et_red_fac is constatnt for et reduction due to crusting, desert pavement [-]
!****************************************************************
! variables used in this subroutine listed in order of their occurrence
! local
!****************************************************************
! i is running variable for row [-]
! j is running variable for column [-]
! dist_earth_sun is relative distance between earth and sun depending on Julian day [-]
! solar_declin is solar declination depending on Julian day [rad]
! sunset_angle is sunset hour angle depending on latitude [rad]
! rad_extraterr is extraterrestrial radiation depending on sunsat angle and solar declination [mm/d]
!****************************************************************
! global
!****************************************************************
! nr
! nc
! et_pot(:,:,:)
! rday
! Tmean(:,:)
! Tmax(:,:)
! Tmin(:,:)
! slope_radiant(:,:)
! aspect_factor(:,:) is aspect factor [-], 4 types; assigned in interstorm_initialise
! sm_1(:,:,:)
! depth_1
! et_1(:,:,:)
! veg(:,:) is variable for vegetation cover [%], no differentiation whether grass or shrub yet; from MAHLERAN_storm
! sm_2(:,:,:)
! depth_2
! et_2(:,:,:)
!****************************************************************


dist_earth_sun = 1. + 0.033 * cos ((2. * pi / 365.) * Julian)
solar_declin = 0.4093 * sin ((2. * pi / 356.) * Julian - 1.405)
sunset_angle = acos (-tan (latitude) * tan (solar_declin))
rad_extraterr = 15.392 * dist_earth_sun * (sunset_angle * sin (latitude) * sin (solar_declin) + cos (latitude) * &
                cos (solar_declin) * sin (sunset_angle))

!write(*,*) dist_earth_sun, solar_declin, sunset_angle, rad_extraterr ! for testing in command bar


!****************************************************************
!       calculation loops for evapotranspiration
!****************************************************************
do i = 2, nr
   do j = 2, nc
      if (rmask (i, j).ge.0.0d0) then ! only calculate areas within catchment

! et_pot: potential evapotranspiration
       
! Eva's ECOGEOM version defines depth (1) and depth (2) here based on input files
! This code removed, and depths are simply defined in read_interstorm_parameters as:       
       !depth (1) = depth_of_upper_layer                    ! read in value for depth of layer 1 in mm
       !depth (2) = depth_of_lower_layer                    ! read in value for depth of layer 2 in mm

         et_pot (i, j) = 0.0023 * (Tmean (rday) + 17.8) * &
                         (Tmax (rday) - Tmin (rday)) ** 0.5 * &
                         rad_extraterr * cos (slope_radiant (i,j)) * aspect_factor(i,j)  ! [mm]

! et_1
         if ((sm (1, rday - 1, i, j) / depth (1)).le.rw) then
            et_1 (i, j) = 0.0
         elseif (rw.lt.(sm (1, rday - 1, i, j) / depth (1)).and.(sm (1, rday - 1, i, j) / depth (1)).le.wsc) then
            et_1 (i, j) = et_pot (i, j) * ((sm (1, rday - 1, i, j) / depth (1)) / wsc) ** 2 * &
                          (1.2 - 0.2 * (veg (i, j) / 100.)) * et_red_fac
         else
            et_1 (i, j) = et_pot (i, j) * (1.2 - 0.2 * (veg (i, j) / 100.)) * et_red_fac
         endif

! et_2
         if ((sm (2, rday - 1, i, j) / depth (2)).le.rw) then
            et_2 (i, j) = 0.0
         elseif ((rw.lt.sm (2, rday - 1, i, j) / depth (2)).and.(sm (2, rday - 1, i, j) / depth (2)).le.wsc) then
            et_2 (i, j) = et_pot (i, j) * ((sm (2, rday - 1, i, j) / depth (2)) / wsc) ** 2 * (veg (i, j) / 100.)
         else
            et_2 (i, j) = et_pot (i, j) * (veg (i, j) / 100.)
         endif

      endif
   enddo
enddo


end