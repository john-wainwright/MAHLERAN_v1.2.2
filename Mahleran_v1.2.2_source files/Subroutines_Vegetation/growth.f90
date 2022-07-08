!****************************************************************
!  subroutine to calculate vegetation growth
!****************************************************************
subroutine growth

use vegdynamics_shared_data
use interstorm_shared_data
use shared_data

implicit none

real :: mean_sm, U, avW, lap, dummy1, dummy2, gr_l

integer :: i, j, k, n, layer, counter

! sm(layer,rday,i,j) in mm
! c_veg(status 1 or 2,no. of species,i,j) in %
! avW in mm, water availability for one vegetation type
!root(layer, no_species) Fraction of grass or shrub roots in upper and lower layer (dimensionless)
!uptake(k), k=1, no_species: potential uptake rate per unit grass or shrub cover in mm/y
!growth (%), gr(no. of species,i,j)
!gr_l, growth related to upper or lower layer
!lap: maximal extent to which grass and shrubs can overlap, set to 20%
!U (dimensionless): fraction of the available water that can be taken up per unit cover

lap = 20

gr (:, :, :) = 0.

do i = 2, nr
   do j = 2, nc
      if (rmask (i, j).ge.0.0d0) then
         !TODO update so that no_species can be any value
         do k = 1, no_species       !loop through number of species, at the moment: 1: grass & 2: shrub
            if (c_veg (1, k, i, j).gt.0.) then
               do layer = 1, 2      !loop through the number of layers, at the moment: 2
!check mean soil moisture of previous 14 days in both layers
                  mean_sm = 0.
                  do n = rday - dt_vegetation + 1, rday
                     mean_sm = mean_sm + sm (layer, n, i, j)
                  enddo
                  mean_sm = mean_sm / dt_vegetation !mean soil moisture of previous 14 days in layer 1
                  mean_sm = mean_sm / depth (layer) !mean soil moisture (m3/m3)
!calculate soil moisture availability (Eq 3, Tietjen 2010)
                  if (mean_sm.le.theta_WP (k)) then
                     avW = 0.
                  else
                     avW = (mean_sm - theta_WP (k)) * depth (layer) !avW in mm
                  endif
!calculate relative water uptake per unit cover U (dimensionless)
                  dummy1 = uptake (1) * root (layer, 1) * c_veg (1, 1, i, j) + &
                           uptake (2) * root (layer, 2) * c_veg (1, 2, i, j)
                  if (dummy1.gt.0) then !check that this does not get below zero
                     U = (uptake (k) * root (layer, k)) / dummy1    !Equ. 4
                  else
                     U = 0.
                  endif
! calculate plant growth
                  if (k.eq.1) then
                     n = 2
                  endif
                  if (k.eq.2) then
                     n = 1
                  endif
                  dummy1 = c_veg (1, n, i, j) - lap / 100
                  if (dummy1.lt.0) then
                     dummy1 = 0                 !potential overlapping should not be below zero
                  endif
                  dummy2 = 1 - (c_veg (1, k, i, j) / (1 - dummy1))
                  gr_l = min (U * avW, 1.) * r (k)* c_veg (1, k, i, j) * dummy2         !Eq. 2, Tietjen 2010
                  gr (k, i, j) = gr (k, i, j) + gr_l            !vegetation growth is split into the sum of growth induced by water in the upper and lower soil layer
               enddo            !layer (no. of soil layer)
            endif
            if (gr (k, i, j).lt.0.) then
               gr(k,i,j)=0.
            endif
     enddo      !k (no. of species)
      endif   !rmask greater zero
   enddo        !j
enddo       !i

!scale growth rates to actual time step (former calculation refered to water uptake for the entire year)
gr (:, :, :) = gr (:, :, :) * dt_vegetation / 365.


end

