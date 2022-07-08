!****************************************************************
!  subroutine to calculate vegetation mortality
!****************************************************************
subroutine mortality

use vegdynamics_shared_data
use interstorm_shared_data
use shared_data

implicit none

real :: mean_sm, U, sustW, lap, dummy1, dummy2, mort_l

integer :: i, j, k, n, layer, counter, duration_season, erosion_veg_feedback

!sm(layer,rday,i,j) in mm
!c_veg(status 1 or 2,no. of species,i,j) in %
!sustW in mm, water availability for one vegetation type for entire growing season
!root(layer, no_species) Fraction of grass or shrub roots in upper and lower layer (dimensionless)
!uptake(k), k=1, no_species: potential uptake rate per unit grass or shrub cover in mm/y



!calculated mortality occurs at the end of the growing season as a result of unfavourable soil moisture conditions during the whole season.
duration_season = stop_season - start_season
mort (:, :, :) = 0.

do i = 2, nr
   do j = 2, nc
      if (rmask (i, j).ge.0.0d0) then
         do k = 1, no_species       !loop through number of species, at the moment: 1: grass & 2 shrub
            if (c_veg (1, k, i, j).gt.0.) then
               do layer = 1, 2      !loop through the number of layers, at the moment: 2
!check mean seasonal water availability (for Sevilleta: July-Mitte Oktober, i.e. 110 days)
                  mean_sm = 0.
                  do n = rday - duration_season + 1, rday
                     mean_sm = mean_sm + sm (layer, n, i, j)
                  enddo
                  mean_sm = mean_sm / real (duration_season)    !mean soil moisture of previous 14 days in layer 1
                  mean_sm = mean_sm / depth (layer) !mean soil moisture (m3/m3)
!calculate seasonal soil moisture availability (Eq 3, Tietjen 2010)
                  if (mean_sm.le.theta_WP (k)) then
                     sustW = 0.
                  else
                     sustW = (mean_sm - theta_WP (k)) * depth (layer)   !sustW in mm    Equ. 5
                  endif
!calculate relative water uptake per unit cover U (dimensionless)
                  dummy1 = uptake (1) * root (layer, 1) * c_veg (1, 1, i, j) + &
                           uptake (2) * root (layer, 2) * c_veg (1, 2, i, j)
                  U = (uptake (k) * root (layer, k)) / dummy1   !Equ. 4
!                  write(*,*) U, sustW, U*sustW, min(U*sustW,1.), (root(1,k)+root(2,k))
!calculate mortality rate:
                  mort_l = mr (k) * c_veg (1, k, i, j) * &
                           (1. - min (U * sustW, 1.)) * (root (layer, k) / (root (1, k) + root (2, k)))
               enddo            !layer (no. of soil layer)
!calculate mortality: sum of mort in layer 1 and layer 2
               mort (k, i, j) = mort (k, i, j) + mort_l         !vegetation growth is split into the sum of growth induced by water in the upper and lower soil layer
            endif
         enddo      !k (no. of species)
         mort (:, i, j) = mort (:, i, j) * duration_season / 365.   !time scaling of mort
      endif   !rmask greater zero
   enddo        !j
enddo       !i

!include simple erosion-vegetation-link
erosion_veg_feedback=1  !JW Apr 2017 make into a parameter
! checks the total detachment rate in a year, it will be set to zero at the end of veg. mort. calculation
if (erosion_veg_feedback.eq.1) then
   do i = 2, nr
      do j = 2, nc
         if (rmask (i, j).ge.0.0d0) then
            if (net_erosion_season (i, j).gt.0.01.and.net_erosion_season (i, j).lt.0.1) then ! erosion of a season bigger than 0.03 mm
               mort (1, i, j) = 0.10  !JW Apr 2017 why only feedback on grass?
               disp (1, i, j) = 0.
            elseif (net_erosion_season (i, j).ge.0.1) then
               mort (1, i, j) = 0.20
               disp (1, i, j) = 0.
            endif
         endif
      enddo
   enddo
endif

!reseat detach_total_season to zero for the next year
net_erosion_season (:, :) = 0.

end



