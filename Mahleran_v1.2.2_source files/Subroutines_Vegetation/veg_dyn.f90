!****************************************************************
!  subroutine to calculate vegetation dynamics
!****************************************************************
subroutine veg_dyn

use vegdynamics_shared_data
use interstorm_shared_data
use shared_data
use time_h

implicit none

integer :: i, j, k, overgrazing

!Implementation based on B. Tietjen et al, Ecohydrol. 3:226-237 (2010)


! c_veg cover in fraction c_veg(status 1 or 2,grass or shrub,i,j) (e.g. 0.25 for 25 %)
!cover(i,j) as used in Mahleran in %    (e.g. 25 for 25 %)

! growth is calculated be-weekly in the growing season
call growth

!mortality and dispersal is calculated at the end of the growing season (set to 1st of November)
if (Julian.eq.stop_season) then
   call dispersal
   call mortality
endif

do i = 2, nr
   do j = 2, nc
      if (rmask (i, j).ge.0.0d0) then
!calculate new vegetation cover
         do k = 1, no_species
            c_veg (2, k, i, j) = c_veg (1, k, i, j) + (gr (k, i, j) - mort (k, i, j) + disp (k, i, j))  !Equ. 1, growth is scaled to beweekly growth by multiplying it with 14/365
            if (c_veg (2, k, i, j).lt.0) then
               c_veg (2, k, i, j) = 0.      !vegetation cover shouldn't be below zero
            endif
            if (c_veg (2, k, i, j).gt.1.) then
               c_veg (2, k, i, j) = 1.  !vegetation cover can't grow endlessly
            endif
         enddo
      endif
   enddo
enddo


!add overgrazing scenarios: reduction of grass cover during rainy season:
!TODO make overgrazing into an input parameter
overgrazing = 2
if (overgrazing.eq.1) then
   do i = 2, nr
      do j = 2, nc
         if (rmask (i, j).ge.0.0d0) then
            if (t.ge.1997.and.t.lt.1998) then
! include soil-feedback by considering the effects of trampeling on infiltration rate (i.e. choose the smaller one for all cells)
!           ksave(2)=ksave(1)
               if (c_veg (2, 1, i, j).ge.0.3) then
                  c_veg (2, 1, i, j) = c_veg (2, 1, i, j) - 0.025
               endif
               if (c_veg (2, 1, i, j).lt.0) then
                  c_veg (2, 1, i, j) = 0.       !vegetation cover shouldn't be below zero
               endif
            endif
         endif
      enddo
   enddo
endif

!assign new veg cover for Mahleran storm/interstorm here (include overlapping bits)
grass_cover (:, :) = c_veg (2, 1, :, :) * 100.
shrub_cover (:, :) = c_veg (2, 2, :, :) * 100.

cover (:, :) = (c_veg (2, 1, :, :) + c_veg (2, 2, :, :)) * 100.
do i = 2, nr
   do j = 2, nc
      if (rmask (i, j).ge.0.0d0.and.t.ge.2005) then !JW Apr 2017 not clear why t>=2005 hard-coded here
         if (cover (i, j).gt.100.) then
            cover (i, j) = 100.     !overlap possible in veg routines
         endif
      endif
   enddo
enddo

!re-set veg-cover for next time step
c_veg(1,:,:,:)=c_veg(2,:,:,:)

!output veg cover
call output_vegdyn

gr (:, :, :) = 0.
mort (:, :, :) = 0.
disp (:, :, :) = 0.

end



