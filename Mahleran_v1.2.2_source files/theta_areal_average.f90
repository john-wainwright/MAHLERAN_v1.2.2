!**********************************************************************
!  subroutine to calculate areal average of soil moisture after each day
!**********************************************************************
subroutine theta_areal

use shared_data
use interstorm_shared_data
use time_h

implicit none

real :: dummy1, dummy2

integer :: i, j, counter

! loop over entire model domain except outer boundaries and outside rainmask and
! calculate areal average of soil moisture

dummy1 = 0.0
dummy2 = 0.0
counter = 0

do i = 2, nr
   do j = 2, nc
      if (rmask (i, j).ge.0.0d0) then
         dummy1 = dummy1 + (sm (1, rday, i, j) / depth (1))
         dummy2 = dummy2 + (sm (2, rday, i, j) / depth (2))
         counter = counter + 1
      endif
   enddo
enddo
! areal average soil moisture in m3/m3 for upper soil layer
theta_areal_average (rday, 1) = dummy1 / real (counter)
! areal average soil moisture in m3/m3 for lower soil layer
theta_areal_average (rday, 2) = dummy2 / real (counter)

end
