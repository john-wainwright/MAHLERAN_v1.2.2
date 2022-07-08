!**********************************************************************
!  subroutine to calculate areal average of soil moisture after each day
!**********************************************************************
subroutine theta_areal (day, moisture1, moisture2)

use shared_data
use interstorm_shared_data
use time_h

implicit none

real :: dummy1, dummy2, moisture1, moisture2

integer :: i, j, counter, day

! loop over entire model domain except outer boundaries and outside rainmask and
! calculate areal average of soil moisture
! day does not have to be the same as rday, e.g. for previous sm calculations

dummy1 = 0
dummy2 = 0
counter = 0

do i = 2, nr
   do j = 2, nc
      if (rmask (i, j).ge.0.0d0) then
         !hardcoded definitions of depth (1) and depth (2) removed here
         dummy1 = dummy1 + (sm (1, day, i, j) / depth (1))
         dummy2 = dummy2 + (sm (2, day, i, j) / depth (2))
         counter = counter + 1
      endif
   enddo
enddo

! areal average soil moisture in m3/m3 for upper soil layer
moisture1 = dummy1 / real (counter)
! areal average soil moisture in m3/m3 for lower soil layer
moisture2 = dummy2 / real (counter)

end
