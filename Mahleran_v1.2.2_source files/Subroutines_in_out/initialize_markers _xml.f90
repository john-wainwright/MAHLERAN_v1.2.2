!****************************************************************
!  subroutine to initialise variables at start of run
!
!  JC May 2011
!  JW  converted to account for xml input changes May 2014
!
!****************************************************************

subroutine initialize_markers_xml

use shared_data
use parameters_from_xml
use mt95

implicit none

integer :: i, k

do mi = 1, mnum
   MXY (mi, 1) = 0.0d0
   MXY (mi, 2) = 0.0d0
   MXY (mi, 3) = 0.0d0
   MZ (mi, 1) = 0.0d0
   motion_susp (mi) = 0.0d0
   vel_susp (mi) = 0.0d0
   motion_bl (mi) = 0.0d0
   rest_bl (mi) = 0.0d0
   status_bl (mi) = 0.0d0
   vel_bl (mi) = 0.0d0
   motion_diffuse (mi) = 0.0d0
   vel_diffuse (mi) = 0.0d0
   marker_status (mi, 1) = 0.0d0
enddo

do i = 1, 2
   do iter = 1, nit
      marker_runoff (iter, i) = 0.0d0
   enddo
enddo

do i = 1, nr2
   do k = 1, nc2
      detach_tot_m (i, k) = 0.0d0
      do phi = 1, 6
     detach_soil_m (phi, i, k) = 0.0d0
      enddo
   enddo
enddo

!    Set up the seed value for the random number generators based on the processor clock
call system_clock (count=seed)
!    Initialize the state of the Mersenne Twister random number generator
call genrand_init (seed)
!    Initialize the state of the other random number generators
! JW May 2014 -- now commented out, as these used elsewhere, so initialization is from the MAIN program
!    call ZBQLINI (seed)

return
end
