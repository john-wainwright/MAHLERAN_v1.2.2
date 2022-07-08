!****************************************************************
!  subroutine to write time series after interstorm calculations
!****************************************************************
subroutine output_interstorm


use interstorm_shared_data
use shared_data

use time_h
implicit none
!integer rday

real  discharge, sediment
integer i,j
!****************************************************************
! variables used in this subroutine listed in order of their occurrence
! local
!****************************************************************
! n is running variable for year [-]
! n is running variable for day [-]
!****************************************************************
! global
!****************************************************************
! pfadout_interstorm
! pfado_interstorm
! no_years
! tstart
! et_pot(:,:,:,:)
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


!****************************************************************
!  output for potential evapotranspiration
!****************************************************************

!open (7, FILE=pfadout_interstorm(1:pfado_interstorm)//'et_pot.txt' , status = 'unknown')
!rewind(7)

!do rday=1,total_days
!   write (7,*) et_pot(rday,9,9)
!enddo

!  close(7)

!write(*,*) 'Potential Evaporation time series written'


!
!****************************************************************
!  output for all interstorm moisture data in one file
!****************************************************************


t = tstart
call calcyear
Julian=dayoutsim+1  !check for leap years, number of days for year

! output file for interstorm dynamics of soil moisture, evaporation etc.
open (18, FILE=pfadout_interstorm(1:pfado_interstorm)//'interstorm_h2o.txt' , status = 'unknown')
rewind(18)
write (18,*) "rday ","Julian ","t ","rain_daily(rday)[mm] ", "et_pot(rday,61,6)[mm] ", "et_1(rday,61,6)[mm] ", &
             "et_2(rday,61,6)[mm] ", "inf_2(rday,61,6)[mm] ", "inf_1(rday,61,6)[mm] ", "drain_1(rday,61,6)[mm] ", &
             "drain_2(rday,61,6)[mm] ", "sm_1to2(rday,61,6)[mm] " , "sm_1(rday,61,6)[mm] " , "sm_2(rday,61,6)[mm] ", &
             "sm_1(rday,61,6)[m3/m3] ", "sm_2(rday,61,6)[m3/m3]", "threshold_rain"

! output file for sum values of water, sediment etc. (daily values of fluxes at the end of every day)
open (19, FILE=pfadout_interstorm(1:pfado_interstorm)//'interstorm_sums.txt' , status = 'unknown')
rewind(19)
    write (19,*) 'rday ', 'Julian ', 't ', 'rain_daily(rday) ', 'sm_1(rday,61,10)/depth_1 ', &
    'sm_2(rday,61,10)/depth_2 ', 'qsum_all(rday,61,10) ', 'sedtotal_all(rday,61,10) ', &
   'theta_areal_average(rday,1) ', 'theta_areal_average(rday,2) ', 'discharge ', 'sediment'

do rday=1,total_days  !
   write (18,'(3i6,15f14.3)') rday, Julian, t, rain_daily (rday), et_pot(rday,61,6), et_1(rday,61,6), et_2(rday,61,6), &
                              inf_2(rday,61,6), inf_1(rday,61,6), drain_1(rday,61,6), drain_2(rday,61,6), sm_1to2(rday,61,6), &
                              sm_1(rday,61,6), sm_2(rday,61,6), sm_1(rday,61,6)/depth_1, sm_2(rday,61,6)/depth_2, threshold_rain

! calculate sum values of all cells at the lower end of the plot
    discharge=0; sediment=0
    do j=2,21
        if (aspect(61,j).eq.3) then ! only include those cells whose flow goes out of the plot (rather than to the side)
            discharge=  discharge +qsum_all(rday,61,j)  !for total water fluxes
            sediment= sediment + sedtotal_all(rday,61,j)    !for total sediment fluxes
        endif
    enddo

    write (19,'(3i6,9f14.3)') rday, Julian, t, rain_daily (rday), sm_1(rday,61,10)/depth_1, &
    sm_2(rday,61,10)/depth_2, qsum_all(rday,61,10), sedtotal_all(rday,61,10), &
   theta_areal_average(rday,1), theta_areal_average(rday,2), discharge, sediment

    Julian=Julian+1     ! to inlcude information on leap years etc.
    if (Julian.eq.dayyear+dayoutsim+1) then
        t=t+1
        Julian=1
        call calcyear
    endif
enddo
close(18)
close(19)




end



