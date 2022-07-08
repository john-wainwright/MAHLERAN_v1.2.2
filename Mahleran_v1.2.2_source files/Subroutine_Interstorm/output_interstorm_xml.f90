!****************************************************************
!  subroutine to write time series after interstorm calculations
!****************************************************************
subroutine output_interstorm_xml

use interstorm_shared_data
use shared_data
use parameters_from_xml
use time_h

implicit none
!integer rday

real :: discharge, sediment
real :: dummy (12)

integer :: i, j, k, counter

character (len = 160) :: filename

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

!t = tstart
!call calcyear
!Julian = dayoutsim + 1 !check for leap years, number of days for year

!original version copied below has hard-coding that is site specific
! output file for interstorm dynamics of soil moisture, evaporation etc.
if (rday.eq.0) then
   filename = output_folder (1:output_folder_length) // 'interstorm_moisture000.dat'
   if (iout.lt.10) then
      write (filename (22 + output_folder_length: 22 + output_folder_length), '(i1)') iout
   elseif (iout.lt.100) then
      write (filename (21 + output_folder_length: 22 + output_folder_length), '(i2)') iout
   elseif (iout.lt.1000) then
      write (filename (20 + output_folder_length: 22 + output_folder_length), '(i3)') iout
   endif
   open (124, file = filename, status = 'unknown')
   rewind (124)
   !
   !sm_1 and sm_2 are average soil moistures of outflow cells (defined here)
   !theta_av_1 and theta_av_2 are average soil moistures of all catchment cells (defined in theta_areal_average)
   write (124,'(a)') 'rday Julian year rain[mm] sm_1_outflow[%] sm_2_outflow[%] theta_av_1[%] theta_av_2[%] et_pot[mm] '//&
                     'et_1[mm] et_2[mm] inf_1[mm] inf_2[mm] drain_1[mm] drain_2[mm] '// &
                     'sm_1to2[mm] sm_1[mm] sm_2[mm] discharge[l] sediment[kg]'

! write data to file
elseif (rday.gt.0.and.rday.ne.total_days + 1) then
   counter = 0
   dummy (:) = 0.
   discharge = 0.
   sediment = 0.
   ! calculate average or sum values of all cells exiting the catchment
   do i = 2, nr
      do j = 2, nc
         if ((aspect (i, j).eq.1.and.rmask (i, j).gt.0.0d0.and.rmask (i - 1, j).lt.0.0d0).or. &
             (aspect (i, j).eq.2.and.rmask (i, j).gt.0.0d0.and.rmask (i, j + 1).lt.0.0d0).or. &
             (aspect (i, j).eq.3.and.rmask (i, j).gt.0.0d0.and.rmask (i + 1, j).lt.0.0d0).or. &
             (aspect (i, j).eq.4.and.rmask (i, j).gt.0.0d0.and.rmask (i, j - 1).lt.0.0d0)) then
            !average values for all outflowing cells not just single cell as per hard-coded version
            dummy (1) = dummy (1) + sm (1, rday, i, j)
            dummy (2) = dummy (2) + sm (2, rday, i, j)
            dummy (3) = dummy (3) + et_pot (i, j)
            dummy (4) = dummy (4) + et_1 (i, j)
            dummy (5) = dummy (5) + et_2 (i, j)
            dummy (6) = dummy (6) + inf_1 (i, j)
            dummy (7) = dummy (7) + inf_2 (i, j)
            dummy (8) = dummy (8) + drain_1 (i, j)
            dummy (9) = dummy (9) + drain_2 (i, j)
            dummy (10) = dummy (10) + sm_1to2 (i, j)
            dummy (11) = dummy (11) + sm (1, rday, i, j)
            dummy (12) = dummy (12) + sm (2, rday, i, j)
            counter = counter + 1
            discharge = discharge + qsum_all (rday, i, j)     !for total water fluxes
            sediment = sediment + sedtotal_all (rday, i, j)   !for total sediment fluxes
         endif
      enddo
   enddo
   do k = 1, 12
      dummy (k) = dummy (k) / real (counter)
   enddo
   dummy (1) = dummy (1) / depth(1) * 100.
   dummy (2) = dummy (2) / depth(2) * 100.
   write (124, 9998) rday, Julian, t, rain_daily (rday), dummy (1), dummy (2), &
             theta_areal_average (rday, 1) * 100., theta_areal_average (rday, 2) * 100., &
                     (dummy (k), k = 3, 12), discharge, sediment
endif

!close file at end of simulation
if (rday.eq.total_days + 1) then
   close (124)
endif

!N.b. if this format statement changes, the one in Annual_statistics must be changed to match
9998   format (i6, 1x, i3, 1x, i4, 1x, f7.3, 16(1x, e10.4))

end


!original version below:
! output file for interstorm dynamics of soil moisture, evaporation etc.
!filename = output_folder (1:output_folder_length) // 'interstorm_h2o.txt'
!open (18, file =  filename, status = 'unknown')
!rewind(18)
!write (18, *) "rday ", "Julian ", "t ", "rain_daily(rday)[mm] ", "et_pot(rday,61,6)[mm] ", "et_1(rday,61,6)[mm] ", &
!             "et_2(rday,61,6)[mm] ", "inf_2(rday,61,6)[mm] ", "inf_1(rday,61,6)[mm] ", "drain_1(rday,61,6)[mm] ", &
!             "drain_2(rday,61,6)[mm] ", "sm_1to2(rday,61,6)[mm] " , "sm_1(rday,61,6)[mm] " , "sm_2(rday,61,6)[mm] ", &
!             "sm_1(rday,61,6)[m3/m3] ", "sm_2(rday,61,6)[m3/m3]", "threshold_rain"
!
! output file for sum values of water, sediment etc. (daily values of fluxes at the end of every day)
!filename = output_folder (1:output_folder_length) // 'interstorm_sums.txt'
!open (19, file =  filename, status = 'unknown')
!rewind(19)
!write (19,*) 'rday ', 'Julian ', 't ', 'rain_daily(rday) ', 'sm_1(rday,61,10)/depth_1 ', &
!             'sm_2(rday,61,10)/depth_2 ', 'qsum_all(rday,61,10) ', 'sedtotal_all(rday,61,10) ', &
!             'theta_areal_average(rday,1) ', 'theta_areal_average(rday,2) ', 'discharge ', 'sediment'
!
!do rday = 1, total_days  !
!   write (18, '(3i6,15f14.3)') rday, Julian, t, rain_daily (rday), et_pot (rday, 61, 6), et_1 (rday, 61, 6), et_2 (rday, 61, 6), &
!               inf_2 (rday, 61, 6), inf_1 (rday, 61, 6), drain_1 (rday, 61, 6), drain_2 (rday, 61, 6), sm_1to2 (rday, 61, 6), &
!               sm_1 (rday, 61, 6), sm_2 (rday, 61, 6), sm_1 (rday, 61, 6) / depth_1, sm_2 (rday, 61, 6) / depth_2, threshold_rain
!
! calculate sum values of all cells at the lower end of the plot
!   discharge = 0.
!   sediment = 0.
!   do j = 2, 21
!      if (aspect (61, j).eq.3) then ! only include those cells whose flow goes out of the plot (rather than to the side)
!         discharge = discharge + qsum_all (rday, 61, j)     !for total water fluxes
!         sediment = sediment + sedtotal_all (rday, 61, j)   !for total sediment fluxes
!      endif
!   enddo
!
!   write (19,'(3i6,9f14.3)') rday, Julian, t, rain_daily (rday), sm_1 (rday, 61, 10) / depth_1, &
!                        sm_2 (rday, 61, 10) / depth_2, qsum_all (rday, 61, 10), sedtotal_all (rday, 61, 10), &
!                             theta_areal_average (rday, 1), theta_areal_average (rday, 2), discharge, sediment
!
!   Julian = Julian + 1     ! to inlcude information on leap years etc.
!   if (Julian.eq.(dayyear + dayoutsim + 1)) then
!      t = t + 1
!      Julian = 1
!      call calcyear
!   endif
!enddo
!close(18)
!close(19)
!
!
