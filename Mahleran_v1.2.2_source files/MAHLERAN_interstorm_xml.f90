
!****************************************************************
!  subroutine to calculate interstorm dynamics
!****************************************************************
subroutine MAHLERAN_interstorm_xml

use interstorm_shared_data
use shared_data
use parameters_from_xml
use time_h
use vegdynamics_shared_data

implicit none

integer :: i, j
integer :: init_continuous, istart

!****************************************************************
! parameters used in this subroutine listed in order of their occurrence
!****************************************************************
! threshold_rain is that amount of daily rainfall where MAHLERAN_storm will be started; [mm]
!****************************************************************
! variables used in this subroutine listed in order of their occurance
! local
!****************************************************************
! i is running variable for row
! j is running variable for column
!****************************************************************
! global
!****************************************************************
! current_year
! rday: running day
! rain_daily(:,:)
!****************************************************************
init_continuous = 2
istart = 1
rday = 0
r_event = 1

!open output files for interstorm soil moisture, fluxes, weathering
call output_interstorm_xml            !soil moisture and evapotranspiration
call output_interstorm_fluxes     !water, sediment and nutrient storm fluxes
!call output_weathering            !weathering processes
call output_vegdyn                !vegetation dynamics

!deal with timing
t = tstart
call calcyear           !check for leap years
Julian = dayoutsim + 1  !Julian day starts at the beginning of mstart of tstart
dayyear = dayyear + dayoutsim
write (6,*) 'Total numbers of days: ', total_days

!
!      MAIN LOOP for continuous calculation
!
write (*,*) total_days
do rday = 2, total_days
   Julian = Julian + 1      ! to include information on leap years etc.
   if (Julian.eq.dayyear + 1) then
      t = t + 1
      Julian = 1
      call calcyear
   endif
   write (6, 9999) Julian, t, rain_daily (rday)
   call calc_et
!
! if threshold exceeded: run MAHLERAN_storm with overland flow and erosion routines
!   if (rain_daily (rday).gt.threshold_rain) then
!      write (6, *) ' threshold rain exceeded: ', rain_daily (rday), ', calling set_rain_new_xml'
   if (Julian.eq.store_rainevents (1, r_event).and.t.eq.store_rainevents (2, r_event)) then
      write (6, *) ' Starting rain event: ', r_event, ', of: ',  rain_daily (rday), ' mm, calling set_rain_new_xml'
      istart = 1
!      call set_rain_new (istart)  !JW old version
      call set_rain_new_xml (istart)
      if (istart.ne.4) then
! feed current soil moisture from daily calculation into starting conditions of Mahleran_storm
         theta_0 (:,:) = sm (1, rday - 1, :, :) / depth (1)
         call initialize_values_xml (init_continuous)
!       cum_inf (:, :) = sm_1(rday-1,:,:)   !cum_inf: cumulative infiltration to present (mm)

! CHECK: is layer in interstorm calculation the same as the layer in Mahleran_storm?
         write (6, *) ' Calling MAHLERAN_storm_xml'
         call MAHLERAN_storm_xml
!  feed current soil moisture after overland flow event back into daily calculations of interstorm period

     sm (1, rday, :, :) = theta (:, :) * depth (1)

! For output of total discharge (in m3/sec) and sediment (?) after an event
         qsum_all (rday, :, :) = qsum (:, :)
     sedtotal_all (rday, :, :) = sed_tot (:, :)
!  output water, sediment and nutrient fluxes after rainstorm event
         call output_interstorm_fluxes
         r_event = r_event + 1
      else
         write (6, *) ' No heavy rainfall, not calling MAHLERAN_storm_xml'
      endif
   endif

!now called from within calc_sm
!   call calc_inf

   call calc_sm

! calculate vegetation dynamics (currently set to calculate between 1st of June and middle of November)
   if (mod (Julian, dt_vegetation).eq.0.and.rday.gt.dt_vegetation.and.Julian.ge.start_season.and.Julian.le.stop_season) then   !calculate dynamics every 14 days (dt_vegetation)
      write (*,*) 'Calculate veg dynamics on: ', t, Julian, mod (Julian, dt_vegetation)
      call veg_dyn
   endif

! Calculate areal average of soil moisture for entire model domain (within rainmask)
!JW Apr 2017 This routine is now called from calc_sm in Interstorm
!   call theta_areal

enddo


!
!       write interstorm output (time series) to file (final call will close files)
!
call output_interstorm_xml
call output_interstorm_fluxes
!
!   write final vegetation output (close file and output final cover maps)
!
call output_vegdyn
close (125)
! calculate annual statistics with output
call Annual_statistics

9999 format ('Julian Day ', i3, ' of year ', i4, ' rainfall: ', f7.3)

end
