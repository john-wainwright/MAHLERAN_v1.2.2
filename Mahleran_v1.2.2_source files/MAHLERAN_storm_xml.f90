!****************************************************************
!  subroutine to calculate storm dynamics (main loop)
!  corresponds to Version 1.01.6 (second half of main programme)
!  modified from Eva's version to include Caspar's latest version
!  with topographic updates May 2014
!****************************************************************
subroutine MAHLERAN_storm_xml

use shared_data
use interstorm_shared_data
use time_h
use parameters_from_xml

implicit none

double precision :: a
double precision :: b
double precision :: sum

!integer :: nit  !declared in shared_data for use in Marker routines
integer :: istart, ifinish
integer :: init, ireset
integer :: nt
integer :: num
integer :: i
integer :: k
integer *4 time_array (8)

logical :: end_storm

!      call intrinsic f90 subroutine date_and_time
call date_and_time (values = time_array)
!      time_array(1)    year
!      time_array(2)    month of the year
!      time_array(3)    day of the month
!      time_array(4)    time offset with respect to UTC in minutes
!      time_array(5)    hour of the day
!      time_array(6)    minutes of the hour
!      time_array(7)    seconds of the minute
!      time_array(8)    milliseconds of the second

ifinish = 0
istart = 1
init = 1
ireset = 2
iter_variable=0
nt = 0
sum = 0
a = 1
b = 1
end_storm = .FALSE.

!JW May2017 write (6, *) 'rain_type = ', rain_type, ', rf_mean = ', rf_mean
!JW May2017 call set_rain_xml (istart)
if (rain_type.eq.1.or.rain_type.eq.3) then
   nit = int (((stormlength / dt) * 3.) + .5)
elseif (rain_type.eq.4) then
   nit = 100000 !in the main loop the continuous rainfile will be checked and end automatically found
else
   nit = int ((stormlength / dt) + .5)
endif
if (rain_type.eq.4) then
   if (f_hydrofile) then
      write (57, 9998) Julian, t
   endif
   if (f_sedfile) then
      write (58, 9998) Julian, t
   endif
   if (f_nutrientfile) then
      write (60, 9998) Julian, t
   endif
endif
!
!   initialize rainfall
!
istart = 1
if (rain_type.eq.1.or.rain_type.eq.2.or.rain_type.eq.3) then
   call set_rain_xml (istart)
elseif (rain_type.eq.4) then
   call set_rain_new_xml (istart)
! don't run Mahleran storm, if there is no heavy rain on that day
   if (istart.eq.4) then
      nit = 1
   endif
endif
write (6, *) ' About to start Mahleran Storm, iout = ', iout
!
!
!
!   MAIN LOOP
!
!  The following loop iterates for each time step of simulation (iter)
!  nit is the maximum number of iterations for a particular run
num = 0
do iter = 1, nit
   write (6, 9999) iter, rval * 3600., dt, t, Julian, istart, q_plot * dx, sed_plot
!   write (6, *) ' Starting iteration ', iter , ' rain intensity: ', r2(9,9), ' time step: ', dt
!   write(52, 900) dt * iter

   call infilt

   call accumulate_flow

   call route_water

!
!   routines for sediment and chemistry
!
   call route_sediment_xml
!   write (6, *) 'Returned from route_sediment_xml'
   call route_chemistry
!   write (6, *) 'Returned from route_chemistry'

!   following additions for dynamic flow and rainfall
!JWFeb05  only needed if iroute=6, otherwise repeated sorting will
!JWFeb05     cause significant increase in run time
   if (iroute.eq.6) then
!             call dynamic_topog_attrib
! Jul 2013 update from Caspar Hewett from 1.01.6
      call dynamic_topog_attribute
!      write (6, *) 'Returned from dynamic_topog_attribute'
   endif
!
!   turn off rain at end of event (const. rainfall case)
!
   if (rain_type.eq.1.and.(dble (iter) * dt).eq.stormlength) then
      call set_rain_xml (ifinish)
   else if (rain_type.eq.2) then
!
!JWDec13 multiply through by dt to stop issues with cases where dt <> 1
!
      if ((iter * dt).lt.int (stormlength)) then
         call set_rain_xml (istart)
      else if ((iter * dt).eq.int (stormlength)) then
         call set_rain_xml (ifinish)
      endif
   elseif (rain_type.eq.3) then
      call set_rain_xml (istart)
   elseif (rain_type.eq.4.and.istart.ne.3) then
!      call set_rain_new (istart)   !JW old version
      call set_rain_new_xml (istart)
   elseif (rain_type.eq.4.and.istart.eq.3) then
      end_storm = .TRUE.  !JW May2017 delay exit from loop until updates have been carried out for the timestep
   endif
!   write (6, *) 'Returned from set_rain_xml'
!
!   add any dynamic output
!
   call output_hydro_data_xml
!   write (6, *) 'Returned from output_hydro_data_xml'
!
!  JW Feb 17 added for COST exercise
!
   if (within_storm_output) then
      if (mod ((iter * dt), within_storm_interval).eq.0) then
          call output_maps_within_storm_xml
      endif
   endif
!
!   update flow attributes
!
   if (update_topography) then
      num = num + 1
      if (num.eq.nt_top_up) then
         call update_top_surface
         num = 0
      else if ( iter.eq.nit ) then
         call update_top_surface
      endif
   endif
   call update_water_flow
!   write (6, *) 'Returned from update_water_flow'
   call update_sediment_flow
!   write (6, *) 'Returned from update_sediment_flow'
   call update_chemistry_flow
!   write (6, *) 'Returned from update_chemistry_flow'
   if (end_storm) then  !JW May 2017 delayed exit from above
      exit  !exit loop
   endif
!
!  END OF MAIN LOOP
!
enddo
close (51)

!EVA2016    calculation of annual net erosion (to be added after each storm event)
net_erosion_season (:, :) = net_erosion_season (:, :) + detach_tot (:, :) - depos_tot (:, :)

open (51, file = output_folder (1:output_folder_length) // 'SedChange.dat')
do i = 1, nr1
   do k = 1, nc1
      z_change (i, k) = depos_tot(i,k) - detach_tot(i,k)
   enddo
   write(51, 902) (z_change (i, k), k = 1, nc1)
enddo
close (51)

close (52)
!
!   reset fluxes for continuous simulation
!
!call initialize_values (ireset)
!
!   define starting conditions for next storm
!call interstorm
!
!end of continuous simulation loop
!
!
!
!   output
call output_maps_xml
!if (update_topography) then
!   call output_surface
!endif
if (hydro_out) then
   close (110)
   close (111)
   close (112)
endif

 900   format ('Time: ', e20.14, ' seconds')
 901   format ('End time:', f10.4, ' seconds')
 902   format (10000 (e17.10, 1x))
 903   format (' Output time ', i2, ':', i2, ':', i2, ' on ', i2, '/', i2, '/', i4)
9999   format (' Starting iteration ', i6, ' rain intensity: ', f7.2, ' time step: ', f5.2, &
               ' t: ', i4, ', Julian: ', i3, ', istart: ', i1, '  q_tot: ', e10.4, ', sed_tot: ', e10.4)
9998   format ('Storm event on day: ', i3, ', Year: ', i4)

end











!****************************************************************
!  subroutine to output final slope surface after simulation
!****************************************************************
!       subroutine output_surface

!    use shared_data


!       return
!       end

!****************************************************************
!  subroutine to define initial conditions for next storm in
!     continuous simulations
!****************************************************************
!       subroutine interstorm

!    use shared_data
!
!   drainage rates
!

!       return
!       end






