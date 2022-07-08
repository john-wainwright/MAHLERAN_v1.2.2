!****************************************************************
!  subroutine to define rainfall pattern for continous simulation (Mahleran interstorm)
!****************************************************************
subroutine set_rain_new_xml (ion_off)

use shared_data
use interstorm_shared_data
use time_h
use parameters_from_xml

implicit none

!implicit double precision (a - h, o - z)
!implicit integer (i - n)

double precision :: num_conv1, time_conv
double precision :: time_last, time_next, start_sec
double precision :: dt_original

integer :: ion_off
integer :: counter
integer :: nbegin, nstop, i, j, k, ion

character (len = 11) :: start_time, atime, atime1
character (len = 10) :: aintensity, idummy
character (len = 2) :: tdummy, sdummy

logical initial

save start_time, time_last, time_next, initial, start_sec, ion, aintensity, counter, atime, atime1, dt_original, nbegin, nstop
!
!   VARIABLES:
!      ion_off = 1 to turn rainfall on
!      ion_off = 2 continue with Mahleran storm
!      ion_off = 3 switch off Mahleran after 20 minutes
!      ion_off = 4 leave Mahleran storm, as there is no heavy rain on that day
!      rain_type = 1 for constant space-time rainfall
!                = 2 for temporally variable rainfall
!      rf_mean = average rainfall rate (mm/h)
!      rval = average rainfall rate (mm/s)
!      rfvar1 = standard deviation of rainfall rate (mm/h)
!      rfvar2 = skewness of rainfall rate (mm/h)
!      r2 (i, j) = rainfall rate at cell (i, j) (mm/s)
!      initial = .TRUE. if rain not previously set
!                .FALSE. otherwise
!
data initial / .TRUE. /

!   rain for continuous simulation (Mahleran Interstorm): rain_type=4
!
!   ion_off=1 find the beginning and end of rain in continuous rain array rain_minute_t
!   ion=1       read current and next time and current rain intensity
!   ion=2       let mahleran storm run for 1 min in dt time steps
!   ion=3       check if there is any rain in next minute, change time step if there are longer intervals without any rain
!   ion=4       let mahleran storm run with changed time steps without any rain
!   nstop and ion_off=3 end of rain data for that day is reached, 20 min model run after last intensity

start_sec = 0.
!write (6, *) ' Entering set_rain_new_xml, ion_off=', ion_off, ' ion=', ion
if (ion_off.eq.1) then  !reset at end of storm per day
   ion = 1
   counter = 0
   dt_original = dt      !store original time step of mahleran storm, as set in mahleran_input.dat
   do k = 1, file_length_rain
      if (rain_minute_t (1, k).eq.t.and.rain_minute_t (2, k).eq.Julian) then
         exit
      endif
   enddo
   nbegin = k            !beginning of current storm event
   if (nbegin.gt.file_length_rain) then
      write (6, *)'WARNING: in set_rain_new_xml, file length exceeded while looking for next storm event'
      write (6, *) 'rain_minute_t (1, k): ', rain_minute_t (1, k), ', rain_minute_t (2, k): ', rain_minute_t (2, k), &
                   ', t: ', t, ', Julian: ', Julian
      ion_off = 4
      ion = 5
   endif
   k = nbegin + 1
   do while (rain_minute_t (1, k).eq.t.and.rain_minute_t (2, k).eq.Julian)
      k = k + 1
      if (k.ge.file_length_rain + 1) then
         exit
      endif
   enddo

   nstop = k - 1          !end of current storm event
!   Get beginning of rain storm directly from rain series in 1-min resolution, jump to rain_actual
   rain_actual = nbegin
   if (ion_off.ne.4) then
      ion_off = 2
   endif
endif

! check if end of rainstorm event on that day is reached:
if (rain_actual.eq.nbegin.and.rain_actual.eq.nstop) then      !if only one entry in rainfile for that day
   rval = 0.
   do i = 1, nr2
      do j = 1, nc2
         if (rmask (i, j).le.nodata_value_from_topog) then    !JW changed relative to no data value in topography file for consistency
            r2 (i, j) = 0.
         elseif (rmask (i, j).ge.0.0) then     !JW changed from .eq.1 for consistency with elsewhere
            r2 (i, j) = rval * rmask (i, j)
         endif
      enddo
   enddo
!   write (6, *) 'time ', atime, ' intensity ', rval
   ion_off = 3      ! continue with Mahleran storm until it is set to ion_off=3
endif
if (rain_actual.eq.nstop) then
   time_last = time_next
   time_next = time_next + 1200.
   ion = 3
endif
!
!  read in two times (next and last), and rainfall intensity for the current minute
!
if (ion.eq.1) then
   write (tdummy, '(i2.2)')  rain_minute_t (3, rain_actual)
   write (sdummy, '(i2.2)')  rain_minute_t (4, rain_actual)
   atime1 = tdummy // ':' // sdummy // ':00.00'      !current hour:minute
   write (idummy, '(f10.6)') rain_minute (rain_actual)
   aintensity = idummy
   time_last = time_conv (atime1)
   write (tdummy, '(i2.2)') rain_minute_t (3, rain_actual + 1)
   write (sdummy, '(i2.2)') rain_minute_t (4, rain_actual + 1)
   atime = tdummy // ':' // sdummy // ':00.00'       !next hour:minute
   time_next = time_conv (atime)
   rain_actual = rain_actual + 1
   rval = num_conv1 (aintensity) / 3600.
   do i = 1, nr2
      do j = 1, nc2
         if (rmask (i, j).le.nodata_value_from_topog) then    !JW changed relative to no data value in topography file for consistency
            r2 (i, j) = 0.
         elseif (rmask (i, j).ge.0.0) then     !JW changed from .eq.1 for consistency with elsewhere
            r2 (i, j) = rval * rmask (i, j)
         endif
      enddo
   enddo
!   write (6, 9999) Julian, t, atime1, atime, rval * 3600.
   ion = 2      !reset for next calculation steps in sub-minute intervals
endif
!
!   calculate current 1-min step in sub-minute calculation steps dt, as is specified in mahleran_input.dat
!
if (ion.eq.2) then
   counter = counter + 1
   if ((counter * dt).gt.59.0) then
      counter = 0
      ion = 3
   endif

   rval = num_conv1 (aintensity) / 3600.    !use intensity from ion.eq.1
   do i = 1, nr2
      do j = 1, nc2
         if (rmask(i,j).le.nodata_value_from_topog) then    !JW changed relative to no data value in topography file for consistency
            r2 (i, j) = 0.
         elseif (rmask (i, j).ge.0.0) then     !JW changed from .eq.1 for consistency with elsewhere
            r2 (i, j) = rval * rmask (i, j)
         endif
!
!  calculate infiltration rate based on cell rainfall intensity in mm/h
!      threshold of 25 mm/h used to avoid negative infiltration values
!
         if (pave (i, j).lt.0.0d0) then
!
!  pavement value missing (off plot) -- use maximum value for no pavement
!
            if (inf_type.eq.3) then
               ksat (i, j) = 0.0142d0 + 0.6d0 * r2 (i, j)
            endif
!
!  0.00694 mm/s = threshold of 25 mm/h
!
         elseif (r2 (i, j).ge.0.00694d0) then
!
!   0.6d0 is 0.0001667 * 3600 to convert back from mm/s to mm/h
!
            if (inf_type.eq.3) then
               ksat (i, j) = 0.00585 + 0.6d0 * r2 (i, j) - pave (i, j)
            endif
         else
!
!   0.004166667 is 0.0001667 * 25 to use constant threshold value
!
            if (inf_type.eq.3) then
               ksat (i, j) = 0.00585 + 0.004166667 * rmask (i, j) - pave (i, j)
            endif
         endif
      enddo
   enddo
endif
!
!   after 1 min was calculated, check if the next rain falls in the next minute, or if there is a larger time span without rain
!
if (ion.eq.3) then
   if ((time_next - time_last).ne.60) then
      ion = 4       !see below
   else
      ion = 1       !read in next 1-min data, if rainfall occurs in the next minute
   endif
!   write (6, *) 'Julian day: ', Julian, ' year: ', t, ', next time ', atime, ' intensity ', rval, ' time step: ', dt
!   write (6, *) ' End of minute, ion=', ion
endif
!
!calculate Mahleran storm with an increase time step (either 10 sec or 60 sec) and rainfall set to zero!
!
if (ion.eq.4) then
   if (counter.lt.3000) then    !continue calculating in 1sec time steps for 1800 sec (0.5 hour)
      dt = dt_original
      counter = counter + 1
   else
      dt = 5.       ! dt=5
      counter = counter + dt
   endif
   if (counter.gt.(time_next - time_last - 60)) then
      counter = 0
      ion = 1
      dt = dt_original      !reset time step to the original time step as defined in mahleran_input.dat
      if (rain_actual.eq.nstop) then
         ion_off = 3    !when end of rain on current day is reached, mahleran is switched off, after it has calculated an additional 40 minutes
      endif
   endif
   rval = 0.    !no rainfall in those time intervals
!   write (6, 9998) Julian, t, atime1, atime, rval * 3600., dt
   do i = 1, nr2
      do j = 1, nc2
!   no rainfall occurs
         r2 (i, j) = 0.
!
!  calculate infiltration rate based on cell rainfall intensity in mm/h
!      threshold of 25 mm/h used to avoid negative infiltration values
!
         if (pave (i, j).lt.0.0d0) then
!
!  pavement value missing (off plot) -- use maximum value for no pavement
!
            if (inf_type.eq.3) then
               ksat (i, j) = 0.0142d0 + 0.6d0 * r2 (i, j)
            endif
!
!  0.00694 mm/s = threshold of 25 mm/h
!
         elseif (r2 (i, j).ge.0.00694d0) then

!   0.6d0 is 0.0001667 * 3600 to convert back from mm/s to mm/h
!
            if (inf_type.eq.3) then
               ksat (i, j) = 0.00585 + 0.6d0 * r2 (i, j) - pave (i, j)
            endif
         else
!
!   0.004166667 is 0.0001667 * 25 to use constant threshold value
!
            if (inf_type.eq.3) then
               ksat (i, j) = 0.00585 + 0.004166667 * rmask (i, j) - pave (i, j)
            endif
         endif
      enddo
   enddo
endif

9999   format ('Julian day: ', i3, ' year: ', i4, ', last time ', a11, ', next time ', a11,' intensity ', f7.3, ' mm/h')
9998   format ('Julian day: ', i3, ' year: ', i4, ', last time ', a11, ', next time ', a11,' intensity ', f7.3, &
               ' mm/h,  time step: ', f7.3, ' s')
return
end
