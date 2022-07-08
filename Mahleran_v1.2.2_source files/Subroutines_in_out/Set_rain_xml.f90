! ************************************************************
!  subroutine to define rainfall pattern
! ************************************************************
subroutine set_rain_xml (ion_off)

use shared_data
use parameters_from_xml

implicit none

double precision num_conv1
double precision time_last, time_next, start_sec
double precision time_conv
double precision rand_from_norm
double precision time

character *11 start_time, atime
character *10 aintensity

logical initial
logical end_of_rainfile

integer *4 idum
integer ion_off
integer i, j

save start_time, time_last, time_next, initial, start_sec, end_of_rainfile
!
!   VARIABLES:
!      ion_off = 1 to turn rainfall on
!              = any other value to end rainfall
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
if (rain_type.eq.1) then
!
!         constant rainfall type
!
   if (ion_off.eq.1) then
      rval = rf_mean / 3600.
   else
      rval = 0.
   endif
   do i = 1, nr2
      do j = 1, nc2
         if (rmask (i, j).lt.-9000.) then
            r2 (i, j) = 0.
         elseif (rmask (i, j).ge.0.0d0) then
            r2 (i, j) = rval * rmask (i, j)
         endif
!
!                Calculate infiltration rate based on mean rainfall
!                intensity in mm/h.
!                Threshold of 25 mm/h used to avoid negative
!                infiltration values
!
         if (pave (i, j).lt.0.0d0) then
!
!                    pavement value missing (off plot) -- use maximum value
!                    for no pavement
!
            if (inf_type.eq.3) then
               ksat (i, j) = 0.0142d0 + 0.6d0 * r2 (i, j)
        endif
!
!                0.00694 mm/s = threshold of 25 mm/h
!
         elseif (r2 (i, j).ge.0.00694d0) then
!
!                0.6d0 is 0.0001667 * 3600 to convert back from mm/s to mm/h
!
            if (inf_type.eq.3) then
               ksat (i, j) = 0.00585 + 0.6d0 * r2 (i, j) - pave (i, j)
        endif
     else
!
!   0.004166667 is 0.0001667 * 25 to use constant threshold value
!
            if (inf_type.eq.3) then
               ksat (i, j) = 0.00585 + 0.004166667 *  rmask (i, j) - pave (i, j)
        endif
         endif
      enddo
   enddo
!
!   variable rainfall read in from file
!
elseif (rain_type.eq.2) then
   if (initial) then
      open (66, file = input_folder (1:input_folder_length) //  rainfall_data, status = 'old')
      rewind (66)
      initial = .FALSE.
      end_of_rainfile = .FALSE.
!
!  read in initial time, next time and initial intensity
!
      read (66, '(a11)') start_time
      start_sec = time_conv (start_time)   ! LT time_conv is a funtion that converts time to seconds
      read (66, '(a11, 1x, a10)', end = 10001, err = 10002) atime, aintensity
      go to 10003
10001       continue
         write (6, *) ' Warning - end of rainfall data file at time ', time, atime
         end_of_rainfile = .TRUE.
         go to 10003
10002       continue
         write (6, *) ' Warning - error reading rainfall data file at time ', time, atime
10003       continue

      if (atime.lt.start_time.and..not.end_of_rainfile) then
         start_sec = start_sec - 86400. ! LT for if its over midnight.
      endif
      time_last = 0.
      time_next = time_conv (atime) - start_sec
      rval = num_conv1 (aintensity) / 3600.
      do i = 1, nr2
         do j = 1, nc2
            if (rmask(i,j).lt.-9900.) then
               r2 (i, j) = 0.0d0
            elseif (rmask (i, j).ge.0.0d0) then
! CJMHJul13         Bug fix - changed from elseif(rmask(i,j).eq.1)
               r2 (i, j) = rval * rmask (i, j)
            endif
         enddo
      enddo
      write (6, *) ' start sec ', start_sec, ' last time ', time_last, ' next time ', time_next, atime, aintensity, rval
   else
!
! JW fix 10/12/2013 fix to ensure integer values of iter don't propagate through
!
      time = dble (iter) * dt

      if (time.gt.time_next) then
!
!   read in next time and intensity
!
         read (66, '(a11, 1x, a10)', end = 10004, err = 10005) atime, aintensity
         go to 10006
10004       continue
            write (6, *) ' Warning - end of rainfall data file at time ', time, atime
            end_of_rainfile = .TRUE.
            aintensity = '0.0'
            go to 10006
10005    continue
         write (6, *) ' Warning - error reading rainfall data file at time ', time, atime
         aintensity = '0.0'
10006    continue
         if (atime.lt.start_time.and..not.end_of_rainfile) then
            start_sec = start_sec - 86400.
         endif
         time_last = time_next
         time_next = time_conv (atime) - start_sec
         rval = num_conv1 (aintensity) / 3600.
         do i = 1, nr2
            do j = 1, nc2
               if (rmask(i,j).lt.-9000.) then
                  r2 (i, j) = 0.0d0
               elseif (rmask (i, j).ge.0.0d0) then
! CJMHJul13           Bug fix - changed from elseif(rmask(i,j).eq.1)
                  r2 (i, j) = rval * rmask (i, j)
               endif
!
!                    calculate infiltration rate based on cell rainfall
!                    intensity in mm/h
!                    threshold of 25 mm/h used to avoid negative
!                    infiltration values
!
               if (pave (i, j).lt.0.0d0) then
!
!                         pavement value missing (off plot) --
!                         use maximum value for no pavement
!
                  if (inf_type.eq.3) then
                     ksat (i, j) = 0.0142d0 + 0.6d0 * r2 (i, j)
                  endif
!
!                     0.00694 mm/s = threshold of 25 mm/h
               elseif (r2 (i, j).ge.0.00694d0) then
!                         0.6d0 is 0.0001667 * 3600 to convert back from
!                         mm/s to mm/h
!
                  if (inf_type.eq.3) then
                     ksat (i, j) = 0.00585 + 0.6d0 * r2 (i, j) - pave (i, j)
                  endif
               else
!
!                        0.004166667 is 0.0001667 * 25 to use constant
!                        threshold value
!
                  if (inf_type.eq.3) then
                 ksat (i, j) = 0.00585 + 0.004166667 * rmask (i, j) - pave (i, j)
                  endif
               endif
            enddo
         enddo
         write (6, *) ' start sec ', start_sec, ' last time ', time_last, ' next time ', time_next, atime, aintensity, rval
      endif
   endif
!
!   define other rainfall models here
!

elseif (rain_type.eq.3) then
   if (ion_off.eq.1) then
      rval = rand_from_norm (rf_mean, rfvar1, idum) / 3600.
   else
      rval = 0.
   endif
   do i = 1, nr2
      do j = 1, nc2
         if (rmask (i, j).lt.-9000.) then
            r2 (i, j)=0
         elseif (rmask (i, j).ge.0.0d0) then
            r2 (i, j) = rval * rmask (i, j)
         endif
!
!  calculate infiltration rate based on mean rainfall intensity in mm/h
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

return
end
