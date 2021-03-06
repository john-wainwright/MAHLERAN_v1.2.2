c ************************************************************
c  subroutine to define rainfall pattern
c ************************************************************
       subroutine set_rain (ion_off)

       use shared_data
       implicit double precision (a - h, o - z)
       implicit integer (i - n)
       double precision num_conv1
       character *11 start_time, atime
       character *10 aintensity
       logical initial
       save start_time, time_last, time_next, initial, start_sec
c
c   VARIABLES:
c      ion_off = 1 to turn rainfall on
c              = any other value to end rainfall
c      rain_type = 1 for constant space-time rainfall
c                = 2 for temporally variable rainfall
c      rf_mean = average rainfall rate (mm/h)
c      rval = average rainfall rate (mm/s)
c      rfvar1 = standard deviation of rainfall rate (mm/h)
c      rfvar2 = skewness of rainfall rate (mm/h)
c      r2 (i, j) = rainfall rate at cell (i, j) (mm/s)
c      initial = .TRUE. if rain not previously set
c                .FALSE. otherwise
c
       data initial / .TRUE. /
       if (rain_type.eq.1) then
c
c         constant rainfall type
c
          if (ion_off.eq.1) then
             rval = rf_mean / 3600.
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
c
c                Calculate infiltration rate based on mean rainfall
c                intensity in mm/h.
c                Threshold of 25 mm/h used to avoid negative
c                infiltration values
c
                 if (pave (i, j).lt.0.0d0) then
c
c                    pavement value missing (off plot) -- use maximum value
c                    for no pavement
c
                     if (inf_type.eq.3) then
                         ksat (i, j) = 0.0142d0 + 0.6d0 * r2 (i, j)
                   endif
c
c                0.00694 mm/s = threshold of 25 mm/h
c
                 elseif (r2 (i, j).ge.0.00694d0) then
c
c                0.6d0 is 0.0001667 * 3600 to convert back from mm/s to mm/h
c
                     if (inf_type.eq.3) then
                         ksat (i, j) = 0.00585 + 0.6d0 * r2 (i, j) -
     &                              pave (i, j)
                   endif
               else
c
c   0.004166667 is 0.0001667 * 25 to use constant threshold value
c
                     if (inf_type.eq.3) then
                         ksat (i, j) = 0.00585 + 0.004166667 *
     &                              rmask (i, j) - pave (i, j)
                   endif
                 endif
             enddo
          enddo

c
c   variable rainfall read in from file
c
       elseif (rain_type.eq.2) then

          if (initial) then
             open (66, FILE=pfadin(1:pfadi)// rainfile, status = 'old')
             rewind (66)
             initial = .FALSE.
c
c  read in initial time, next time and initial intensity
c
             read (66, '(a11)') start_time
             start_sec = time_conv (start_time)   ! LT time_conv is a funtion that converts time to seconds
             read (66, '(a11, 1x, a10)', end = 10001, err = 10002)
     &                     atime, aintensity
cb  Should remember that the rainfall file is a formated
             go to 10003
10001        continue
             write (6, *) ' Warning - end of rainfall data file at',
     &                    ' time ', time, atime
             go to 10003
10002        continue
             write (6, *) ' Warning - error reading rainfall data',
     &                    ' file at time ', time, atime
10003        continue

             if (atime.lt.start_time) then
                start_sec = start_sec - 86400. ! LT for if its over midnight.
             endif
             time_last = 0.
             time_next = time_conv (atime) - start_sec
             rval = num_conv1 (aintensity) / 3600.
             do 3 i = 1, nr2
                do 4 j = 1, nc2
                    if (rmask(i,j).lt.-9900.) then
                      r2 (i, j) = 0.0d0
                    elseif (rmask (i, j).ge.0.0d0) then
c CJMHJul13         Bug fix - changed from elseif(rmask(i,j).eq.1)
                        r2 (i, j) = rval * rmask (i, j)
                    endif
   4            continue
   3         continue
             write (6, *) ' start sec ', start_sec,
     &             ' last time ', time_last, ' next time ',
     &             time_next, atime, aintensity, rval
          else
c
c JW fix 10/12/2013 fix to ensure integer values of iter don't propagate through
c
             time = dble (iter) * dt

             if (time.gt.time_next) then
c
c   read in next time and intensity
c
                read (66, '(a11, 1x, a10)', end = 10004, err = 10005)
     &                                     atime, aintensity
                go to 10006
10004           continue
                write (6, *) ' Warning - end of rainfall data file at',
     &                       ' time ', time, atime
                go to 10006
10005           continue
                write (6, *) ' Warning - error reading rainfall data',
     &                       ' file at time ', time, atime
10006           continue
                if (atime.lt.start_time) then
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
c CJMHJul13           Bug fix - changed from elseif(rmask(i,j).eq.1)
                          r2 (i, j) = rval * rmask (i, j)
                      endif
c
c                    calculate infiltration rate based on cell rainfall
c                    intensity in mm/h
c                    threshold of 25 mm/h used to avoid negative
c                    infiltration values
c
                      if (pave (i, j).lt.0.0d0) then
c
c                         pavement value missing (off plot) --
c                         use maximum value for no pavement
c
                          if (inf_type.eq.3) then
                              ksat (i, j) = 0.0142d0 + 0.6d0 * r2 (i, j)
                          endif
c
c                     0.00694 mm/s = threshold of 25 mm/h
                      elseif (r2 (i, j).ge.0.00694d0) then
c                         0.6d0 is 0.0001667 * 3600 to convert back from
c                         mm/s to mm/h
c
                          if (inf_type.eq.3) then
                              ksat (i, j) = 0.00585 + 0.6d0 * r2 (i, j)
     &                                    - pave (i, j)
                          endif
                      else
c
c                        0.004166667 is 0.0001667 * 25 to use constant
c                        threshold value
c
                         if (inf_type.eq.3) then
                            ksat (i, j) = 0.00585 + 0.004166667 *
     &                                    rmask (i, j) - pave (i, j)
                         endif
                      endif
                   enddo
                enddo
                write (6, *) ' start sec ', start_sec,
     &             ' last time ', time_last, ' next time ',
     &             time_next, atime, aintensity, rval
             endif
          endif
c
c   define other rainfall models here
c

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
c
c  calculate infiltration rate based on mean rainfall intensity in mm/h
c      threshold of 25 mm/h used to avoid negative infiltration values
c
                 if (pave (i, j).lt.0.0d0) then
c
c  pavement value missing (off plot) -- use maximum value for no pavement
c
                     if (inf_type.eq.3) then
                         ksat (i, j) = 0.0142d0 + 0.6d0 * r2 (i, j)
                   endif
c
c  0.00694 mm/s = threshold of 25 mm/h
c
                 elseif (r2 (i, j).ge.0.00694d0) then
c
c   0.6d0 is 0.0001667 * 3600 to convert back from mm/s to mm/h
c
                     if (inf_type.eq.3) then
                         ksat (i, j) = 0.00585 + 0.6d0 * r2 (i, j) -
     &                              pave (i, j)
                   endif
                 else
c
c   0.004166667 is 0.0001667 * 25 to use constant threshold value
c
                     if (inf_type.eq.3) then
                         ksat (i, j) = 0.00585 + 0.004166667 *
     &                              rmask (i, j) - pave (i, j)
                   endif
                 endif
             enddo
          enddo
       endif

       return
       end
