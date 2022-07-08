c****************************************************************
c  subroutine to define rainfall pattern for continous simulation (Mahleran interstorm)
c****************************************************************
       subroutine set_rain_new (ion_off)

       use shared_data
       use interstorm_shared_data
       use time_h
       implicit double precision (a - h, o - z)
       implicit integer (i - n)
       integer counter, dt_original
       double precision num_conv1

       character *11 start_time, atime
       character *10 aintensity,idummy
       character *2 tdummy,sdummy

       logical initial

       save start_time, time_last, time_next, initial, start_sec
c
c   VARIABLES:
c      ion_off = 1 to turn rainfall on
c      ion_off = 2 continue with Mahleran storm
c      ion_off = 3 switch off Mahleran after 20 minutes
c      ion_off = 4 leave Mahleran storm, as there is no heavy rain on that day
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



!     rain for continuous simulation (Mahleran Interstorm): rain_type=4
!
!     ion_off=1 find the beginning and end of rain in continuous rain array rain_minute_t
!     ion=1     read current and next time and current rain intensity
!     ion=2     let mahleran storm run for 1 min in dt time steps
!     ion=3     check if there is any rain in next minute, change time step if there are longer intervals without any rain
!     ion=4     let mahleran storm run with changed time steps without any rain
!     nstop and ion_off=3   end of rain data for that day is reached, 20 min model run after last intensity
       start_sec=0.
       if (ion_off.eq.1) then   !reset at end of storm per day
            ion=1
            counter=0
            dt_original=dt  !store original time step of mahleran storm, as set in mahleran_input.dat
        do k=1,file_length_rain
            if(rain_minute_t(1,k).eq.t.
     &          and.rain_minute_t(2,k).eq.Julian) exit
        enddo
        nbegin=k        !begin of current storm event
        if(nbegin.gt.file_length_rain) then
            write(*,*)'something is wrong with the rain storm events a'
            ion_off=4
              ion=5
        endif

        do k=nbegin,file_length_rain
            if(rain_minute_t(1,k).eq.t.
     &          and.rain_minute_t(2,k).ne.Julian) exit
        enddo
        nstop=k-1       !stop of current storm event
        if(nstop.gt.file_length_rain+1) then
            write(*,*)'something is wrong with the rain storm events b'
!           stop
        endif
!     Get beginning of rain storm directly from rain series in 1-min resolution, jump to rain_actual
        rain_actual=nbegin
        if (ion_off.ne.4)   ion_off=2


       endif

! check if end of rainstorm event on that day is reached:
       if(rain_actual.eq.nbegin.and.rain_actual.eq.nstop) then  !if only one entry in rainfile for that day
            rval = 0.
            do 32 i = 1, nr2
                do 42 j = 1, nc2
                    if (rmask(i,j).lt.-9900.) then
                        r2(i,j)=0
                    elseif(rmask(i,j).eq.1) then
                        r2 (i, j) = rval * rmask (i, j)
                    endif
   42           continue
   32       continue
          write (6, *) 'time ', atime, ' intensity ', rval
            ion_off=3   ! continue with Mahleran storm until it is set to ion_off=3
       endif
       if(rain_actual.eq.nstop) then
            time_last=time_next
            time_next=time_next+1200.
              ion=3
       endif
!

!
!  read in two times (next and last), and rainfall intensity for the current minute
!
       if (ion.eq.1) then
          write(tdummy,'(i2.2)')  rain_minute_t(3,rain_actual)
          write(sdummy,'(i2.2)')  rain_minute_t(4,rain_actual)
          atime=tdummy//':'//sdummy//':00.00'   !current hour:minute
        write(idummy,'(f10.6)') rain_minute(rain_actual)
        aintensity=idummy
        time_last = time_conv (atime)

        write(tdummy,'(i2.2)')  rain_minute_t(3,rain_actual+1)
          write(sdummy,'(i2.2)')  rain_minute_t(4,rain_actual+1)
          atime=tdummy//':'//sdummy//':00.00'       !next hour:minute
          time_next = time_conv (atime)

        rain_actual=rain_actual+1

          rval = num_conv1 (aintensity) / 3600.
          do 30 i = 1, nr2
              do 40 j = 1, nc2
                  if (rmask(i,j).lt.-9900.) then
                    r2(i,j)=0
                elseif(rmask(i,j).eq.1) then
                    r2 (i, j) = rval * rmask (i, j)
                endif
   40          continue
   30     continue
          write (6, *) 'Julian day: ', Julian, ' year: ',t,
     &  'next time ', atime,' intensity ', rval
          ion=2 !reset for next calculation steps in sub-minute intervalls
       endif
!
!     calculate current 1-min step in sub-minute calculation steps dt, as is specified in mahleran_input.dat
!
       if (ion.eq.2) then
        counter=counter+1
        if((counter*dt).gt.59) then
            counter=0
            ion=3
        endif

        rval = num_conv1 (aintensity) / 3600.   !use intensity from ion.eq.1
          do i = 1, nr2
               do j = 1, nc2
                  if (rmask(i,j).lt.-9000.) then
                    r2(i,j)=0
                elseif(rmask(i,j).eq.1) then
                    r2 (i, j) = rval * rmask (i, j)
                endif
c
c  calculate infiltration rate based on cell rainfall intensity in mm/h
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

c   0.6d0 is 0.0001667 * 3600 to convert back from mm/s to mm/h
c
                      if (inf_type.eq.3) then
                          ksat (i, j) = 0.00585 + 0.6d0 * r2 (i, j) -
     &                    pave (i, j)
                      endif

                  else
c
c   0.004166667 is 0.0001667 * 25 to use constant threshold value
c
                      if (inf_type.eq.3) then
                            ksat (i, j) = 0.00585 + 0.004166667 *
     &                                    rmask (i, j) - pave (i, j)
                      endif
                  endif
               enddo
          enddo
       endif

!     after 1 min was calculated, check if the next rain falls in the next minute, or if there is a larger time span without rain
       if(ion.eq.3) then
            if ((time_next - time_last).ne.60) then !!! EVA! check this equation
!     check if time span to next rainfall is less than 40 minutes   and calculate in 10 sect time steps
!               if((time_next - time_last).le.2400) then
!                   dt=10
      ! check if time span to next rainfall is more than 40 minutes and change time step to dt=1min
!               elseif((time_next - time_last).gt.2400) then
!                   dt=10
!               endif
                ion=4
            else
                ion=1       !read in next 1-min data, if rainfall occurs in the next minute
            endif
              write (6, *) 'Julian day: ', Julian, ' year: ',t,
     &      'next time ', atime, ' intensity ', rval, ' time step', dt

       endif
!
!calculate Mahleran storm with an increase time step (either 10 sec or 60 sec) and rainfall set to zero!
!
       if (ion.eq.4) then
            counter=counter+1
              if(counter*dt.lt.3600) then
                dt=dt_original
            else
                dt=60
            endif
            if((counter*dt).gt.(time_next - time_last)) then
                counter=0
                ion=1
                dt=dt_original      !reset time step to the original time step as defined in mahleran_input.dat
                if(rain_actual.eq.nstop) then
                    ion_off=3   !when end of rain on current day is reached, mahleran is switched off, after it has calculated an additional 20 minutes
                endif
            endif
            rval = 0.   !no rainfall in those time intervalls
              do i = 1, nr2
               do j = 1, nc2
                  if (rmask(i,j).lt.-9000.) then
                    r2(i,j)=0
                elseif(rmask(i,j).eq.1) then
                    r2 (i, j) = rval * rmask (i, j)
                endif
c
c  calculate infiltration rate based on cell rainfall intensity in mm/h
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

c   0.6d0 is 0.0001667 * 3600 to convert back from mm/s to mm/h
c
                      if (inf_type.eq.3) then
                          ksat (i, j) = 0.00585 + 0.6d0 * r2 (i, j) -
     &                    pave (i, j)
                      endif

                  else
c
c   0.004166667 is 0.0001667 * 25 to use constant threshold value
c
                      if (inf_type.eq.3) then
                            ksat (i, j) = 0.00585 + 0.004166667 *
     &                                    rmask (i, j) - pave (i, j)
                      endif
                  endif
                enddo
              enddo

       endif



       return
       end
