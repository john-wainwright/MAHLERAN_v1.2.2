!****************************************************************
!  subroutine to write time series after interstorm calculations
!****************************************************************
subroutine output_interstorm_fluxes

use interstorm_shared_data
use shared_data
use time_h
use parameters_from_xml

implicit none

real :: dummy (14)
real :: theta_av1, theta_av2

integer :: i, j, counter

character (len = 160) :: filename

counter = 0

filename = output_folder (1:output_folder_length) // 'storm_fluxes000.dat'
if (iout.lt.10) then
   write (filename (15 + output_folder_length: 15 + output_folder_length), '(i1)') iout
elseif (iout.lt.100) then
   write (filename (14 + output_folder_length: 15 + output_folder_length), '(i2)') iout
elseif (iout.lt.1000) then
   write (filename (13 + output_folder_length: 15 + output_folder_length), '(i3)') iout
endif


! output file for sum values of water, sediment etc. (daily values of fluxes at the end of every day)
if (rday.eq.0) then
   open (125, file = filename, status = 'unknown')
   rewind(125)
!original version below -- this version recoded to avoid site-specific hard-coding
   write (125, '(a)') 'rday Julian year Rain[mm] water[l] sediment[kg] ksat[mm/s] ff[-] fines[-] aver_moist_up  aver_moist_low '
elseif (rday.gt.0.and.rday.ne.total_days + 1) then
! total fluxes after an event
!qsum(:,:) in litres
!sed_tot(:,:)   !sediment (sum of all six classes) in kg
!y_amm(:,:) !dissolved ammonium in mg
!y_nit(:,:) !dissolved nitrate in mg
!y_phos(:,:)    !dissolved phosphorus in mg
!amm_tot(:,:)   !sediment-bound ammonium in grams
!nit_tot(:,:)   !sediment-bound nitrate in grams
!TN_tot(:,:)        !sediment-bound total nitrogen in grams
!TP_tot(:,:)        !sediment-bound total phosphorus in grams
! calculate sum values of all cells at the lower end of the plot
   dummy (:) = 0.
   do i = 2, nr
      do j = 2, nc
      !simplified version of output code from output_hydro_data_xml
         if ((aspect (i, j).eq.1.and.rmask (i, j).gt.0.0d0.and.rmask (i - 1, j).lt.0.0d0).or. &
             (aspect (i, j).eq.2.and.rmask (i, j).gt.0.0d0.and.rmask (i, j + 1).lt.0.0d0).or. &
             (aspect (i, j).eq.3.and.rmask (i, j).gt.0.0d0.and.rmask (i + 1, j).lt.0.0d0).or. &
             (aspect (i, j).eq.4.and.rmask (i, j).gt.0.0d0.and.rmask (i, j - 1).lt.0.0d0)) then
            ! only include those cells whose flow goes out of the plot (rather than to the side)
            dummy (1) = dummy (1) + qsum (i, j) * 1000. !for total water fluxes
            dummy (2) = dummy (2) + sed_tot (i, j)
            !dummy(3)=  dummy(3) + y_amm(61,j)/1000+y_nit(61,j)/1000+TN_tot(61,j)   !dissolved and sediment-bound total nitrogen
            !dummy(4)=   dummy(4) + y_phos(61,j)/1000+TP_tot(61,j)                      !dissolved and sediment-bound total phosphorus
            !dummy(5)=   dummy(5) + y_amm(61,j)/1000
            !dummy(6)=   dummy(6) + y_nit(61,j)/1000
            !dummy(7)=   dummy(7) + y_phos(61,j)/1000
            !dummy(8)=   dummy(8) + amm_tot(61,j)
            !dummy(9)=   dummy(9) + nit_tot(61,j)
            !dummy(10)=  dummy(10) + TN_tot(61,j)
            !dummy(11)=  dummy(11) + TP_tot(61,j)
            !dummy(12)=  dummy(12) + ksat(61,j)
!               counter=counter+1
         endif
      enddo
   enddo
   !dummy(12)=dummy(12)/counter
   do i = 2, nr2
      do j = 2, nc2
         grav (i,j) = 100.0d0 * (ps_init_ave (5,i,j) + ps_init_ave (6,i,j))
         fines (i,j) = 100.0d0 - grav (i,j)
      enddo
   enddo

   counter = 0
   do i = 2, nr
      do j = 2, nc
         if (rmask (i, j).ge.0.0d0) then
            dummy (12) = dummy (12) + ksat (i, j)
            dummy (13) = dummy (13) + ff (i, j)
            dummy (14) = dummy (14) + fines(i,j)
            counter = counter + 1
         endif
      enddo
   enddo

   do i = 0, 2
      dummy (12 + i) = dummy (12 + i) / real (counter)
   enddo

   !extra call added in here as the current day not yet updated (so value actually for start of storm)
   !otherwise just outputs zeroes in two final columns
   call theta_areal (rday - 1, theta_av1, theta_av2)

   write (125, 9999) rday, Julian, t, rain_daily (rday), &
                     (dummy (i), i = 1,2), (dummy (j), j = 12, 14), theta_av1, theta_av2

endif

!close file at end of simulation
if (rday.eq.total_days + 1) then
   close (125)
endif

!N.b. if this format statement changes, the one in Annual_statistics must be changed to match
9999   format (i6, 1x, i3, 1x, i4, 1x, f7.3, 7(1x, e10.4))

end


!original code below:
!   if(pfadin(1:(pfadi-2)).eq.'./Input/Sevilleta/input_p') then
!       write (125,'(11a12)') 'rday ', 'Julian ', 'year ', 'Rain(mm)','water(l) ', 'sediment(kg) ', &
!       'ksat(mm/s) ', 'ff(-) ', 'fines(-)) ',  'aver_moist_up ', 'aver_moist_low '
!   elseif(pfadin(1:pfadi).eq.'./Input/BL_Villacarli/') then
!       write (125,'(8a15)') 'rday ', 'Julian ', 'year ', 'Rain(mm)', 'water(m3) ', 'sed(kg) ', &
!       'aver_moist_up ', 'aver_moist_low '
!   endif
!
!
!elseif (rday.gt.0.and.rday.ne.total_days+1) then
!
!   output for plots in Sevilleta
!   if(pfadin(1:(pfadi-2)).eq.'./Input/Sevilleta/input_p') then
! total fluxes after an event
!qsum(:,:) in litres
!sed_tot(:,:)   !sediment (sum of all six classes) in kg
!y_amm(:,:) !dissolved ammonium in mg
!y_nit(:,:) !dissolved nitrate in mg
!y_phos(:,:)    !dissolved phosphorus in mg
!amm_tot(:,:)   !sediment-bound ammonium in grams
!nit_tot(:,:)   !sediment-bound nitrate in grams
!TN_tot(:,:)        !sediment-bound total nitrogen in grams
!TP_tot(:,:)        !sediment-bound total phosphorus in grams
!
! caculate sum values of all cells at the lower end of the plot
!       dummy(:)=0.
!       do j=2,21
!           if (aspect(61,j).eq.3) then ! only include those cells whose flow goes out of the plot (rather than to the side)
!               dummy(1)=   dummy(1) +qsum(61,j)*1000   !for total water fluxes
!               dummy(2)=   dummy(2) + sed_tot(61,j)
!       !       dummy(3)=   dummy(3) + y_amm(61,j)/1000+y_nit(61,j)/1000+TN_tot(61,j)   !dissolved and sediment-bound total nitrogen
!       !       dummy(4)=   dummy(4) + y_phos(61,j)/1000+TP_tot(61,j)                       !dissolved and sediment-bound total phosphorus
!       !       dummy(5)=   dummy(5) + y_amm(61,j)/1000
!       !       dummy(6)=   dummy(6) + y_nit(61,j)/1000
!       !       dummy(7)=   dummy(7) + y_phos(61,j)/1000
!       !       dummy(8)=   dummy(8) + amm_tot(61,j)
!       !       dummy(9)=   dummy(9) + nit_tot(61,j)
!       !       dummy(10)=  dummy(10) + TN_tot(61,j)
!       !       dummy(11)=  dummy(11) + TP_tot(61,j)
!               !dummy(12)=  dummy(12) + ksat(61,j)
!!              counter=counter+1
!           endif
!       enddo
!       !       dummy(12)=dummy(12)/counter
!       do i = 2, nr2
!         do j = 2, nc2
!           grav (i,j) = 100.0d0 * (ps_init_ave (5,i,j) + ps_init_ave (6,i,j))
!           fines (i,j) = 100.0d0 - grav (i,j)
!         enddo
!       enddo
!
!       counter=0
!       do i = 2, nr
!           do j = 2, nc
!               if (rmask (i, j).ge.0.0d0) then
!                   dummy(12)=dummy(12)+ksat(i,j)
!                   dummy(13)=dummy(13)+ff(i,j)
!                   dummy(14)=dummy(14)+fines(i,j)
!                   counter=counter+1
!               endif
!           enddo
!       enddo
!
!       do i=0,2
!           dummy(12+i)=dummy(12+i)/counter
!       enddo
!
!       write (125,'(3i6,8f14.3)') rday, Julian, t, rain_daily(rday), (dummy(i), i=1,2), (dummy(j), j=12,14), theta_areal_average(rday,1),theta_areal_average(rday,2)
!
!   output for Badland Villacarli
!   elseif(pfadin(1:pfadi).eq.'./Input/BL_Villacarli/') then
!       dummy(:)=0
!       dummy(1)=qsum(209,168)
!       dummy(2)=sed_tot(209,168)
!       write (125,'(3i6,5f14.3)') rday, Julian, t, rain_daily(rday), (dummy(i), i=1,2), theta_areal_average(rday,1),theta_areal_average(rday,2)
!
!   endif
