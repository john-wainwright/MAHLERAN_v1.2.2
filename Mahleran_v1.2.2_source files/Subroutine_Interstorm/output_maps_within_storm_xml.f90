!****************************************************************
!
!  subroutine to output spatial data within run
!
!  JW added Feb 2017 for COST comparison exercise
!
!
!****************************************************************
subroutine output_maps_within_storm_xml
!
! uses .asc format which uses single input file with following header lines:
!
!       ncols [x] 
!       nrows [y]
!       xllcorner [xmin]
!       yllcorner [ymin]
!       cellsize [dx]
!       nodata_value [nodata] 
!
!  followed by row 1 etc. of the data
!
    
use shared_data
use parameters_from_xml
       
implicit none

double precision :: uc, uc1   ! variables used to convert units for output

integer *4 :: ieighteen
integer *4 :: minute_of_storm

integer :: i, k

logical :: fexist, exists

character *180 thetafile_img, qfile_img, dfile_img, vfile_img, sedfile_img, detfile_img, &
      depfile_img, soildepfile_img, soilvelfile_img, neteros_img, raindet_img, flowdet_img

write (6, *) ' In output_maps_within_storm_xml, iout = ', iout      
!
!  templates of filenames
!
thetafile_img = output_folder (1:output_folder_length) // 'theta001_000min.asc'
qfile_img = output_folder (1:output_folder_length) // 'dschg001_000min.asc'
dfile_img = output_folder (1:output_folder_length) // 'depth001_000min.asc'
vfile_img = output_folder (1:output_folder_length) // 'veloc001_000min.asc'
sedfile_img = output_folder (1:output_folder_length) // 'sedtr001_000min.asc'
detfile_img = output_folder (1:output_folder_length) // 'detac001_000min.asc'
depfile_img = output_folder (1:output_folder_length) // 'depos001_000min.asc'
soildepfile_img = output_folder (1:output_folder_length) // 'soild001_000min.asc'
soilvelfile_img = output_folder (1:output_folder_length) // 'soilv001_000min.asc'
neteros_img = output_folder (1:output_folder_length) // 'neter001_000min.asc'
raindet_img = output_folder (1:output_folder_length) //  'radet001_000min.asc'
flowdet_img = output_folder (1:output_folder_length) // 'fldet001_000min.asc'
!
!   set up files for output based on value of iout calculated at start of run
!
if (iout.lt.10) then
   write (thetafile_img (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
   write (qfile_img (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
   write (dfile_img (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
   write (vfile_img (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
   write (sedfile_img (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
   write (detfile_img (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
   write (depfile_img (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
   write (soildepfile_img (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
   write (soilvelfile_img (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
   write (neteros_img (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
   write (raindet_img (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
   write (flowdet_img (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
elseif (iout.lt.100) then
   write (thetafile_img (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
   write (qfile_img (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
   write (dfile_img (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
   write (vfile_img (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
   write (sedfile_img (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
   write (detfile_img (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
   write (depfile_img (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
   write (soildepfile_img (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
   write (soilvelfile_img (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
   write (neteros_img (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
   write (raindet_img (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
   write (flowdet_img (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
elseif (iout.lt.1000) then
   write (thetafile_img (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
   write (qfile_img (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
   write (dfile_img (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
   write (vfile_img (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
   write (sedfile_img (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
   write (detfile_img (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
   write (depfile_img (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
   write (soildepfile_img (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
   write (soilvelfile_img (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
   write (neteros_img (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
   write (raindet_img (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
   write (flowdet_img (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
endif
!
!  insert minute of run into filenames
!
minute_of_storm = (iter * dt) / 60
write (6, *) ' Minute of storm: ', minute_of_storm
if (minute_of_storm.lt.10) then
   write (thetafile_img (12 + output_folder_length:12 + output_folder_length), '(i1)') minute_of_storm
   write (qfile_img (12 + output_folder_length:12 + output_folder_length), '(i1)') minute_of_storm
   write (dfile_img (12 + output_folder_length:12 + output_folder_length), '(i1)') minute_of_storm
   write (vfile_img (12 + output_folder_length:12 + output_folder_length), '(i1)') minute_of_storm
   write (sedfile_img (12 + output_folder_length:12 + output_folder_length), '(i1)') minute_of_storm
   write (detfile_img (12 + output_folder_length:12 + output_folder_length), '(i1)') minute_of_storm
   write (depfile_img (12 + output_folder_length:12 + output_folder_length), '(i1)') minute_of_storm
   write (soildepfile_img (12 + output_folder_length:12 + output_folder_length), '(i1)') minute_of_storm
   write (soilvelfile_img (12 + output_folder_length:12 + output_folder_length), '(i1)') minute_of_storm
   write (neteros_img (12 + output_folder_length:12 + output_folder_length), '(i1)') minute_of_storm
   write (raindet_img (12 + output_folder_length:12 + output_folder_length), '(i1)') minute_of_storm
   write (flowdet_img (12 + output_folder_length:12 + output_folder_length), '(i1)') minute_of_storm
elseif (minute_of_storm.lt.100) then
   write (thetafile_img (11 + output_folder_length:12 + output_folder_length), '(i2)') minute_of_storm
   write (qfile_img (11 + output_folder_length:12 + output_folder_length), '(i2)') minute_of_storm
   write (dfile_img (11 + output_folder_length:12 + output_folder_length), '(i2)') minute_of_storm
   write (vfile_img (11 + output_folder_length:12 + output_folder_length), '(i2)') minute_of_storm
   write (sedfile_img (11 + output_folder_length:12 + output_folder_length), '(i2)') minute_of_storm
   write (detfile_img (11 + output_folder_length:12 + output_folder_length), '(i2)') minute_of_storm
   write (depfile_img (11 + output_folder_length:12 + output_folder_length), '(i2)') minute_of_storm
   write (soildepfile_img (11 + output_folder_length:12 + output_folder_length), '(i2)') minute_of_storm
   write (soilvelfile_img (11 + output_folder_length:12 + output_folder_length), '(i2)') minute_of_storm
   write (neteros_img (11 + output_folder_length:12 + output_folder_length), '(i2)') minute_of_storm
   write (raindet_img (11 + output_folder_length:12 + output_folder_length), '(i2)') minute_of_storm
   write (flowdet_img (11 + output_folder_length:12 + output_folder_length), '(i2)') minute_of_storm
elseif (minute_of_storm.lt.1000) then
   write (thetafile_img (10 + output_folder_length:12 + output_folder_length), '(i3)') minute_of_storm
   write (qfile_img (10 + output_folder_length:12 + output_folder_length), '(i3)') minute_of_storm
   write (dfile_img (10 + output_folder_length:12 + output_folder_length), '(i3)') minute_of_storm
   write (vfile_img (10 + output_folder_length:12 + output_folder_length), '(i3)') minute_of_storm
   write (sedfile_img (10 + output_folder_length:12 + output_folder_length), '(i3)') minute_of_storm
   write (detfile_img (10 + output_folder_length:12 + output_folder_length), '(i3)') minute_of_storm
   write (depfile_img (10 + output_folder_length:12 + output_folder_length), '(i3)') minute_of_storm
   write (soildepfile_img (10 + output_folder_length:12 + output_folder_length), '(i3)') minute_of_storm
   write (soilvelfile_img (10 + output_folder_length:12 + output_folder_length), '(i3)') minute_of_storm
   write (neteros_img (10 + output_folder_length:12 + output_folder_length), '(i3)') minute_of_storm
   write (raindet_img (10 + output_folder_length:12 + output_folder_length), '(i3)') minute_of_storm
   write (flowdet_img (10 + output_folder_length:12 + output_folder_length), '(i3)') minute_of_storm
endif
!
! open files
! use of 3xx to avoid clashes with main outputs
!
open (311, file = thetafile_img, status = 'unknown')
rewind (311)
open (312, file = qfile_img, status = 'unknown')
rewind (312)
open (313, file = dfile_img, status = 'unknown')
rewind (313)
open (314, file = sedfile_img, status = 'unknown')
rewind (314)
open (315, file = neteros_img, status = 'unknown')
rewind (315)
!
!  commented to stop huge amounts of data being produced
!
!open (316, file = vfile_img, status = 'unknown')
!rewind (316)
!open (317, file = detfile_img, status = 'unknown')
!rewind (317)
!open (318, file = depfile_img, status = 'unknown')
!rewind (318)
!open (319, file = soildepfile_img, status = 'unknown')
!rewind (319)
!open (320, file = soilvelfile_img, status = 'unknown')
!rewind (320)
!open (321, file = raindet_img, status = 'unknown')
!rewind (321)
!open (322, file = flowdet_img, status = 'unknown')
!rewind (322)
!
! write header lines
! 
!do i = 311, 322
do i = 311, 315
   inquire (unit = i, opened = exists)
   if (exists) then
      write (i, 9999) nc2
      write (i, 9998) nr2
      write (i, 9997) xmin
      write (i, 9996) ymin
      write (i, 9995) dx / 1.d3   ! converts back to m from mm
      write (i, 9994)
   endif
enddo
!
! unit conversions
!
uc = dx * density * 1.e-6 * dt ! calculation of sediment total mass in kg,  uc converts to kg
uc1 = dx * dx * density * 1.e-6 * dt
!
! write data lines
!
do i = 1, nr2
   write (311, 9993) (theta (i, k), k = 1, nc2)
   write (312, 9993) (qsum (i, k) * dx  * 1.e-9, k = 1, nc2)
   write (313, 9993) (dmax (i, k), k = 1, nc2)
   write (314, 9993) (sed_tot (i, k) * uc, k = 1, nc2)
   write (315, 9993) ((detach_tot (i, k) - depos_tot (i, k)) * uc1, k = 1, nc2) 
!   write (316, 9993) (vmax (i, k), k = 1, nc2)
!   write (317, 9993) (detach_tot (i, k) * uc1, k = 1, nc2) 
!   write (319, 9993) (depos_tot (i, k) * uc1, k = 1, nc2) 
!   write (319, 9993) (dmax_soil (i, k), k = 1, nc2) 
!   write (320, 9993) (vmax_soil (i, k), k = 1, nc2) 
!   write (321, 9993) (raindrop_detach_tot (i, k) * uc1,  k = 1, nc2) 
!   write (322, 9993) (flow_detach_tot (i, k) * uc1, k = 1, nc2) 
enddo
!
!   output finished, close files
!
do i = 311, 315
!do i = 311, 322
   close (i)
enddo

9999   format ('ncols ', i6)
9998   format ('nrows ', i6)
9997   format ('xllcorner ', f15.8)
9996   format ('yllcorner ', f15.8)
9995   format ('cellsize ', f15.8)
9994   format ('nodata_value -9999') 
9993   format (10000 (e10.4, 1x))
9992   format (10000 (i1, 1x))

return
end

