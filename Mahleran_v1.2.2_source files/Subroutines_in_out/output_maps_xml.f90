!****************************************************************
!
!  subroutine to output spatial data at end of run
!
!
!****************************************************************
subroutine output_maps_xml
!
!JWMay 2005 now uses .asc format which uses single input file with following header lines:
!
!       ncols [x]
!       nrows [y]
!       xllcorner [xmin]
!       yllcorner [ymin]
!       cellsize [dx]
!       nodata_value [nodata]
!
!  followed by row 1 etc. of the data
!JWMay 2005

use shared_data
use parameters_from_xml

implicit none

double precision :: uc, uc1   ! variables used to convert units for output

integer *4 :: ieighteen

integer :: i, k

logical :: fexist, exists

!
!    REB added aspect write to output flow direction: aspectfile
!

character *180 thetafile_img, nutrifile_img, paramfile_name, qfile_img,  aspectfile_img, dfile_img, &
               vfile_img, sedfile_img, detfile_img, depfile_img, soildepfile_img, soilvelfile_img, &
               neteros_img, raindet_img, flowdet_img, p_nitrate_img, p_ammonium_img, p_TN_img, &
               p_TP_img, p_IC_img, p_TC_img, ksat_img

data ieighteen / 18 /

thetafile_img = output_folder (1:output_folder_length) // 'theta001.asc'
nutrifile_img = output_folder (1:output_folder_length) // 'nutri001.asc'
qfile_img = output_folder (1:output_folder_length) // 'dschg001.asc'
aspectfile_img = output_folder (1:output_folder_length) // 'aspct001.asc'
dfile_img = output_folder (1:output_folder_length) // 'depth001.asc'
vfile_img = output_folder (1:output_folder_length) // 'veloc001.asc'
sedfile_img = output_folder (1:output_folder_length) // 'sedtr001.asc'
detfile_img = output_folder (1:output_folder_length) // 'detac001.asc'
depfile_img = output_folder (1:output_folder_length) // 'depos001.asc'
soildepfile_img = output_folder (1:output_folder_length) // 'soild001.asc'
soilvelfile_img = output_folder (1:output_folder_length) // 'soilv001.asc'
neteros_img = output_folder (1:output_folder_length) // 'neter001.asc'
raindet_img = output_folder (1:output_folder_length) //  'radet001.asc'
flowdet_img = output_folder (1:output_folder_length) // 'fldet001.asc'
p_nitrate_img = output_folder (1:output_folder_length) // 'pnitr001.asc'
p_ammonium_img = output_folder (1:output_folder_length) // 'pammo001.asc'
p_TN_img = output_folder (1:output_folder_length) // 'pTNxx001.asc'
p_TP_img = output_folder (1:output_folder_length) // 'pTPxx001.asc'
p_IC_img = output_folder (1:output_folder_length) // 'pICxx001.asc'
p_TC_img = output_folder (1:output_folder_length) // 'pTCxx001.asc'
ksat_img = output_folder (1:output_folder_length) // 'ksat_001.asc'

paramfile_name = output_folder (1:output_folder_length) // 'param001.dat'

!
!   open files for output based on value of iout calculated at start of run
!
!   REB  Again added aspectfiledoc and aspectfile_img here to output flow dirn
!
!   27/9/2 JW Added sedfile_img and sedfiledoc
if (iout.lt.10) then
   write (thetafile_img (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
   write (nutrifile_img (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
   write (qfile_img (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
   write (paramfile_name (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
   write (aspectfile_img (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
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
   write (p_nitrate_img (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
   write (p_ammonium_img (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
   write (p_TN_img (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
   write (p_TP_img (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
   write (p_IC_img (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
   write (p_TC_img (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
   write (ksat_img (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
elseif (iout.lt.100) then
   write (thetafile_img (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
   write (nutrifile_img (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
   write (qfile_img (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
   write (paramfile_name (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
   write (aspectfile_img (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
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
   write (p_ammonium_img (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
   write (p_TN_img (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
   write (p_TP_img (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
   write (p_IC_img (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
   write (p_TC_img (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
   write (ksat_img (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
elseif (iout.lt.1000) then
   write (thetafile_img (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
   write (nutrifile_img (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
   write (qfile_img (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
   write (paramfile_name (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
   write (aspectfile_img (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
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
   write (p_ammonium_img (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
   write (p_TN_img (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
   write (p_TP_img (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
   write (p_IC_img (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
   write (p_TC_img (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
   write (ksat_img (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
 endif
 if(f_thetafileimg) then
    open (11, file = thetafile_img, status = 'unknown')
    rewind (11)
 endif
if (f_nutrifileimg) then
   open (12, file = nutrifile_img, status = 'unknown')
   rewind (12)
endif
if (f_qfileimg) then
   open (13, file = qfile_img, status = 'unknown')
   rewind (13)
endif
if (f_aspectfileimg) then
   open (14, file = aspectfile_img, status = 'unknown')
   rewind (14)
endif
if (f_dfileimg) then
   open (15, file = dfile_img, status = 'unknown')
   rewind (15)
endif
if (f_vfileimg) then
   open (16, file = vfile_img, status = 'unknown')
   rewind (16)
endif
if (f_sedfileimg) then
   open (17, file = sedfile_img, status = 'unknown')
   rewind (17)
endif
if (f_paramfile) then
   open (18, file = paramfile_name, status = 'unknown')
   rewind (18)
endif
if (f_ksat) then
   open (19, file = ksat_img, status = 'unknown')
   rewind (19)
endif
if (f_pave) then
   open (20, file  = output_folder (1:output_folder_length) // 'pave.asc', status = 'unknown')
   rewind (20)
endif
if (f_rainmask) then
   open (21, file = output_folder (1:output_folder_length) // 'rainmask.asc', status = 'unknown')
   rewind (21)
endif
if (f_topog) then
   open (22, file = output_folder (1:output_folder_length) // 'topog.asc', status = 'unknown')
   rewind (22)
endif
if (f_veg) then
   open (23, file  = output_folder (1:output_folder_length) // 'veg.asc', status = 'unknown')
   rewind (23)
endif
if (f_detfileimg) then
   open (24, file = detfile_img, status = 'unknown')
   rewind (24)
endif
if (f_depfileimg) then
   open (25, file = depfile_img, status = 'unknown')
   rewind (25)
endif
if (f_soildepfileimg) then
   open (26, file = soildepfile_img, status = 'unknown')
   rewind (26)
endif
if (f_soilvelfileimg) then
   open (27, file = soilvelfile_img, status = 'unknown')
   rewind (27)
endif
if (f_neterosimg) then
   open (28, file = neteros_img, status = 'unknown')
   rewind (28)
endif
if (f_raindetimg) then
   open (29, file = raindet_img, status = 'unknown')
   rewind (29)
endif
if (f_flowdetimg) then
   open (30, file = flowdet_img, status = 'unknown')
   rewind (30)
endif
if (f_p_nitrateimg) then
   open (31, file = p_nitrate_img, status = 'unknown')
   rewind (31)
endif
if (f_p_ammoniumimg) then
   open (32, file = p_ammonium_img, status = 'unknown')
   rewind (32)
endif
if (f_p_TNimg) then
   open (33, file = p_TN_img, status = 'unknown')
   rewind (33)
endif
if (f_p_TPimg) then
   open (34, file = p_TP_img, status = 'unknown')
   rewind (34)
endif
if (f_p_ICimg) then
   open (35, file = p_IC_img, status = 'unknown')
   rewind (35)
endif
if (f_p_TCimg) then
   open (36, file = p_TC_img, status = 'unknown')
   rewind (36)
endif
!
!   write .asc files
!
!JWMay2005
!JWMay2005   Mask out values beyond the edges of the simulation
!JWMay2005
do i = 1, nr2
   do k = 1, nc2
      if (rmask (i, k).lt.0.0d0) then
         theta (i, k) = -9999
         y_amm (i, k) = -9999
         qsum (i, k) = -9999
         aspect (i, k) = -9999
         dmax (i, k) = -9999
         vmax (i, k) = -9999
         dmax_soil (i, k) = -9999
         vmax_soil (i, k) = -9999
         sed_tot (i, k) = -9999
     detach_tot (i, k) = -9999
     depos_tot (i, k) = -9999
     raindrop_detach_tot (i, k) = -9999
     flow_detach_tot (i, k) = -9999
!LTOct2007 added in sediment-bound nutrients
     amm_tot (i, k) = -9999
         nit_tot (i, k) = -9999
     TN_tot (i, k) = -9999
     TP_tot (i, k) = -9999
     IC_tot (i, k) = -9999
     TC_tot (i, k) = -9999
      endif
   enddo
enddo
!JWMay2005
!JWMay2005  write header lines
!JWMay2005
do i = 11, 30
   if (i.ne.18) then
      inquire (unit = i, opened = exists)
      if (exists) then
         write (i, 9999) nc2
         write (i, 9998) nr2
         write (i, 9997) xmin
         write (i, 9996) ymin
         write (i, 9995) dx / 1.d3   ! converts back to m from mm
         write (i, 9994)
      endif
   endif
enddo

!JWMay2005
!JWMay2005  write data lines
!JWMay2005
uc = dx * density * 1.e-6 * dt
uc1 = dx * dx * density * 1.e-6 * dt
do i = 1, nr2
   if (f_thetafileimg) then
      write (11, 9993) (theta (i, k), k = 1, nc2)
   endif
!Eva    output of total ammonium mass in mg
   if (f_nutrifileimg) then  !TODO should check for correct index, not discharge
      write (12, 9993) (y_amm(i,k), k = 1, nc2)
   endif
!cEva: output discharge in m^3/s (sum of discharge for whole rainfall event)
   if (f_qfileimg) then
      write (13, 9993) (qsum (i, k) * dx  * 1.e-9, k = 1, nc2)
   endif

   if(f_aspectfileimg) then
      write (14, 9992) (aspect (i, k), k= 1, nc2)
   endif
!
!j   Output dmax and vmax
!
   if (f_dfileimg) then
      write (15, 9993) (dmax (i, k), k = 1, nc2)
   endif
   if (f_vfileimg) then
      write (16, 9993) (vmax (i, k), k = 1, nc2)
   endif
!Eva:   calculation of sediment total mass in kg
!  uc converts to kg
   if (f_sedfileimg) then
      write (17, 9993) (sed_tot (i, k) * uc, k = 1, nc2)
   endif
!JW echo input parameters
   if (f_ksat) then
      write (19, 9993) (ksat (i, k), k = 1, nc2)
   endif
   if (f_pave) then
      write (20, 9993) (pave (i, k), k = 1, nc2)
   endif
   if (f_rainmask) then
      write (21, 9993) (rmask (i, k), k = 1, nc2)
   endif
   if (f_topog) then
      write (22, 9993) (z (i, k), k = 1, nc2)
   endif
   if (f_veg) then
      write (23, 9993) (veg (i, k), k = 1, nc2)
   endif
   if (f_detfileimg) then
      write (24, 9993) (detach_tot (i, k) * uc1, k = 1, nc2)
   endif
   if (f_depfileimg) then
      write (25, 9993) (depos_tot (i, k) * uc1, k = 1, nc2)
   endif
   if (f_soildepfileimg) then
      write (26, 9993) (dmax_soil (i, k), k = 1, nc2)
   endif
   if (f_soilvelfileimg) then
      write (27, 9993) (vmax_soil (i, k), k = 1, nc2)
   endif
   if (f_neterosimg) then
      write (28, 9993) ((detach_tot (i, k) - depos_tot (i, k)) * uc1, k = 1, nc2)
   endif
   if (f_raindetimg) then
      write (29, 9993) (raindrop_detach_tot (i, k) * uc1,  k = 1, nc2)
   endif
   if (f_flowdetimg) then
      write (30, 9993) (flow_detach_tot (i, k) * uc1, k = 1, nc2)
   endif
!LTOct2007 added in output total nutrient flux.
   if (f_p_nitrateimg) then
      write (31, 9993) (amm_tot (i,k), k = 1, nc2)     ! check units (grams)
   endif
   if (f_p_ammoniumimg) then
      write (32, 9993) (nit_tot(i,k), k = 1, nc2)     ! check units
   endif
   if (f_p_TNimg) then
      write (33, 9993) (TN_tot (i,k), k = 1, nc2)     ! check units
   endif
   if (f_p_TPimg) then
      write (34, 9993) (TP_tot (i,k), k = 1, nc2)     ! check units
   endif
   if (f_p_ICimg) then
      write (35, 9993) (IC_tot (i,k), k = 1, nc2)     ! check units
   endif
   if (f_p_TCimg) then
      write (36, 9993) (TC_tot (i,k), k = 1, nc2)     ! check units
   endif
enddo
!
!   Output parameter file
!
call echo_params_xml (ieighteen)

!
!  close file units for use next time
!
do i = 11, 36
   close (i)
enddo

9999   format ('ncols ', i6)
9998   format ('nrows ', i6)
9997   format ('xllcorner ', f15.8)
9996   format ('yllcorner ', f15.8)
9995   format ('cellsize ', f15.8)
9994   format ('nodata_value -9999')
9993   format (10000 (e10.4, 1x))
9992   format (10000 (i5, 1x))

return
end
