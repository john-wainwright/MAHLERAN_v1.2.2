!****************************************************************
!  subroutine to output soil moisture and drainage at end of run
!
!  REB need to add in writes for flow direction here...
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

       implicit double precision (a - h, o - z)
       implicit integer (i - n)

       integer *4 ieighteen

       logical fexist, exists

c
c    REB added aspect write to output flow direction: aspectfile
c

       character *50 thetafileimg, nutrifileimg,
     &               paramfile,
     &               qfileimg,
     &               aspectfileimg,
     &               dfileimg,
     &               vfileimg,
     &               sedfileimg, detfileimg, depfileimg,
     &               soildepfileimg, soilvelfileimg,
     &               neterosimg, raindetimg, flowdetimg,
     &               p_nitrateimg, p_ammoniumimg, p_TNimg,
     &               p_TPimg, p_ICimg, p_TCimg,
     &               ksatimg
       data ieighteen / 18 /
      thetafileimg = output_folder (1:output_folder_length) // 'theta001.asc'
      nutrifileimg = output_folder (1:output_folder_length) // 'nutri001.asc'
      qfileimg = output_folder (1:output_folder_length) // 'dschg001.asc'
      aspectfileimg = output_folder (1:output_folder_length) // 'aspct001.asc'
      dfileimg = output_folder (1:output_folder_length) // 'depth001.asc'
      vfileimg = output_folder (1:output_folder_length) // 'veloc001.asc'
      sedfileimg = output_folder (1:output_folder_length) // 'sedtr001.asc'
      detfileimg = output_folder (1:output_folder_length) // 'detac001.asc'
      depfileimg = output_folder (1:output_folder_length) // 'depos001.asc'
      soildepfileimg = output_folder (1:output_folder_length) // 'soild001.asc'
      soilvelfileimg = output_folder (1:output_folder_length) // 'soilv001.asc'
      neterosimg = output_folder (1:output_folder_length) // 'neter001.asc'
      raindetimg = output_folder (1:output_folder_length) //  'radet001.asc'
      flowdetimg = output_folder (1:output_folder_length) // 'fldet001.asc'
      p_nitrateimg = output_folder (1:output_folder_length) // 'pnitr001.asc'
      p_ammoniumimg = output_folder (1:output_folder_length) // 'pammo001.asc'
      p_TNimg = output_folder (1:output_folder_length) // 'pTNxx001.asc'
      p_TPimg = output_folder (1:output_folder_length) // 'pTPxx001.asc'
      p_ICimg = output_folder (1:output_folder_length) // 'pICxx001.asc'
      p_TCimg = output_folder (1:output_folder_length) // 'pTCxx001.asc'
      ksatimg = output_folder (1:output_folder_length) // 'ksat_001.asc'

      paramfile = output_folder (1:output_folder_length) // 'param001.dat'

c
c   open files for output based on value of iout calculated at start of run
c
c   REB  Again added aspectfiledoc and aspectfileimg here to output flow dirn
c
c   27/9/2 JW Added sedfileimg and sedfiledoc


       if (iout.lt.10) then
          write (thetafileimg (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
          write (nutrifileimg (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
          write (qfileimg (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
          write (paramfile (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
          write (aspectfileimg (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
          write (dfileimg (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
          write (vfileimg (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
          write (sedfileimg (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
          write (detfileimg (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
          write (depfileimg (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
          write (soildepfileimg (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
          write (soilvelfileimg (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
          write (neterosimg (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
          write (raindetimg (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
          write (flowdetimg (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
          write (p_nitrateimg (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
          write (p_ammoniumimg (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
          write (p_TNimg (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
          write (p_TPimg (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
          write (p_ICimg (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
        write (p_TCimg (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
        write (ksatimg (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
       elseif (iout.lt.100) then
          write (thetafileimg (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
          write (nutrifileimg (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
          write (qfileimg (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
          write (paramfile (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
          write (aspectfileimg (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
          write (dfileimg (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
          write (vfileimg (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
          write (sedfileimg (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
          write (detfileimg (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
          write (depfileimg (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
          write (soildepfileimg (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
          write (soilvelfileimg (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
          write (neterosimg (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
          write (raindetimg (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
          write (flowdetimg (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
          write (p_ammoniumimg (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
          write (p_TNimg (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
          write (p_TPimg (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
          write (p_ICimg (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
        write (p_TCimg (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
        write (ksatimg (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
       elseif (iout.lt.1000) then
          write (thetafileimg (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
          write (nutrifileimg (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
          write (qfileimg (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
          write (paramfile (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
          write (aspectfileimg (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
          write (dfileimg (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
          write (vfileimg (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
          write (sedfileimg (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
          write (detfileimg (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
          write (depfileimg (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
          write (soildepfileimg (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
          write (soilvelfileimg (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
          write (neterosimg (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
          write (raindetimg (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
          write (flowdetimg (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
          write (p_ammoniumimg (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
          write (p_TNimg (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
          write (p_TPimg (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
          write (p_ICimg (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
        write (p_TCimg (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
        write (ksatimg (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
       endif
       if(f_thetafileimg) open (11, file = thetafileimg,
     &                    status = 'unknown')
       if(f_thetafileimg) rewind (11)
       if(f_nutrifileimg) open (12, file = nutrifileimg,
     &                    status = 'unknown')
       if(f_nutrifileimg) rewind (12)
       if(f_qfileimg) open (13, file = qfileimg, status = 'unknown')
       if(f_qfileimg) rewind (13)
       if(f_aspectfileimg) open (14, file = aspectfileimg,
     &                    status = 'unknown')
       if(f_aspectfileimg) rewind (14)
       if(f_dfileimg) open (15, file = dfileimg, status = 'unknown')
       if(f_dfileimg) rewind (15)
       if(f_vfileimg) open (16, file = vfileimg, status = 'unknown')
       if(f_vfileimg) rewind (16)
       if(f_sedfileimg) open (17, file = sedfileimg, status = 'unknown')
       if(f_sedfileimg) rewind (17)
       if(f_paramfile) open (18, file = paramfile, status = 'unknown')
       if(f_paramfile) rewind (18)
c       if(f_ksat) open (19, file  = output_folder (1:output_folder_length) // 'ksat.asc',
c     &         status = 'unknown')
       if(f_ksat) open (19, file = ksatimg, status = 'unknown')
       if(f_ksat) rewind (19)
       if(f_pave) open (20, file  = output_folder (1:output_folder_length) // 'pave.asc',
     &          status = 'unknown')
       if(f_pave) rewind (20)
       if(f_rainmask) open (21, file = output_folder (1:output_folder_length) // 'rainmask.asc',
     &              status = 'unknown')
       if(f_rainmask) rewind (21)
       if(f_topog) open (22, file = output_folder (1:output_folder_length) // 'topog.asc',
     &           status = 'unknown')
       if(f_topog) rewind (22)
       if(f_veg) open (23, file  = output_folder (1:output_folder_length) // 'veg.asc',
     &         status = 'unknown')
       if(f_veg) rewind (23)
       if(f_detfileimg) open (24, file = detfileimg, status = 'unknown')
       if(f_detfileimg)rewind (24)
       if(f_depfileimg) open (25, file = depfileimg, status = 'unknown')
       if(f_depfileimg) rewind (25)
       if(f_soildepfileimg) open (26, file = soildepfileimg,
     &                      status = 'unknown')
       if(f_soildepfileimg) rewind (26)
       if(f_soilvelfileimg) open (27, file = soilvelfileimg,
     &                      status = 'unknown')
       if(f_soilvelfileimg)rewind (27)
       if(f_neterosimg) open (28, file = neterosimg, status = 'unknown')
       if(f_neterosimg) rewind (28)
       if(f_raindetimg) open (29, file = raindetimg, status = 'unknown')
       if(f_raindetimg) rewind (29)
       if(f_flowdetimg) open (30, file = flowdetimg, status = 'unknown')
       if(f_flowdetimg) rewind (30)
       if(f_p_nitrateimg) open (31, file = p_nitrateimg,
     &                    status = 'unknown')
       if(f_p_nitrateimg) rewind (31)
       if(f_p_ammoniumimg) open (32, file = p_ammoniumimg,
     &                     status = 'unknown')
       if(f_p_ammoniumimg)rewind (32)
       if(f_p_TNimg) open (33, file = p_TNimg, status = 'unknown')
       if(f_p_TNimg)rewind (33)
       if(f_p_TPimg) open (34, file = p_TPimg, status = 'unknown')
       if(f_p_TPimg) rewind (34)
       if(f_p_ICimg) open (35, file = p_ICimg, status = 'unknown')
       if(f_p_ICimg) rewind (35)
       if(f_p_TCimg) open (36, file = p_TCimg, status = 'unknown')
       if(f_p_TCimg) rewind (36)

c
c   write .asc files
c
cJWMay2005
cJWMay2005   Mask out values beyond the edges of the simulation
cJWMay2005
       do i = 1, nr1
          do k = 1, nc1
             if (rmask (i, k).lt.0.0d0) then
                theta (i, k) = 0.0d0
                y_amm (i, k) = 0.0d0
                qsum (i, k) = 0.0d0
                aspect (i, k) = 0.0d0
                dmax (i, k) = 0.0d0
                vmax (i, k) = 0.0d0
                dmax_soil (i, k) = 0.0d0
                vmax_soil (i, k) = 0.0d0
                sed_tot (i, k) = 0.0d0
                detach_tot (i, k) = 0.0d0
                depos_tot (i, k) = 0.0d0
                raindrop_detach_tot (i, k) = 0.0d0
              flow_detach_tot (i, k) = 0.0d0
cLTOct2007 added in sediment-bound nutrients
                amm_tot (i, k) = 0.0d0
                nit_tot (i, k) = 0.0d0
                TN_tot (i, k) = 0.0d0
                TP_tot (i, k) = 0.0d0
                IC_tot (i, k) = 0.0d0
                TC_tot (i, k) = 0.0d0
             endif
          enddo
       enddo
cJWMay2005
cJWMay2005  write header lines
cJWMay2005
      do i = 11, 30
        inquire (UNIT=i,OPENED=exists)
          if (exists) then
             write (i, 9999) nc1
             write (i, 9998) nr1
             write (i, 9997) xmin
             write (i, 9996) ymin
             write (i, 9995) dx / 1.d3   ! converts back to m from mm
             write (i, 9994)
          endif
       enddo

cJWMay2005
cJWMay2005  write data lines
cJWMay2005
       uc = dx * density * 1.e-6 * dt
       uc1 = dx * dx * density * 1.e-6 * dt
       do i = 1, nr1
         if(f_thetafileimg) write (11, 9993) (theta (i, k), k = 1, nc1)
cEva  output of total ammonium mass in mg
         if(f_qfileimg) write (12, 9993) (y_amm(i,k), k = 1, nc1)
cEva: output discharge in m^3/s (sum of discharge for whole rainfall event)
         if(f_qfileimg) write (13, 9993) (qsum (i, k) * dx  * 1.e-9,
     &                k = 1, nc1)
         if(f_aspectfileimg) write (14, 9992) (aspect (i, k), k= 1, nc1)
c
cj   Output dmax and vmax
c
         if(f_dfileimg) write (15, 9993) (dmax (i, k), k = 1, nc1)
         if(f_vfileimg) write (16, 9993) (vmax (i, k), k = 1, nc1)
cEva: calculation of sediment total mass in kg
c  uc converts to kg
         if(f_sedfileimg) write (17, 9993) (sed_tot (i, k) * uc,
     &                  k = 1, nc1)
cJW echo input parameters
         if(f_ksat) write (19, 9993) (ksat (i, k), k = 1, nc1)
         if(f_pave) write (20, 9993) (pave (i, k), k = 1, nc1)
         if(f_rainmask) write (21, 9993) (rmask (i, k), k = 1, nc1)
         if(f_topog) write (22, 9993) (z (i, k), k = 1, nc1)
         if(f_veg) write (23, 9993) (veg (i, k), k = 1, nc1)
         if(f_detfileimg) write (24, 9993) (detach_tot (i, k) * uc1,
     &                  k = 1, nc1)
         if(f_depfileimg) write (25, 9993) (depos_tot (i, k) * uc1,
     &              k = 1, nc1)
         if(f_soildepfileimg) write (26, 9993) (dmax_soil (i, k),
     &                      k = 1, nc1)
         if(f_soilvelfileimg) write (27, 9993) (vmax_soil (i, k),
     &                      k = 1, nc1)
         if(f_neterosimg) write (28, 9993) ((detach_tot (i, k)
     &                  - depos_tot (i, k))
     &                     * uc1, k = 1, nc1)
         if(f_raindetimg) write (29, 9993) (raindrop_detach_tot (i, k)
     &                    * uc1,  k = 1, nc1)
         if(f_flowdetimg) write (30, 9993) (flow_detach_tot (i, k)
     &                  * uc1, k = 1, nc1)
cLTOct2007 added in output total nutrient flux.
         if(f_p_nitrateimg) write (31, 9993) (amm_tot (i,k), k = 1, nc1)     ! check units (grams)
         if(f_p_ammoniumimg) write (32, 9993) (nit_tot(i,k), k = 1, nc1)     ! check units
         if(f_p_TNimg) write (33, 9993) (TN_tot (i,k), k = 1, nc1)     ! check units
         if(f_p_TPimg) write (34, 9993) (TP_tot (i,k), k = 1, nc1)     ! check units
         if(f_p_ICimg) write (35, 9993) (IC_tot (i,k), k = 1, nc1)     ! check units
         if(f_p_TCimg)  write (36, 9993) (TC_tot (i,k), k = 1, nc1)     ! check units
        enddo
c
c   Output parameter file
c
       call echo_params (ieighteen)

c
c  close file units for use next time
c
       do i = 11, 30
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
