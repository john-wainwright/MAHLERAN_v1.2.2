c****************************************************************
c  subroutine to define output files (maps and time series)
c****************************************************************


       subroutine output_file_definition


       use shared_data
       implicit double precision (a - h, o - z)
       implicit integer (i - n)
       CHARACTER (50) :: dummy




cEva09: create output files as specified in outfiles.dat
      f_hydrofile=.FALSE.   !disable all output files
      f_sedfile=.FALSE.
      f_dischfile=.FALSE.
      f_nutrientfile=.FALSE.
      f_seddetfile=.FALSE.
      f_seddeposfile=.FALSE.
      f_seddepthfile=.FALSE.
      f_sedvelfile=.FALSE.
      f_seddischfile=.FALSE.
      f_sedpropnfile=.FALSE.
      f_sedconcfile=.FALSE.
      f_v_xsect=.FALSE.
      f_d_xsect=.FALSE.
      f_q_xsect=.FALSE.
      f_shrubland_xsect=.FALSE.
      f_p_nutfile=.FALSE.
      f_qpointfile=.FALSE.
      f_qspointfile=.FALSE.
      f_nutpointfile=.FALSE.
cEva09: for spatial images
      f_thetafileimg=.FALSE.
      f_nutrifileimg=.FALSE.
      f_qfileimg=.FALSE.
      f_aspectfileimg=.FALSE.
      f_dfileimg=.FALSE.
      f_vfileimg=.FALSE.
      f_sedfileimg=.FALSE.
      f_detfileimg=.FALSE.
      f_depfileimg=.FALSE.
      f_soildepfileimg=.FALSE.
      f_soilvelfileimg=.FALSE.
      f_neterosimg=.FALSE.
      f_raindetimg=.FALSE.
      f_flowdetimg=.FALSE.
      f_p_nitrateimg=.FALSE.
      f_p_ammoniumimg=.FALSE.
      f_p_TNimg=.FALSE.
      f_p_TPimg=.FALSE.
      f_p_ICimg=.FALSE.
      f_p_TCimg=.FALSE.
      f_paramfile=.FALSE.
      f_paramfile=.FALSE.
      f_ksat=.FALSE.
      f_pave=.FALSE.
      f_rainmask=.FALSE.
      f_topog=.FALSE.
      f_veg=.FALSE.
      f_contrib=.FALSE.
      f_order=.FALSE.
      f_slope=.FALSE.

       OPEN(11,FILE=pfadin(1:pfadi)// 'outfiles.dat',IOSTAT=istate,
     &  STATUS='old')
       IF (istate==0) THEN
        READ(11,*,IOSTAT=istate)dummy
          READ(11,*,IOSTAT=istate)dummy
        DO WHILE (istate==0)
c     enable/disable file output as selected in outfiles.dat
            SELECT CASE (trim(dummy))
                CASE ('f_hydrofile')
                f_hydrofile=.TRUE.
                CASE ('f_sedfile')
                f_sedfile=.TRUE.
                CASE ('f_dischfile')
                f_dischfile=.TRUE.
                CASE ('f_nutrientfile')
                f_nutrientfile=.TRUE.
                CASE ('f_seddetfile')
                f_seddetfile=.TRUE.
                CASE ('f_seddeposfile')
                f_seddeposfile=.TRUE.
                CASE ('f_seddepthfile')
                f_seddepthfile=.TRUE.
                CASE ('f_sedvelfile')
                f_sedvelfile=.TRUE.
                CASE ('f_seddischfile')
                f_seddischfile=.TRUE.
                CASE ('f_sedpropnfile')
                f_sedpropnfile=.TRUE.
                CASE ('f_sedconcfile')
                f_sedconcfile=.TRUE.
                CASE ('f_v_xsect')
                f_v_xsect=.TRUE.
                CASE ('f_d_xsect')
                f_d_xsect=.TRUE.
                CASE ('f_q_xsect')
                f_q_xsect=.TRUE.
                CASE ('f_shrubland_xsect')
                f_shrubland_xsect=.TRUE.
                CASE ('f_p_nutfile')
                f_p_nutfile=.TRUE.
                CASE ('f_qpointfile')
                f_qpointfile=.TRUE.
                CASE ('f_qspointfile')
                f_qspointfile=.TRUE.
                CASE ('f_nutpointfile')
                f_nutpointfile=.TRUE.
c for spatial images:
                CASE ('f_thetafileimg')
                f_thetafileimg=.TRUE.
                CASE ('f_nutrifileimg')
                f_nutrifileimg=.TRUE.
                CASE ('f_qfileimg')
                f_qfileimg=.TRUE.
                CASE ('f_aspectfileimg')
                f_aspectfileimg=.TRUE.
                CASE ('f_dfileimg')
                f_dfileimg=.TRUE.
                CASE ('f_vfileimg')
                f_vfileimg=.TRUE.
                CASE ('f_sedfileimg')
                f_sedfileimg=.TRUE.
                CASE ('f_detfileimg')
                f_detfileimg=.TRUE.
                CASE ('f_depfileimg')
                f_depfileimg=.TRUE.
                CASE ('f_soildepfileimg')
                f_soildepfileimg=.TRUE.
                CASE ('f_soilvelfileimg')
                f_soilvelfileimg=.TRUE.
                CASE ('f_neterosimg')
                f_neterosimg=.TRUE.
                CASE ('f_raindetimg')
                f_raindetimg=.TRUE.
                CASE ('f_flowdetimg')
                f_flowdetimg=.TRUE.
                CASE ('f_p_nitrateimg')
                f_p_nitrateimg=.TRUE.
                CASE ('f_p_ammoniumimg')
                f_p_ammoniumimg=.TRUE.
                CASE ('f_p_TNimg')
                f_p_TNimg=.TRUE.
                CASE ('f_p_TPimg')
                f_p_TPimg=.TRUE.
                CASE ('f_p_ICimg')
                f_p_ICimg=.TRUE.
                CASE ('f_p_TCimg')
                f_p_TCimg=.TRUE.
                CASE ('f_paramfile')
                f_paramfile=.TRUE.
                CASE ('f_ksat')
                f_ksat=.TRUE.
                CASE ('f_pave')
                f_pave=.TRUE.
                CASE ('f_rainmask')
                f_rainmask=.TRUE.
                CASE ('f_topog')
                f_topog=.TRUE.
                CASE ('f_veg')
                f_veg=.TRUE.
                CASE ('f_contrib')
                f_contrib=.TRUE.
                CASE ('f_order')
                f_order=.TRUE.
                CASE ('f_slope')
                f_slope=.TRUE.

            END SELECT
            READ(11,*,IOSTAT=istate)dummy   !try to read next line
        END DO
          CLOSE(11)
        ELSE
        WRITE(*,*)pfadin(1:pfadi)// 'outfiles.dat could not
     &      be opened. Using default output files.'
c     these are the default output files:
        f_hydrofile=.TRUE.
        f_sedfile=.TRUE.
        f_dischfile=.TRUE.
        f_nutrientfile=.TRUE.
        f_seddetfile=.TRUE.
        f_seddeposfile=.TRUE.
        f_seddepthfile=.TRUE.
        f_sedvelfile=.TRUE.
        f_seddischfile=.TRUE.
        f_sedpropnfile=.TRUE.
        f_sedconcfile=.TRUE.
        f_v_xsect=.TRUE.
          f_d_xsect=.TRUE.
        f_q_xsect=.TRUE.
        f_shrubland_xsect=.TRUE.
        f_p_nutfile=.TRUE.
        f_qpointfile=.TRUE.
        f_qspointfile=.TRUE.
        f_nutpointfile=.TRUE.
        f_thetafileimg=.TRUE.
        f_nutrifileimg=.TRUE.
        f_qfileimg=.TRUE.
        f_aspectfileimg=.TRUE.
        f_dfileimg=.TRUE.
        f_vfileimg=.TRUE.
        f_sedfileimg=.TRUE.
        f_detfileimg=.TRUE.
        f_depfileimg=.TRUE.
        f_soildepfileimg=.TRUE.
        f_soilvelfileimg=.TRUE.
        f_neterosimg=.TRUE.
        f_raindetimg=.TRUE.
        f_flowdetimg=.TRUE.
        f_p_nitrateimg=.TRUE.
        f_p_ammoniumimg=.TRUE.
        f_p_TNimg=.TRUE.
        f_p_TPimg=.TRUE.
        f_p_ICimg=.TRUE.
        f_p_TCimg=.TRUE.
        f_paramfile=.TRUE.
        f_ksat=.TRUE.
        f_pave=.TRUE.
        f_rainmask=.TRUE.
        f_topog=.TRUE.
        f_veg=.TRUE.
        f_contrib=.TRUE.
        f_order=.TRUE.
        f_slope=.TRUE.
       END IF

      return
      end
