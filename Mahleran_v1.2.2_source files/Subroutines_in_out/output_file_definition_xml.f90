!****************************************************************
!  subroutine to define output files (maps and time series)
!  JW May 2014 modified to account for xml input changes
!****************************************************************


subroutine output_file_definition_xml


use shared_data
use parameters_from_xml

!implicit double precision (a - h, o - z)
!implicit integer (i - n)
implicit none

integer istate 
character (50) :: dummy

write (6, *) 'defining output files'
! for time series:
if (d_xsect) then
   f_d_xsect = .TRUE.
else
   f_d_xsect = .FALSE.
endif
if (dischfile_xml) then
   f_dischfile = .TRUE.
else
   f_dischfile = .FALSE.
endif
if (hydrofile_xml) then
   f_hydrofile = .TRUE.
else
   f_hydrofile = .FALSE.
endif
if (nutpointfile_xml) then
   f_nutpointfile = .TRUE.
else
   f_nutpointfile = .FALSE.
endif
if (nutrientfile_xml) then
   f_nutrientfile = .TRUE.
else
   f_nutrientfile = .FALSE.
endif
if (p_nutfile_xml) then
   f_p_nutfile = .TRUE.
else
   f_p_nutfile = .FALSE.
endif
if (q_xsect) then
   f_q_xsect = .TRUE.
else
   f_q_xsect = .FALSE.
endif
if (qpointfile_xml) then
   f_qpointfile = .TRUE.
else
   f_qpointfile = .FALSE.
endif
if (qspointfile_xml) then
   f_qspointfile = .TRUE.
else
   f_qspointfile = .FALSE.
endif
if (sedconcfile_xml) then
   f_sedconcfile = .TRUE.
else
   f_sedconcfile = .FALSE.
endif
if (seddeposfile_xml) then
   f_seddeposfile = .TRUE.
else
   f_seddeposfile = .FALSE.
endif
if (seddepthfile_xml) then
   f_seddepthfile = .TRUE.
else
   f_seddepthfile = .FALSE.
endif
if (seddetfile_xml) then
   f_seddetfile = .TRUE.
else
   f_seddetfile = .FALSE.
endif
if (seddischfile_xml) then
   f_seddischfile = .TRUE.
else
   f_seddischfile = .FALSE.
endif
if (sedfile_xml) then
   f_sedfile = .TRUE.
else
   f_sedfile = .FALSE.
endif
if (sedpropnfile_xml) then
   f_sedpropnfile = .TRUE.
else
   f_sedpropnfile = .FALSE.
endif
if (sedvelfile_xml) then
   f_sedvelfile = .TRUE.
else
   f_sedvelfile = .FALSE.
endif
if (shrubland_xsect) then
   f_shrubland_xsect = .TRUE.
else
   f_shrubland_xsect = .FALSE.
endif
if (v_xsect) then
   f_v_xsect = .TRUE.
else
   f_v_xsect = .FALSE.
endif
! for spatial images:
if (aspectfileimg) then
   f_aspectfileimg = .TRUE.
else
   f_aspectfileimg = .FALSE.
endif
if (contrib_xml) then
   f_contrib = .TRUE.
else
   f_contrib = .FALSE.
endif
if (depfileimg) then
   f_depfileimg = .TRUE.
else
   f_depfileimg = .FALSE.
endif
if (detfileimg) then
   f_detfileimg = .TRUE.
else
   f_detfileimg = .FALSE.
endif
if (dfileimg) then
   f_dfileimg = .TRUE.
else
   f_dfileimg = .FALSE.
endif
if (flowdetimg) then
   f_flowdetimg = .TRUE.
else
   f_flowdetimg = .FALSE.
endif
if (ksat_output) then
   f_ksat = .TRUE.
else
   f_ksat = .FALSE.
endif
if (neterosimg) then
   f_neterosimg = .TRUE.
else
   f_neterosimg = .FALSE.
endif
if (nutrifileimg) then
   f_nutrifileimg = .TRUE.
else
   f_nutrifileimg = .FALSE.
endif
if (order_xml) then
   f_order = .TRUE.
else
   f_order = .FALSE.
endif
if (p_ammoniumimg) then
   f_p_ammoniumimg = .TRUE.
else
   f_p_ammoniumimg = .FALSE.
endif
if (p_ICimg) then
   f_p_ICimg = .TRUE.
else
   f_p_ICimg = .FALSE.
endif
if (p_nitrateimg) then
   f_p_nitrateimg = .TRUE.
else
   f_p_nitrateimg = .FALSE.
endif
if (p_TCimg) then
   f_p_TCimg = .TRUE.
else
   f_p_TCimg = .FALSE.
endif
if (p_TNimg) then
   f_p_TNimg = .TRUE.
else
   f_p_TNimg = .FALSE.
endif
if (p_TPimg) then
   f_p_TPimg = .TRUE.
else
   f_p_TPimg = .FALSE.
endif
if (paramfile) then
   f_paramfile = .TRUE.
else
   f_paramfile = .FALSE.
endif
if (pave_xml) then
   f_pave = .TRUE.
else
   f_pave = .FALSE.
endif
if (qfileimg) then
   f_qfileimg = .TRUE.
else
   f_qfileimg = .FALSE.
endif
if (raindetimg) then
   f_raindetimg = .TRUE.
else
   f_raindetimg = .FALSE.
endif
if (rainmask) then
   f_rainmask = .TRUE.
else
   f_rainmask = .FALSE.
endif
if (sedfileimg) then
   f_sedfileimg = .TRUE.
else
   f_sedfileimg = .FALSE.
endif
if (slope_xml) then
   f_slope = .TRUE.
else
   f_slope = .FALSE.
endif
if (soildepfileimg) then
   f_soildepfileimg = .TRUE.
else
   f_soildepfileimg = .FALSE.
endif
if (soilvelfileimg) then
   f_soilvelfileimg = .TRUE.
else
   f_soilvelfileimg = .FALSE.
endif
if (thetafileimg_xml) then
   f_thetafileimg = .TRUE.
else
   f_thetafileimg = .FALSE.
endif
if (topog) then
   f_topog = .TRUE.
else
   f_topog = .FALSE.
endif
if (veg_xml) then
   f_veg = .TRUE.
else
   f_veg = .FALSE.
endif
if (vfileimg) then
   f_vfileimg = .TRUE.
else
   f_vfileimg = .FALSE.
endif

!
!  Files for MiC output
!
if (markerfileimg_xml) then
   f_markerfileimg = .TRUE.
else
   f_markerfileimg = .FALSE.
endif
if (markerofffile_xml) then
   f_markerofffile = .TRUE.
else
   f_markerofffile = .FALSE.
endif
if (mstatusfile_xml) then
   f_mstatusfile = .TRUE.
else
   f_mstatusfile = .FALSE.
endif
if (MXYfile_xml) then
   f_MXYfile = .TRUE.
   f_MXYfinalfile = .TRUE.
else
   f_MXYfile = .FALSE.
   f_MXYfinalfile = .FALSE.
endif
if (dmapfile_xml) then
   f_dmapfile = .TRUE.
else
   f_dmapfile = .FALSE.
endif

!
! "orphaned" outputs relative to original output_file_definition
!
!if (hypointsfile) then
!   f_hypointsfile = .TRUE.
!else
!   f_hypointsfile = .FALSE.
!endif
!if (inundfile) then
!   f_inundfile = .TRUE.
!else
!   f_inundfile = .FALSE.
!endif


!if (nitratefileimg) then
!   f_nitratefileimg = .TRUE.
!else
!   f_nitratefileimg = .FALSE.
!endif
!if (phosfileimg) then
!   f_phosfileimg = .TRUE.
!else
!   f_phosfileimg = .FALSE.
!endif

write (6, *) 
write (6, *) 'finished defining output files'
write (6, *) 

return
end
