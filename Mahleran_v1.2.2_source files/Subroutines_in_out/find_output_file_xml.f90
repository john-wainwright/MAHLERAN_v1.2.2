!****************************************************************
!  subroutine to find first empty output file number
!
!
!****************************************************************

subroutine find_output_file_xml

use shared_data
use parameters_from_xml
!    implicit double precision (a - h, o - z)
!       implicit integer (i - n)
implicit none

integer :: status
integer :: i
logical fexist, exists
character *120 theta_file_img
!character (30) :: dummy

theta_file_img = output_folder (1:output_folder_length) // 'theta001.asc'

hydrofile = output_folder (1:output_folder_length) // 'hydro000.dat'
sedfile = output_folder (1:output_folder_length) // 'sedtr000.dat'
dischfile = output_folder (1:output_folder_length) // 'disch000.dat'
nutrientfile = output_folder (1:output_folder_length) // 'nutri000.dat'
seddetfile = output_folder (1:output_folder_length) // 'seddetach000.dat'
seddeposfile =output_folder (1:output_folder_length) //  'seddepos000.dat'
seddepthfile = output_folder (1:output_folder_length) // 'seddepth000.dat'
sedvelfile = output_folder (1:output_folder_length) // 'sedveloc000.dat'
seddischfile = output_folder (1:output_folder_length) // 'seddisch000.dat'
sedpropnfile = output_folder (1:output_folder_length) // 'sedpropn000.dat'
sedconcfile = output_folder (1:output_folder_length) // 'sedconcn000.dat'
qpointfile = output_folder (1:output_folder_length) // 'hypnt000.dat'
qspointfile = output_folder (1:output_folder_length) // 'sedpt000.dat'
nutpointfile = output_folder (1:output_folder_length) // 'nutpt000.dat'
p_nutfile = output_folder (1:output_folder_length) // 'p_nut000.dat'

!
!   loop to find first thetaxxx.asc file that does not already exist
!
iout = 1
fexist = .TRUE.
do while (fexist)
   if (iout.lt.10) then
      write (theta_file_img (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
   elseif (iout.lt.100) then
      write (theta_file_img (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
   elseif (iout.lt.1000) then
      write (theta_file_img (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
   endif
   write (6, *) 'checking if ', trim (theta_file_img), ' exists'
   inquire (file = theta_file_img, exist = fexist, err = 2)
   go to 3
      2   continue
          fexist = .FALSE.
      3   continue
   if (fexist) then
!
!   file already exists, try next
!
      write (6, *) ' - found it, checking for next file'
      iout = iout + 1
   endif
enddo

if (iout.lt.10) then
   write (hydrofile (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
   write (sedfile (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
   write (dischfile (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
   write (nutrientfile (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
   write (seddetfile (12 + output_folder_length:12 + output_folder_length), '(i1)') iout
   write (seddeposfile (11 + output_folder_length:11 + output_folder_length), '(i1)') iout
   write (seddepthfile (11 + output_folder_length:11 + output_folder_length), '(i1)') iout
   write (sedvelfile (11 + output_folder_length:11 + output_folder_length), '(i1)') iout
   write (seddischfile (11 + output_folder_length:11 + output_folder_length), '(i1)') iout
   write (sedpropnfile (11 + output_folder_length:11 + output_folder_length), '(i1)') iout
   write (sedconcfile (11 + output_folder_length:11 + output_folder_length), '(i1)') iout
   write (qpointfile (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
   write (qspointfile (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
   write (nutpointfile (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
   write (p_nutfile (8 + output_folder_length:8 + output_folder_length), '(i1)') iout
elseif (iout.lt.100) then
   write (hydrofile (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
   write (sedfile (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
   write (dischfile (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
   write (nutrientfile (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
   write (seddetfile (11 + output_folder_length:12 + output_folder_length), '(i2)') iout
   write (seddeposfile (10 + output_folder_length:11 + output_folder_length), '(i2)') iout
   write (seddepthfile (10 + output_folder_length:11 + output_folder_length), '(i2)') iout
   write (sedvelfile (10 + output_folder_length:11 + output_folder_length), '(i2)') iout
   write (seddischfile (10 + output_folder_length:11 + output_folder_length), '(i2)') iout
   write (sedpropnfile (10 + output_folder_length:11 + output_folder_length), '(i2)') iout
   write (sedconcfile (10 + output_folder_length:11 + output_folder_length), '(i2)') iout
   write (qpointfile (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
   write (qspointfile (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
   write (nutpointfile (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
   write (p_nutfile (7 + output_folder_length:8 + output_folder_length), '(i2)') iout
elseif (iout.lt.1000) then
   write (hydrofile (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
   write (sedfile (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
   write (dischfile (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
   write (nutrientfile (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
   write (seddetfile (10 + output_folder_length:12 + output_folder_length), '(i3)') iout
   write (seddeposfile (9 + output_folder_length:11 + output_folder_length), '(i3)') iout
   write (seddepthfile (9 + output_folder_length:11 + output_folder_length), '(i3)') iout
   write (sedvelfile (9 + output_folder_length:11 + output_folder_length), '(i3)') iout
   write (seddischfile (9 + output_folder_length:11 + output_folder_length), '(i3)') iout
   write (sedpropnfile (9 + output_folder_length:11 + output_folder_length), '(i3)') iout
   write (sedconcfile (9 + output_folder_length:11 + output_folder_length), '(i3)') iout
   write (qpointfile (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
   write (qspointfile (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
   write (nutpointfile (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
   write (p_nutfile (6 + output_folder_length:8 + output_folder_length), '(i3)') iout
endif

!   Call outfiles.dat to check which output files will be generated
call output_file_definition_xml

!   Open / create requested outputfile
if (f_hydrofile) open (57, file = hydrofile, status = 'unknown')
if (f_sedfile) open (58, file = sedfile, status = 'unknown')
if (f_dischfile) open (59, file = dischfile, status = 'unknown')
if (f_nutrientfile)open (60, file = nutrientfile, status = 'unknown')
if (f_seddetfile) open (100, file = seddetfile, status ='unknown')
if (f_seddeposfile) open (101, file = seddeposfile, status = 'unknown')
if (f_seddepthfile) open (102, file = seddepthfile, status = 'unknown')
if (f_sedvelfile) open (103, file = sedvelfile, status ='unknown')
if (f_seddischfile) open (104, file = seddischfile, status = 'unknown')
if (f_sedpropnfile) open (105, file = sedpropnfile, status = 'unknown')
if (f_sedconcfile) open (106, file = sedconcfile, status = 'unknown')
if (f_v_xsect) open (107, file = output_folder (1:output_folder_length) // 'v_xsect.dat', status = 'unknown')
if (f_d_xsect) open (108, file = output_folder (1:output_folder_length) // 'd_xsect.dat', status = 'unknown')
if (f_q_xsect) open (109, file = output_folder (1:output_folder_length) // 'q_xsect.dat', status = 'unknown')
if (f_shrubland_xsect) open (99, file = output_folder (1:output_folder_length) //  'shrubland_xsect.dat', status = 'unknown')
if (f_p_nutfile) open (55, file = p_nutfile, status = 'unknown')
if (hydro_out) then
   if (f_qpointfile) then
      open (110, file = qpointfile, status = 'unknown')
      write (110, '("time", 1000 (1x, "[", i4, "," i4, "]"))') (x_hypoints (i), y_hypoints (i), i = 1, n_hypoints)
   endif
   if (f_qspointfile) then
      open (111, file = qspointfile, status = 'unknown')
      write (111, '("time", 1000 (1x, "[", i4, "," i4, "]"))') (x_hypoints (i), y_hypoints (i), i = 1, n_hypoints)
   endif
   if (f_nutpointfile) then
      open (112, file = nutpointfile, status = 'unknown')
      write (112, '("time", 1000 (1x, "[", i4, "," i4, "]_NO3 NH4 PO4"))') &
             (x_hypoints (i), y_hypoints (i), i = 1, n_hypoints)
   endif
endif

write (6, *) ' Leaving find_output_file_xml, iout = ', iout
return
end
