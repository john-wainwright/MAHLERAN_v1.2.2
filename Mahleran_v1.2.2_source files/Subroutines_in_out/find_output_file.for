c****************************************************************
c  subroutine to find first empty output file number
c
c
****************************************************************

       subroutine find_output_file

       use shared_data
       implicit double precision (a - h, o - z)
       implicit integer (i - n)
       integer :: status
       logical fexist, exists
       character *60 thetafileimg
       CHARACTER (30) :: dummy

cEva09c       data thetafileimg / pfadout(1:pfado)//'theta001.asc' /
       iout=1
       thetafileimg=pfadout(1:pfado)//'theta001.asc'

       hydrofile = pfadout(1:pfado)//'hydro000.dat'
       sedfile = pfadout(1:pfado)//'sedtr000.dat'
       dischfile = pfadout(1:pfado)//'disch000.dat'
       nutrientfile = pfadout(1:pfado)//'nutri000.dat'
       seddetfile = pfadout(1:pfado)//'seddetach000.dat'
       seddeposfile =pfadout(1:pfado)// 'seddepos000.dat'
       seddepthfile = pfadout(1:pfado)//'seddepth000.dat'
       sedvelfile = pfadout(1:pfado)//'sedveloc000.dat'
       seddischfile = pfadout(1:pfado)//'seddisch000.dat'
       sedpropnfile = pfadout(1:pfado)//'sedpropn000.dat'
       sedconcfile = pfadout(1:pfado)//'sedconcn000.dat'
       qpointfile = pfadout(1:pfado)//'hypnt000.dat'
       qspointfile = pfadout(1:pfado)//'sedpt000.dat'
       nutpointfile = pfadout(1:pfado)//'nutpt000.dat'
       p_nutfile = pfadout(1:pfado)//'p_nut000.dat'

c
c   loop to find first thetaxxx.asc file that does not already exist
c
       fexist = .FALSE.
   1   continue
       if (iout.lt.10) then
          write (thetafileimg (8+pfado:8+pfado), '(i1)') iout
       elseif (iout.lt.100) then
          write (thetafileimg (7+pfado:8+pfado), '(i2)') iout
       elseif (iout.lt.1000) then
          write (thetafileimg (6+pfado:8+pfado), '(i3)') iout
       endif
       inquire (file = thetafileimg, exist = fexist, err = 2)
       go to 3
   2   continue
       fexist = .FALSE.
   3   continue
       if (fexist) then
c
c   file already exists, try next
c
          iout = iout + 1
          go to 1
       endif

      if (iout.lt.10) then
          write (hydrofile (8+pfado:8+pfado), '(i1)') iout
          write (sedfile (8+pfado:8+pfado), '(i1)') iout
        write (dischfile (8+pfado:8+pfado), '(i1)') iout
        write (nutrientfile (8+pfado:8+pfado), '(i1)') iout
          write (seddetfile (12+pfado:12+pfado), '(i1)') iout
          write (seddeposfile (11+pfado:11+pfado), '(i1)') iout
          write (seddepthfile (11+pfado:11+pfado), '(i1)') iout
          write (sedvelfile (11+pfado:11+pfado), '(i1)') iout
          write (seddischfile (11+pfado:11+pfado), '(i1)') iout
          write (sedpropnfile (11+pfado:11+pfado), '(i1)') iout
          write (sedconcfile (11+pfado:11+pfado), '(i1)') iout
          write (qpointfile (8+pfado:8+pfado), '(i1)') iout
          write (qspointfile (8+pfado:8+pfado), '(i1)') iout
          write (nutpointfile (8+pfado:8+pfado), '(i1)') iout
          write (p_nutfile (8+pfado:8+pfado), '(i1)') iout
       elseif (iout.lt.100) then
          write (hydrofile (7+pfado:8+pfado), '(i2)') iout
          write (sedfile (7+pfado:8+pfado), '(i2)') iout
        write (dischfile (7+pfado:8+pfado), '(i2)') iout
        write (nutrientfile (7+pfado:8+pfado), '(i2)') iout
          write (seddetfile (11+pfado:12+pfado), '(i2)') iout
          write (seddeposfile (10+pfado:11+pfado), '(i2)') iout
          write (seddepthfile (10+pfado:11+pfado), '(i2)') iout
          write (sedvelfile (10+pfado:11+pfado), '(i2)') iout
          write (seddischfile (10+pfado:11+pfado), '(i2)') iout
          write (sedpropnfile (10+pfado:11+pfado), '(i2)') iout
          write (sedconcfile (10+pfado:11+pfado), '(i2)') iout
          write (qpointfile (7+pfado:8+pfado), '(i2)') iout
          write (qspointfile (7+pfado:8+pfado), '(i2)') iout
          write (nutpointfile (7+pfado:8+pfado), '(i2)') iout
          write (p_nutfile (7+pfado:8+pfado), '(i2)') iout
       elseif (iout.lt.1000) then
          write (hydrofile (6+pfado:8+pfado), '(i3)') iout
          write (sedfile (6+pfado:8+pfado), '(i3)') iout
        write (dischfile (6+pfado:8+pfado), '(i3)') iout
        write (nutrientfile (6+pfado:8+pfado), '(i3)') iout
          write (seddetfile (10+pfado:12+pfado), '(i3)') iout
          write (seddeposfile (9+pfado:11+pfado), '(i3)') iout
          write (seddepthfile (9+pfado:11+pfado), '(i3)') iout
          write (sedvelfile (9+pfado:11+pfado), '(i3)') iout
          write (seddischfile (9+pfado:11+pfado), '(i3)') iout
          write (sedpropnfile (9+pfado:11+pfado), '(i3)') iout
          write (sedconcfile (9+pfado:11+pfado), '(i3)') iout
          write (qpointfile (6+pfado:8+pfado), '(i3)') iout
          write (qspointfile (6+pfado:8+pfado), '(i3)') iout
          write (nutpointfile (6+pfado:8+pfado), '(i3)') iout
          write (p_nutfile (6+pfado:8+pfado), '(i3)') iout
       endif

c     Call outfiles.dat to check which output files will be generated
      call output_file_definition

c     Open / create requested outputfile
      if (f_hydrofile) open (57, file = hydrofile, status = 'unknown')
      if (f_sedfile) open (58, file = sedfile, status = 'unknown')
      if (f_dischfile) open (59, file = dischfile, status = 'unknown')
      if (f_nutrientfile)open (60, file = nutrientfile, status =
     &  'unknown')
      if (f_seddetfile) open (100, file = seddetfile, status ='unknown')
      if (f_seddeposfile) open (101, file = seddeposfile, status =
     &  'unknown')
      if (f_seddepthfile) open (102, file = seddepthfile, status =
     &  'unknown')
      if (f_sedvelfile) open (103, file = sedvelfile, status ='unknown')
      if (f_seddischfile) open (104, file = seddischfile, status =
     &  'unknown')
      if (f_sedpropnfile) open (105, file = sedpropnfile, status =
     &  'unknown')
      if (f_sedconcfile) open (106, file = sedconcfile, status =
     &  'unknown')
      if (f_v_xsect) open (107, file = pfadout(1:pfado)//'v_xsect.dat',
     &      status = 'unknown')
      if (f_d_xsect) open (108, file = pfadout(1:pfado)//'d_xsect.dat',
     &      status = 'unknown')
      if (f_q_xsect) open (109, file = pfadout(1:pfado)//'q_xsect.dat',
     &      status = 'unknown')
      if (f_shrubland_xsect) open (99, file = pfadout(1:pfado)//
     &  'shrubland_xsect.dat',
     &      status = 'unknown')
      if (f_p_nutfile) open (55, file = p_nutfile, status = 'unknown')
      if (hydro_out) then
         if (f_qpointfile) then
            open (110, file = qpointfile, status = 'unknown')
          write (110, '("time", 1000 (1x, "[", i4, "," i4, "]"))')
     &             (x_hypoints (i), y_hypoints (i), i = 1, n_hypoints)
         endif
         if (f_qspointfile) then
          open (111, file = qspointfile, status = 'unknown')
          write (111, '("time", 1000 (1x, "[", i4, "," i4, "]"))')
     &             (x_hypoints (i), y_hypoints (i), i = 1, n_hypoints)
        endif
         if (f_nutpointfile) then
        open (112, file = nutpointfile, status = 'unknown')
          write (112, '("time", 1000 (1x, "[", i4, "," i4,
     &             "]_NO3 NH4 PO4"))')
     &             (x_hypoints (i), y_hypoints (i), i = 1, n_hypoints)
        endif
      endif


      return
      end
