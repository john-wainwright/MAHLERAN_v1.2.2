c****************************************************************
c  subroutine to read parameters for simulation
c
c
cLT_Oct2007 added into input file, the nutrient concentration (mg/g) of each phi class.
c
c
c****************************************************************
       subroutine read_parameters

       use shared_data
c      USE DFLIB
       implicit double precision (a - h, o - z)
       implicit integer (i - n)

       integer *4 isix, len
c      CHARACTER($MAXPATH) buf
c      CHARACTER(100) buf

c      integer phi
c
c   will always read data from file "ncfmodin.dat", if it exists.
c   Otherwise, will request whether user wants to define file with
c      input data, or input directly from screen
c
       logical fexist, fexist1, exists
       logical (4) results, mkdir
       character *80 filename, fname1, file, file1

       data isix / 6 /
       filename = 'mahleran_input.dat'
       fexist = .FALSE.

c
cb   check if default input file (mahleran_input.dat) is present
c
cb  if ncfmodin.dat exists then fexist = true then go to 2, else fexist = false then go to 1
       inquire (file = filename, exist = fexist, err = 1)
       go to 2
   1   continue
       fexist = .FALSE.
   2   continue
c
cb   if fexist = false then read data from screen or redefine default file
c
       if (.not.fexist) then
          write (6, *) ' Enter "*" to input from screen, or '//
     &                 'give name of data file '
          read (5, '(a)') fname1
          if (fname1.eq.'*') then
             go to 3
          endif
          inquire (file = fname1, exist = fexist1, err = 3)
          go to 4
   3      continue
          fexist1 = .FALSE.
   4      continue
          if (fexist1) then
             fexist = .TRUE.
             filename = fname1
             write (6, *) ' Reading data from file ', fname1
cb  else if fname1 does not exist (fexist = false) then enter input from screen
          else
             write (6, *) ' enter name of .asc file containing '//
     &                    'topographic data (without extension)'
             read (5, '(a)') file
             ifile_point = 1
   6         continue
cb  if the end of the filename is reached then attach the .doc and .img extensions to the doc
cb  file and the topo files respectively
                if (file (ifile_point: ifile_point).eq.' ') then
                   go to 7
                endif
                ifile_point = ifile_point + 1
                go to 6
   7         continue
             ifile_point = ifile_point - 1
cJWMay2005             doc_file = file (1: ifile_point) // '.doc'
             topo_file = file (1: ifile_point) // '.asc'
             write (6, *) ' enter name of .asc file containing '//
     &                    'vegetation data (without extension)'
             read (5, '(a)') file
             ifile_point = 1
  11         continue
                if (file (ifile_point: ifile_point).eq.' ') then
                   go to 12
                endif
                ifile_point = ifile_point + 1
                go to 11
  12         continue
             ifile_point = ifile_point - 1
cJWMay2005             veg_doc_file = file (1: ifile_point) // '.doc'
             veg_file = file (1: ifile_point) // '.asc'
             write (6, *) ' enter name of .asc file containing '//
     &                    'ksat data (without extension)'
             read (5, '(a)') file
             ifile_point = 1
  24         continue
                if (file (ifile_point: ifile_point).eq.' ') then
                   go to 23
                endif
                ifile_point = ifile_point + 1
                go to 24
  23         continue
             ifile_point = ifile_point - 1
cJWMay2005             ksat_doc_file = file (1: ifile_point) // '.doc'
             ksat_file = file (1: ifile_point) // '.asc'
             write (6, *) ' enter name of data file containing '//
     &                    'storm intensity data (with extension)'
             read (5, '(a)') rainfile
             write (6, *) ' enter name of .asc file containing '//
     &                    'rainfall mask data (without extension)'
             read (5, '(a)') file
             ifile_point = 1
  13         continue
                if (file (ifile_point: ifile_point).eq.' ') then
                   go to 14
                endif
                ifile_point = ifile_point + 1
                go to 13
  14         continue
             ifile_point = ifile_point - 1
cJWMay2005             rmask_doc_file = file (1: ifile_point) // '.doc'
             rmask_file = file (1: ifile_point) // '.asc'
             write (6, *) ' enter name of .asc file containing '//
     &                    'pavement cover data (without extension)'
             read (5, '(a)') file
             ifile_point = 1
  21         continue
                if (file (ifile_point: ifile_point).eq.' ') then
                   go to 22
                endif
                ifile_point = ifile_point + 1
                go to 21
  22         continue
             ifile_point = ifile_point - 1
cJWMay2005             pavement_doc_file = file (1: ifile_point) // '.doc'
             pavement_file = file (1: ifile_point) // '.asc'
             write (6, *) ' Enter model type: '
             write (6, *) '    1. average '
             write (6, *) '    2. binary system '
             write (6, *) '    3. stochastic simulation '
           read (5, *) model_type
           write (6, *) ' Enter infiltration parameter type: '
             write (6, *) '    1. pavement and mean rainfall intensity '
             write (6, *) '    2. deterministic or stochastic based ',
     &                           'on values in input file '
           write (6, *) '    3. pavement and dynamic rainfall '//
     &                           'intensity'
           write (6, *) '    4. ksat map from file'
             read (5, *) inf_type
             write (6, *) ' Enter infiltration model to be used'
             write (6, *) '    1. Smith and Parlange (1978) with ksat'//
     &                    ' as parameterized'
             write (6, *) '    2. Smith and Parlange (1978) with ksat'//
     &                    ' Exponential from pavement cover and'//
     &                    ' rainfall intensity'
           read (5, *) inf_model
               write (6, *) ' Enter mean Ksat for shb, grs, mnt, deg  '//
     &                    ' areas (in mm/s) '
             read (5, *) (ksave (i), i = 1, 4)
             write (6, *) ' Enter standard deviation of Ksat for '//
     &                    ' shb, grs, mnt, deg (in mm/s) '
             read (5, *) (kssd (i), i = 1, 4)
             write (6, *) ' Enter mean wetting-front suction for '//
     &                    'shb, grs, mnt, deg (in mm)'
             read (5, *) (psiave (i), i = 1, 4)
             write (6, *) ' Enter standard deviation of '//
     &                    'wetting-front suction for '//
     &                    'shb, grs, mnt, deg (in mm)'
             read (5, *) (psisd (i), i = 1, 4)
             write (6, *) ' Enter mean drainage parameter for '//
     &                    'shb, grs, mnt, deg '
             read (5, *) (drain_par_ave (i), i = 1, 4)
             write (6, *) ' Enter std. dev. of drainage parameter '//
     &                    'for shb, grs, mnt, deg '
             read (5, *) (drain_par_sd (i), i = 1, 4)

cLTOct2007 Replace
c            write (6, *) ' Enter Theta_0 for shb, grs, mnt, deg '
c            read (5, *) (theta_0 (i), i = 1, 4)
c          with:
           write (6, *) ' enter name of .asc file containing '//
     &                    'soil moisture data (without extension)'
             read (5, '(a)') file
             ifile_point = 1
  35       continue
                if (file (ifile_point: ifile_point).eq.' ') then
                   go to 36
                endif
                ifile_point = ifile_point + 1
                go to 35
  36         continue
             ifile_point = ifile_point - 1
cLTOct2007             sm_doc_file = file (1: ifile_point) // '.doc'
             sm_file = file (1: ifile_point) // '.asc'
cLTOct2007 Replace
c             write (6, *) ' Enter Theta_sat for shb, grs, mnt, deg '
c             read (5, *) (theta_sat (i), i = 1, 4)
c             with:
             write (6, *) ' enter name of .asc file containing '//
     &                    'theta_sat (without extension)'
             read (5, '(a)') file
             ifile_point = 1
  41       continue
                if (file (ifile_point: ifile_point).eq.' ') then
                   go to 42
                endif
                ifile_point = ifile_point + 1
                go to 41
  42         continue
             ifile_point = ifile_point - 1
cLTOct2007             theta_sat_doc_file = file (1: ifile_point) // '.doc'
             theta_sat_file = file (1: ifile_point) // '.asc'
             write (6, *) ' Enter soil thickness for shb, grs, mnt, deg'
             read (5, *) (soil_thick (i), i = 1, 4)
             write (6, *) ' Enter friction factor parameter type: '
             write (6, *) '    1. deterministic '
             write (6, *) '    2. stochastic '
             read (5, *) ff_type
             write (6, *) ' Enter mean ff for shb, grs, mnt, deg '
             read (5, *) (ffave (i), i = 1, 4)
             write (6, *) ' Enter standard deviation of ff for '//
     &                    'shb, grs, mnt, deg '
             read (5, *) (ffsd (i), i = 1, 4)
             write (6, *) ' Enter rainfall model: '
             write (6, *) '    1. constant average rainfall '
             write (6, *) '    2. storm read in from file '
             read (5, *) rain_type
             write (6, *) ' Enter length of simulated storm '
             read (5, *) stormlength
             write (6, *) ' Enter mean rainfall, standard deviation, '//
     &                    ' skewness (in mm/h)'
             read (5, *) rf_mean, rfvar1, rfvar2
   5         continue
             write (6, *) ' Enter number of flow directions to be '//
     &                    'used - must be 4 or 8 '
             read (5, *) ndirn
             if (ndirn.ne.4.and.ndirn.ne.8) then
                write (6, *) ' Error in input - ndirn should '//
     &                       'only be 4 or 8 '
                write (6, *)
                go to 5
             endif
  10           continue
                 write (6, *) ' Enter flow routing method to be used '
                 write (6, *) '     1. routing model after H.M. '//
     &                              'Scoging (1992)'
                 write (6, *) '     2. FTCS scheme'
                 write (6, *) '     3. Lax"s method'
                 write (6, *) '     4. Two-step Lax-Wendroff method'
                 write (6, *) '     5. Crank-Nicolson with bisection ',
     &                        'solution'
                 write (6, *) '     6. Crank-Nicolson with automatic ',
     &                        'overtopping of sinks'
                 write (6, *) '     7. Crank-Nicolson with Newton-',
     &                        'Raphson solution'
                 read (5, *) iroute
                 if (iroute.lt.1.or.iroute.gt.7) then
                     write (6, *) ' Error in input - value should be '//
     &                                'between 1 and 7'
                      write (6, *)
                      go to 10
                 endif
             write (6, *) ' Enter timestep to be used (s) '
             read (5, *) dt
             write (6, *) ' Enter rill definition technique '
             write (6, *) '    1. deterministic '
             write (6, *) '    2. fuzzy boundaries '
             read (5, *) itype
             write (6, *) ' Enter particle density (g/cm3)'
             read (5, *) density
             write (6, *) ' Enter active layer sensitivity'
             read (5, *) hz
             write (6, *) ' Enter initial particle size (means)'
c             read (5, *) (ps_init_ave (phi), phi = 1, 6)
c            write (6, *) ' Enter initial particle size (sd)'
           write (6, *) ' Read phi (1) map '
c     read phi 1
           read (5, '(a)') file
             ifile_point = 1
  56         continue
             if (file (ifile_point: ifile_point).eq.' ') then
                go to 57
             endif
             ifile_point = ifile_point + 1
             go to 56
  57         continue
             ifile_point = ifile_point - 1
cLTOct2007          phi_1_doc_file = file (1: ifile_point) // '.doc'
             phi_1_file = file (1: ifile_point) // '.asc'
           write (6, *) ' Read phi (2) map '
c     read phi 2
           read (5, '(a)') file
             ifile_point = 1
  58         continue
             if (file (ifile_point: ifile_point).eq.' ') then
                go to 59
             endif
             ifile_point = ifile_point + 1
             go to 58
  59         continue
             ifile_point = ifile_point - 1
cLTOct2007          phi_2_doc_file = file (1: ifile_point) // '.doc'
             phi_2_file = file (1: ifile_point) // '.asc'
           write (6, *) ' Read phi (3) map '
c     read phi 3
           read (5, '(a)') file
             ifile_point = 1
  60         continue
             if (file (ifile_point: ifile_point).eq.' ') then
                go to 61
             endif
             ifile_point = ifile_point + 1
             go to 60
  61         continue
             ifile_point = ifile_point - 1
cLTOct2007          phi_3_doc_file = file (1: ifile_point) // '.doc'
             phi_3_file = file (1: ifile_point) // '.asc'
           write (6, *) ' Read phi (4) map '
c     read phi 4
           read (5, '(a)') file
             ifile_point = 1
  62         continue
             if (file (ifile_point: ifile_point).eq.' ') then
                go to 63
             endif
             ifile_point = ifile_point + 1
             go to 62
  63         continue
          ifile_point = ifile_point - 1
cLTOct2007          phi_4_doc_file = file (1: ifile_point) // '.doc'
          phi_4_file = file (1: ifile_point) // '.asc'
           write (6, *) ' Read phi (5) map '
c     read phi 5
           read (5, '(a)') file
             ifile_point = 1
  64         continue
             if (file (ifile_point: ifile_point).eq.' ') then
                go to 65
             endif
             ifile_point = ifile_point + 1
             go to 64
  65         continue
             ifile_point = ifile_point - 1
cLTOct2007          phi_5_doc_file = file (1: ifile_point) // '.doc'
             phi_5_file = file (1: ifile_point) // '.asc'
           write (6, *) ' Read phi (6) map '
c     read phi 6
           read (5, '(a)') file
             ifile_point = 1
  66         continue
             if (file (ifile_point: ifile_point).eq.' ') then
                go to 67
             endif
             ifile_point = ifile_point + 1
             go to 66
  67         continue
             ifile_point = ifile_point - 1
cLTOct2007          phi_6_doc_file = file (1: ifile_point) // '.doc'
             phi_6_file = file (1: ifile_point) // '.asc'

             read (5, *) (ps_init_sd (phi), phi = 1, 6)
             write (6, *) ' Enter nutrient parameter CS ', ! LTOct2007 Cs is in mg/l
     &                    '(ammonium, nitrate, phosphorus):'
           read (5, *) (Cs (i), i = 1, 3)
             write (6, *) ' Enter mass transfer coefficients ',
     &                    '(ammonium, nitrate, phosphorus): '
           read (5, *) (alpha (i), i = 1, 3)
c
cJWFeb05  Read in detachment parameters
c
             write (6, *) ' Enter raindrop detachment a parameter '
           read (5, *) (spa (i), i = 1, 6)
             write (6, *) ' Enter raindrop detachment b parameter '
           read (5, *) (spb (i), i = 1, 6)
             write (6, *) ' Enter raindrop detachment c parameter '
           read (5, *) (spc (i), i = 1, 6)
             write (6, *) ' Enter raindrop detachment max parameter '
           read (5, *) (hs (i), i = 1, 6)
             write (6, *) ' Enter splash distribution d parameter '
           read (5, *) (spd (i), i = 1, 6)
c
cLTOct2007  Read in concentration of particulate-bound nutrient for each phi class.
c
             write (6, *) ' Enter particulate-bound ammonium conc. '
             read (5, *) (p_ammonium (i), i = 1, 6)
             write (6, *) ' Enter particulate-bound nitrate conc. '
             read (5, *) (p_nitrate (i), i = 1, 6)
             write (6, *) ' Enter particulate-bound TN conc. '
             read (5, *) (p_TN (i), i = 1, 6)
           write (6, *) ' Enter particulate-bound TP conc. '
             read (5, *) (p_TP (i), i = 1, 6)
           write (6, *) ' Enter particulate-bound IC conc. '
             read (5, *) (p_IC (i), i = 1, 6)
           write (6, *) ' Enter particulate-bound TC conc. '
             read (5, *) (p_TC (i), i = 1, 6)
c
cLTOct2007  Read in dissolved rainfall nutrient concentrations.
c
             write (6, *) ' Enter rainfall dissolved nutrient conc ',
     &                    '(ammonium, nitrate, phosphorus): '
             read (5, *) (Rn (i), i = 1, 3)




          endif
       endif
c
c  read data from default file
c
       if (fexist) then
          open (2, file = filename, status = 'unknown')
          rewind (2)
        read (2,*)
        read (2,'(a)') pfadin           !set input folder
        read (2,'(a)') pfadout      !set output folder
C   Check if the specified directory exists, if not create an output directory with the name 'Output'
c       len=FULLPATHQQ (pfadout, buf)
c       inquire(file=buf, EXIST=exists)
c       IF (.not.exists) THEN
c           WRITE (*,*) 'The specified output order
c     &     does not exist, output in folder Output_default'
c           makdir = MAKEDIRQQ('Output_default')
c           pfadout='./Output_default/'
c          END IF

c     determine length of basic model path name
        pfado = sizeof(pfadout)
      DO i=1,sizeof(pfadout)
        IF (pfadout(i:i) == ' ') THEN
          pfado=i-1
          EXIT
        END IF
      END DO

        inquire (file = pfadout (1: pfado), exist = exists)
        if (.not.exists) then
            write (6, *) 'Inquire command does not recognize '//
     &       'the specified output folder: "'// pfadout (1: pfado)//'"'
c            write (6, *) 'Please create it and re-run the model.'
c           stop
        endif

c          open (3, file = pfadout (1: pfado)//'WriteMsg.txt',
c     &             status = 'unknown')
c          read (3, *) file1
c          write (3, *) 'Here is a message'
c          close (3)
c          stop

        read (2, '(a)') file
          ifile_point = 1
   8      continue
             if (file (ifile_point: ifile_point).eq.' ') then
                go to 9
             endif
             ifile_point = ifile_point + 1
             go to 8
   9      continue
          ifile_point = ifile_point - 1
cJWMay2005          doc_file = file (1: ifile_point) // '.doc'
          topo_file = file (1: ifile_point) // '.asc'
          read (2, '(a)') file
          ifile_point = 1
  15      continue
             if (file (ifile_point: ifile_point).eq.' ') then
                go to 16
             endif
             ifile_point = ifile_point + 1
             go to 15
  16      continue
          ifile_point = ifile_point - 1
cJWMay2005          veg_doc_file = file (1: ifile_point) // '.doc'
          veg_file = file (1: ifile_point) // '.asc'
          read (2, '(a)') rainfile
          read (2, '(a)') file
          ifile_point = 1
  17      continue
             if (file (ifile_point: ifile_point).eq.' ') then
                go to 18
             endif
             ifile_point = ifile_point + 1
             go to 17
  18      continue
          ifile_point = ifile_point - 1
cJWMay2005          ksat_doc_file = file (1: ifile_point) // '.doc'
          ksat_file = file (1: ifile_point) // '.asc'
          read (2, '(a)') file
          ifile_point = 1
  19      continue
             if (file (ifile_point: ifile_point).eq.' ') then
                go to 20
             endif
             ifile_point = ifile_point + 1
             go to 19
  20      continue
          ifile_point = ifile_point - 1
cJWMay2005          rmask_doc_file = file (1: ifile_point) // '.doc'
          rmask_file = file (1: ifile_point) // '.asc'
          read (2, '(a)') file
          ifile_point = 1
  25      continue
             if (file (ifile_point: ifile_point).eq.' ') then
                go to 26
             endif
             ifile_point = ifile_point + 1
             go to 25
  26      continue
          ifile_point = ifile_point - 1
cJWMay2005          pavement_doc_file = file (1: ifile_point) // '.doc'
          pavement_file = file (1: ifile_point) // '.asc'
          read (2, '(a)') file
          ifile_point = 1
  30      continue
             if (file (ifile_point: ifile_point).eq.' ') then
                go to 31
             endif
             ifile_point = ifile_point + 1
             go to 30
  31      continue
          ifile_point = ifile_point - 1
cEva  reads in vegetation cover file
cJWMay2005          cover_doc_file = file (1: ifile_point) // '.doc'
          cover_file = file (1: ifile_point) // '.asc'
cEva  reads in model type for spatial distribution of Ksat and friction factor
          read (2, *) model_type
        read (2, *) inf_type
        read (2, *) inf_model
          read (2, *) (ksave (i), i = 1, 4)
          read (2, *) (kssd (i), i = 1, 4)
          read (2, *) (psiave (i), i = 1, 4)
          read (2, *) (psisd (i), i = 1, 4)
          read (2, *) (drain_par_ave (i), i = 1, 4)
          read (2, *) (drain_par_sd (i), i = 1, 4)

cLTOct2007read in distributed soil moisture map
c         read (2, *) (theta_0 (i), i = 1, 4)
        read (2, '(a)') file
          ifile_point = 1
  37      continue
             if (file (ifile_point: ifile_point).eq.' ') then
                go to 38
             endif
             ifile_point = ifile_point + 1
             go to 37
  38      continue
          ifile_point = ifile_point - 1
cLTOct2007          sm_doc_file = file (1: ifile_point) // '.doc'
          sm_file = file (1: ifile_point) // '.asc'

cLTOct2007read in distributed theta_sat map
c         read (2, *) (theta_sat (i), i = 1, 4)
        read (2, '(a)') file
          ifile_point = 1
  39      continue
             if (file (ifile_point: ifile_point).eq.' ') then
                go to 40
             endif
             ifile_point = ifile_point + 1
             go to 39
  40      continue
          ifile_point = ifile_point - 1
cLTOct2007          theta_sat_doc_file = file (1: ifile_point) // '.doc'
          theta_sat_file = file (1: ifile_point) // '.asc'
          read (2, *) (soil_thick (i), i = 1, 4)
          read (2, *) ff_type
          read (2, *) (ffave (i), i = 1, 4)
          read (2, *) (ffsd (i), i = 1, 4)
          read (2, *) rain_type
          read (2, *) stormlength
          read (2, *) rf_mean, rfvar1, rfvar2
          read (2, *) ndirn
          if (ndirn.ne.4.and.ndirn.ne.8) then
             write (6, *) ' Error in input - ndirn should '//
     &                    'only be 4 or 8 '
             stop
          endif
          read (2, *) iroute
          if (iroute.lt.1.or.iroute.gt.7) then
             write (6, *) ' Error in input - iroute should '//
     &                    'be between 1 and 7'
             stop
          endif
          read (2, *) dt
          read (2, *) itype
          read (2, *) density
          read (2, *) hz

c     read phi 1
        read (2, '(a)') file
          ifile_point = 1
  44      continue
             if (file (ifile_point: ifile_point).eq.' ') then
                go to 45
             endif
             ifile_point = ifile_point + 1
             go to 44
  45      continue
          ifile_point = ifile_point - 1
cLTOct2007          phi_1_doc_file = file (1: ifile_point) // '.doc'
          phi_1_file = file (1: ifile_point) // '.asc'

c     read phi 2
        read (2, '(a)') file
          ifile_point = 1
  46      continue
             if (file (ifile_point: ifile_point).eq.' ') then
                go to 47
             endif
             ifile_point = ifile_point + 1
             go to 46
  47      continue
          ifile_point = ifile_point - 1
cLTOct2007          phi_2_doc_file = file (1: ifile_point) // '.doc'
          phi_2_file = file (1: ifile_point) // '.asc'

c     read phi 3
        read (2, '(a)') file
          ifile_point = 1
  48      continue
             if (file (ifile_point: ifile_point).eq.' ') then
                go to 49
             endif
             ifile_point = ifile_point + 1
             go to 48
  49      continue
          ifile_point = ifile_point - 1
cLTOct2007          phi_3_doc_file = file (1: ifile_point) // '.doc'
          phi_3_file = file (1: ifile_point) // '.asc'

c     read phi 4
        read (2, '(a)') file
          ifile_point = 1
  50      continue
             if (file (ifile_point: ifile_point).eq.' ') then
                go to 51
             endif
             ifile_point = ifile_point + 1
             go to 50
  51      continue
          ifile_point = ifile_point - 1
cLTOct2007          phi_4_doc_file = file (1: ifile_point) // '.doc'
          phi_4_file = file (1: ifile_point) // '.asc'

c     read phi 5
        read (2, '(a)') file
          ifile_point = 1
  52      continue
             if (file (ifile_point: ifile_point).eq.' ') then
                go to 53
             endif
             ifile_point = ifile_point + 1
             go to 52
  53      continue
          ifile_point = ifile_point - 1
cLTOct2007          phi_5_doc_file = file (1: ifile_point) // '.doc'
          phi_5_file = file (1: ifile_point) // '.asc'

c     read phi 6
        read (2, '(a)') file
          ifile_point = 1
  54      continue
             if (file (ifile_point: ifile_point).eq.' ') then
                go to 55
             endif
             ifile_point = ifile_point + 1
             go to 54
  55      continue
          ifile_point = ifile_point - 1
cLTOct2007          phi_6_doc_file = file (1: ifile_point) // '.doc'
          phi_6_file = file (1: ifile_point) // '.asc'

c         read (2, *) (ps_init_ave (phi), phi = 1, 6)
          read (2, *) (ps_init_sd (phi), phi = 1, 6)
cEva  read in nutrient parameter CS (ammonium, nitrate, phosphorus):
        read (2, *) (Cs (i), i = 1, 3)
cEva  read in mass transfer coefficients(ammonium, nitrate, phosphorus):
        read (2, *) (alpha (i), i = 1, 3)
c
cJWFeb05  Read in detachment parameters
c
        read (2, *) (spa (i), i = 1, 6)
        read (2, *) (spb (i), i = 1, 6)
        read (2, *) (spc (i), i = 1, 6)
        read (2, *) (hs (i), i = 1, 6)
c
cJWNov2005   Read in splash distance parameter
c
        read (2, *) (spd (i), i = 1, 6)
c
cLTOct2007  Read in concentration of particulate-bound nutrient for each phi class.
c
          read (2, *) (p_ammonium (i), i = 1, 6)
          read (2, *) (p_nitrate (i), i = 1, 6)
          read (2, *) (p_TN (i), i = 1, 6)
          read (2, *) (p_TP (i), i = 1, 6)
          read (2, *) (p_IC (i), i = 1, 6)
          read (2, *) (p_TC (i), i = 1, 6)
          read (2, *) (Rn (i), i = 1, 3)
          close (2)
       endif


c      determine length of path name for output files
      DO i=1,sizeof(pfadin)
        IF (pfadin(i:i) == ' ') THEN
          pfadi=i-1
          EXIT
        END IF
      END DO

c      determine length of basic model path name
      DO i=1,sizeof(pfadout)
        IF (pfadout(i:i) == ' ') THEN
          pfado=i-1
          EXIT
        END IF
      END DO


       call echo_params (isix)

       return
       end
