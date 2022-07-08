c****************************************************************
c  subroutine to read initial marker x,y coordinates
c
c  JC Dec 2011
c
c****************************************************************

       subroutine read_markers

       use shared_data
       implicit double precision (a - h, o - z)
       implicit integer (i - n)

       character *14 a_mnum
         character *100 temp_file


         temp_file = pfadin(1:pfadi) // markerfile
c      Open the *.asc file
       open (3, FILE = temp_file, status = 'unknown')
       rewind (3)

c      Read header of file
       read (3, 9999) a_mnum, mnum
20     continue

c      Allocate size of arrays
       include 'allocat_markers.var'
       call initialise_markers

c      Read in data
       do i = 1, mnum
          read (3, *) (MXY (i, k), k = 1, 3)
       enddo
       close (3)

       write (6, *)
       write (6, *) ' Number of markers read in '

9999   format (a6, i10)

       return
       end

