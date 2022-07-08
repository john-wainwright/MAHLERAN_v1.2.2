c ************************************************************
c  subroutine to calculate contributing areas to each cell
c ************************************************************
       subroutine contrib_area (sdir)
       use shared_data
       implicit double precision (a - h, o - z)
       implicit integer (i - n)
c
       integer *4 sdir (4, 2)
c      sdir() contains four vectors in the N[-1,0], E[0,1],
c      S[1,0] and W[0,-1] directions
c
c       write(6, *) 'sdir'
c       do n = 1, 4
c           write (6, 9990) ( sdir( n, j), j = 1, 2 )
c       enddo
c      
c      initialize array
       do i = 1, nr1
          do k = 1, nc1
             contrib (i, k) = 0
          enddo
       enddo
c
c      Calculate the number of cells contributing to each cell
c
       mincontrib = ncell
       maxcontrib = 0
       do i = 2, nr
          do k = 2, nc
             inow = i
             know = k
             ir2 = aspect (inow, know)
             contrib (inow, know) = contrib (inow, know) + 1
             
cCJMH        do while loop to determine to which of the four cardinal 
cCJMH        neighbouring cells the processing cell contributes and increments
cCJMH        the contributing area of that cell by one.
             
             do while (aspect (inow, know).ne.0)
                ir1 = aspect (inow, know)
                inow = inow + sdir (ir1, 1)
                know = know + sdir (ir1, 2)
                
cb              The following if .. then block checks whether or not the cell 
cb              is out of the boundary of interest.
                if (inow.le.1.or.
     &              inow.ge.nr1.or.know.le.1.or.know.ge.nc1) then
                   exit
                endif
                if (abs (ir1 - ir2).eq.2) then
c                   If ir1 and ir2 are in opposing directions, their 
c                   difference will = 2.
c                   Check for this condition to avoid infinite loop
c
                    exit
cb                  in other words, if two neighbouring cells have flow 
cb                  directions towards each other then exit to avoid infinite 
cb                  loop.
cbq                 How can the above problem be created, if the DEM is 
cbq                 depressionless?
                endif
                contrib (inow, know) = contrib (inow, know) + 1
                ir2 = ir1
             enddo
             if (z (i, k).le.-9999) then
	          contrib (i, k) = 0
             endif
          enddo
       enddo
c
9999   format ('ncols ', i6)
9998   format ('nrows ', i6)
9997   format ('xllcorner ', f15.8)
9996   format ('yllcorner ', f15.8)
9995   format ('cellsize ', f15.8)
9994   format ('nodata_value -9999') 
9993   format (10000 (i6, 1x))
9992   format (10000 (a1, 1x))
9991   format (10000 (e10.4, 1x))
9990   format ( '[',I2,',',I2,']' )

       return
       end