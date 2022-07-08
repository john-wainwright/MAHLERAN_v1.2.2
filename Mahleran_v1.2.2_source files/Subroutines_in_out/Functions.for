c****************************************************************
c modified function from NUMERICAL RECIPES to return random number 
c    from normal distribution
c****************************************************************
       double precision function rand_from_norm (ave, stddev, idum)

       implicit double precision (a - h, o - z)
       implicit integer*4 (i - n)

       double precision fac, gset, rsq, v1, v2, ran1, gasdev
       double precision ave, stddev

       integer *4 idum, iset


       save iset, gset

       data iset / 0 /


       if (iset.eq.0) then
   1      continue
          v1 = 2. * ran1 (idum) - 1.
          v2 = 2. * ran1 (idum) - 1.
          rsq = v1 ** 2 + v2 ** 2
          if (rsq.ge.1..or.rsq.eq.0.) then
             go to 1
          endif
          fac = sqrt (-2. * log (rsq) / rsq)
          gset = v1 * fac
          gasdev = v2 * fac
          iset = 1
       else
          gasdev = gset
          iset = 0
       endif

       rand_from_norm = ave + (gasdev * stddev)


       return
       end

c****************************************************************
c  function from NUMERICAL RECIPES to return random number from 
c     uniform distribution [0, 1]
c****************************************************************
       double precision function ran1 (idum)

       implicit double precision (a - h, o - z)
       integer *4  idum, ia, im, iq, ir, ntab, ndiv
       double precision am, eps, rnmx
c
c   minimal random number generator. Returns a uniform random
c   deviate between 0.0 and 1.0 (exclusive of the endpoint
c   values).
c   Call with idum a negative integer to initialize. Thereafter
c   do not alter idum between successive deviates in a sequence.
c   rnmx should approximate the largest floating point value 
c   that is less than 1.
c
       parameter (ia = 16807, im = 2147483647, am = 1. / im,
     &    iq = 127773, ir = 2836, ntab = 32,
     &    ndiv = 1 + (im - 1) / ntab, eps = 1.2e-7,
     &    rnmx = 1. - eps)
       integer *4 j, k, iv (ntab), iy
       save iv, iy
       data iv /ntab * 0/, iy /0/
 
       if (idum.le.0.or.iy.eq.0) then
          idum = max (-idum, 1)
          do j = ntab + 8, 1, -1
             k = idum / iq
             idum = ia * (idum - k * iq) - ir * k
             if (idum.lt.0) then
                idum = idum + im
             endif
             if (j.le.ntab) then
                iv (j) = idum
             endif
          enddo
          iy = iv (1)
       endif
       k = idum / iq
       idum = ia * (idum - k * iq) - ir * k
       if (idum.lt.0.) then
          idum = idum + im
       endif
       j = 1 + iy / ndiv
       iy = iv (j)
       iv (j) = idum
       ran1 = min (am * iy, rnmx)

       return
       end

c ****************************************************************
c   functions for converting text to times and values for rainfall
c   variability runs                                              
c   num_conv - converts integer from text to value (needed for
c                                              time_conv function)
c   num_conv1 - converts real number from text to value
c   inum_conv - identical to num_conv but returns integer rather than d.p. value       
c ****************************************************************
       double precision function num_conv (a)

       implicit double precision (a-h, o-z)
       implicit integer *4 (i-n)

       character *(*) a
       character *1 a1

       iunit = 0
       num_conv = 0.

       do j = len (a), 1, -1
          a1 = a (j:j)
          if (a1.ge.'0'.and.a1.le.'9') then
             num_conv = num_conv + ((ichar (a1) - 48) * 10 ** iunit)
             iunit = iunit + 1
          endif
       enddo

       return
       end

       integer function inum_conv (a)

       implicit double precision (a-h, o-z)
       implicit integer *4 (i-n)

       character *(*) a
       character *1 a1

       iunit = 0
       inum_conv = 0

       do j = len (a), 1, -1
          a1 = a (j:j)
          if (a1.ge.'0'.and.a1.le.'9') then
             inum_conv = inum_conv + ((ichar (a1) - 48) * 10 ** iunit)
             iunit = iunit + 1
          endif
       enddo

       return
       end


       double precision function num_conv1 (a)

       implicit double precision (a-h, o-z)
       implicit integer *4 (i-n)

       logical point

       character *(*) a
       character *1 a1

       point = .FALSE.
       num_conv1 = 0.

       do j = 1, len (a)
          a1 = a (j:j)
          if (a1.eq.'.') then
             point = .TRUE.
             iunit = 0
          elseif (point.and.a1.ge.'0'.and.a1.le.'9') then
             iunit = iunit - 1
          endif
       enddo
       if (.not.point) then
          iunit = 0
       endif   

       do j = len (a), 1, -1
          a1 = a (j:j)
          if (a1.ge.'0'.and.a1.le.'9') then
             num_conv1 = num_conv1 + ((ichar (a1) - 48) * 10. ** iunit)
             iunit = iunit + 1
          endif
       enddo

       return
       end

c **********************************************************************
c  function to convert time in format hh:mm:ss.ss into number of seconds
c **********************************************************************
       double precision function time_conv (a)

       implicit double precision (a-h, o-z)
       implicit integer *4 (i-n)

       double precision num_conv, num_conv1

       character *11 a

       time_conv = (num_conv (a (1:2)) * 3600.) +
     &             (num_conv (a (4:5)) * 60.) + 
     &              num_conv1 (a (7:11))

       return
       end

c
c   sorts an array arr (1:n) into ascending numerical order using the
c     Quicksort algorithm. n is input; arr is replaced on output by its
c     sorted rearrangement.
c   Parameters: m is the size of subarrays sorted by straight insertion
c     and nstack is the required auxillary storage
c
c   Derived from Numerical Recipes routine
c

       subroutine sort (n, arr)

       implicit double precision (a - h, o - z)

       integer *4 n, m, nstack
       parameter (m = 7, nstack = 50)
c      , ncell = 250000)
       integer *4 arr (n, 3)

       integer *4 istack (nstack)
       integer *4 i, ir, j, jstack, k, l, j1, l1
       double precision a, temp
       integer *4 a1x, a1y, tempx, tempy

       jstack = 0
       l = 1
       ir = n

   1   continue

       if (ir - l.lt.m) then
          l1 = l + 1
          do 12 j = l1, ir
             a = arr (j, 3)
             a1x = arr (j, 1)
             a1y = arr (j, 2)
             j1 = j - 1
             do 11 i = j1, 1, -1
                if (arr (i, 3).le.a) then
                   go to 2
                endif
                arr (i + 1, 3) = arr (i, 3)
                arr (i + 1, 1) = arr (i, 1)
                arr (i + 1, 2) = arr (i, 2)
  11         continue
             i = 0
   2         continue
             arr (i + 1, 3) = a
             arr (i + 1, 1) = a1x
             arr (i + 1, 2) = a1y
  12      continue
          if (jstack.eq.0) then
             return
          endif
c
c   pop stack and begin a new round of partitioning
c
          ir = istack (jstack)
          l = istack (jstack - 1)
          jstack = jstack - 2
       else
c
c   choose median of left, centre, and right elements as partitioning
c      element a. Also rearrange so that a(l+1)<=a(l)<=a(ir)
c
          k = (l + ir) / 2
          temp = arr (k, 3)
          tempx = arr (k, 1)
          tempy = arr (k, 2)
          arr (k, 3) = arr (l + 1, 3)
          arr (k, 1) = arr (l + 1, 1)
          arr (k, 2) = arr (l + 1, 2)
          arr (l + 1, 3) = temp
          arr (l + 1, 1) = tempx
          arr (l + 1, 2) = tempy
          if (arr (l + 1, 3).gt.arr (ir, 3)) then
             temp = arr (l + 1, 3)
             tempx = arr (l + 1, 1)
             tempy = arr (l + 1, 2)
             arr (l + 1, 3) = arr (ir, 3)
             arr (l + 1, 1) = arr (ir, 1)
             arr (l + 1, 2) = arr (ir, 2)
             arr (ir, 3) = temp
             arr (ir, 1) = tempx
             arr (ir, 2) = tempy
          endif
          if (arr (l, 3).gt.arr (ir, 3)) then
             temp = arr (l, 3)
             tempx = arr (l, 1)
             tempy = arr (l, 2)
             arr (l, 3) = arr (ir, 3)
             arr (l, 1) = arr (ir, 1)
             arr (l, 2) = arr (ir, 2)
             arr (ir, 3) = temp
             arr (ir, 1) = tempx
             arr (ir, 2) = tempy
          endif
          if (arr (l + 1, 3).gt.arr (l, 3)) then
             temp = arr (l + 1, 3)
             tempx = arr (l + 1, 1)
             tempy = arr (l + 1, 2)
             arr (l + 1, 3) = arr (l, 3)
             arr (l + 1, 1) = arr (l, 1)
             arr (l + 1, 2) = arr (l, 2)
             arr (l, 3) = temp
             arr (l, 1) = tempx
             arr (l, 2) = tempy
          endif
c
c   initialize pointers for partitioning
c
          i = l + 1
          j = ir
c
c   partitioning element
c
          a = arr (l, 3)
          a1x = arr (l, 1)
          a1y = arr (l, 2)
c
c   beginning of innermost loop
c
   3      continue
c
c   scan up to find element > a
             i = i + 1
             if (arr (i, 3).lt.a) then
                go to 3
             endif
   4         continue
c
c   scan down to find element < a
c
                j = j - 1
                    if (arr (j, 3).gt.a) then
                       go to 4
                    endif
                    if (j.lt.i) then
c
c   pointers crossed - exit with partitioning complete
c
                       go to 5
                    endif
c
c   exchange elements
c
                    temp = arr (i, 3)
                    tempx = arr (i, 1)
                    tempy = arr (i, 2)
                    arr (i, 3) = arr (j, 3)
                    arr (i, 1) = arr (j, 1)
                    arr (i, 2) = arr (j, 2)
                    arr (j, 3) = temp
                    arr (j, 1) = tempx
                    arr (j, 2) = tempy
c
c   end of innermost loop
c
                 go to 3
c
c   insert partitioning element
c
   5      continue
          arr (l, 3) = arr (j, 3)
          arr (l, 1) = arr (j, 1)
          arr (l, 2) = arr (j, 2)
          arr (j, 3) = a
          arr (j, 1) = a1x
          arr (j, 2) = a1y
          jstack = jstack + 2
c
c   push pointers to larger subarray on stack, process smaller subarray
c     immediately
c
          if (jstack.gt.nstack) then
             write (6, *) 'nstack too small in sort'
             stop
          endif
          if (ir - i + 1.ge.j - 1) then
             istack (jstack) = ir
             istack (jstack - 1) = i
             ir = j - 1
          else
             istack (jstack) = j - 1
             istack (jstack - 1) = l
             l = i
          endif
       endif
       go to 1

       end