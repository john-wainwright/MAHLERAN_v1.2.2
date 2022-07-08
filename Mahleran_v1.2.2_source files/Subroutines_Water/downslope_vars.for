c****************************************************************
c  subroutine to define downslope flow variables
c****************************************************************
       subroutine downslope_vars (qdown, ddown, i, j)

       use shared_data
       implicit double precision (a - h, o - z)
       implicit integer (i - n)


       integer idi (0:8), idj (0:8)

       data idi / 0, -1, 0, 1, 0, -1, 1, 1, -1 /
       data idj / 0, 0, 1, 0, -1, 1, 1, -1, -1 /

       iasp = aspect (i, j)
c
c   check for zero routing, and adjust for flow depth
cbq  According to the following do loop, when the processig cell has
cbq  a flow direction value of zero, flow from the processing cell goes to
cbq  the first cell in the loop whose flow depth is less than the flow depth of
cbq  the processing cell.  Shouldn't the flow go to the neghbouring cell whose
cbq  elevation plus flow depth is less than that of the processing cell?
c
       if (iasp.eq.0) then
          do k = 1, ndirn
             if (d (1, i + idi (k), j + idj (k)).gt.
     &           d (1, i + idi (iasp), j + idj (iasp))) then
                iasp = k
             endif
          enddo
       endif
c
c   calculate downslope q and d for four and eight flow direction cases
cbq  I have cheked the four flow directions, but I am not sure about the
cbq  the eight flow directions.  Need to discuss this with JW.
c
       if (iasp.le.4) then
          qdown = q (1, i + idi (iasp), j + idj (iasp))
          ddown = d (1, i + idi (iasp), j + idj (iasp))
       else

cbq  I can't follow the logic used to calculate qdown and ddown for eight flow direction cases
          iasp1 = mod (iasp, 8)
          iasp2 = mod (iasp + 1, 8)
          if (iasp2.eq.0) then
             iasp2 = 1
          endif
          qdown = 0.5 * (q (1, i + idi (iasp1), j + idj (iasp1)) +
     &                  q (1, i + idi (iasp2), j + idj (iasp2)))
          ddown = 0.5 * (d (1, i + idi (iasp1), j + idj (iasp1)) +
     &                  d (1, i + idi (iasp2), j + idj (iasp2)))
       endif
       return
       end
