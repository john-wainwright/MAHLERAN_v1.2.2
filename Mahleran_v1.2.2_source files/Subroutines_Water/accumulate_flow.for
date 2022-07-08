c****************************************************************
c  subroutine to define water flow accumulation
c****************************************************************
       subroutine accumulate_flow

       use shared_data
       implicit double precision (a - h, o - z)
       implicit integer (i - n)

c     Eva: subroutine also defines the nutrient concentration
c     Eva: sum of concentrations of nutrients in upstream, adjacent cells
       double precision amm_up(4), nitr_up(4), phos_up(4)

cEva  add_soil(phi,i,j): amount of sediment discharge entering cell [mm2/s]

c
c  VARIABLES:
c
c      ndirn = 4 routes flow to 'N', 'E', 'S', 'W'
c            = 8 splits 'NE', 'SE', 'SW', 'NW' flows to adjacent cells,
c              proportionally according to the local slopes
c      add (i, j) = amount of discharge entering cell in timestep (mm2/s)
c      aspect (i, j) = direction of maximum slope
c
c   initialize add and dup
cLTOct2007 This is setting everything to 0 for the first iteration.
cLTOct2007 addammonium, addnitrate, addphosphorus is the average concentration of nutrient in runoff
c   entering the cell from adjacent cells
c
c
       do i = 2, nr
          do j = 2, nc
             add (i, j) = 0.
             dup (i, j) = 0.
           addammonium (i, j) = 0.
           addnitrate (i, j) = 0.
           addphosphorus (i, j) = 0.
           do phi = 1, 6
                add_soil (phi, i, j) = 0.
           enddo
          enddo
       enddo
c
c   route along four directions
c
       if (ndirn.eq.4) then
          do i = 2, nr
             i1 = i - 1
             i2 = i + 1
             do j = 2, nc
                j1 = j - 1
                j2 = j + 1

c     Eva: initialize incoming nutrient concentration from surrounding cells
              do m = 1, 4
                 amm_up (m) = 0.
                 nitr_up (m) = 0.
                 phos_up (m) = 0.
              enddo
c
c 'N'
c
                if (aspect (i2, j).eq.1) then
                   add (i, j) = add (i, j) + q (1, i2, j)
                 dup (i, j) = dup (i, j) + d (1, i2, j)
                 amm_up (1) = ammonium (1, i2, j)
                 nitr_up (1) = nitrate (1, i2, j)
                 phos_up (1) = phosphorus (1, i2, j)
                 do phi = 1, 6
                    add_soil (phi, i, j) = add_soil (phi, i, j)
     &                                     + q_soil (phi, 1, i2, j)
                 enddo

                endif
c
c 'E
c
                if (aspect (i, j1).eq.2) then
                   add (i, j) = add (i, j) + q (1, i, j1)
                   dup (i, j) = dup (i, j) + d (1, i, j1)
                 amm_up (2) = ammonium (1, i, j1)
                 nitr_up (2) = nitrate (1, i, j1)
                 phos_up (2) = phosphorus (1, i, j1)
                 do phi = 1, 6
                    add_soil (phi,i, j) = add_soil (phi,i, j)
     &                                  + q_soil (phi, 1, i, j1)
                 enddo
                endif
c
c 'S
c
                if (aspect (i1, j).eq.3) then
                   add (i, j) = add (i, j) + q (1, i1, j)
                   dup (i, j) = dup (i, j) + d (1, i1, j)
                 amm_up (3) = ammonium (1, i1, j)
                 nitr_up (3) = nitrate (1, i1, j)
                 phos_up (3) = phosphorus (1, i1, j)
                 do phi = 1, 6
                    add_soil (phi,i, j) = add_soil (phi, i, j)
     &                                  + q_soil (phi, 1, i1, j)
                 enddo
                endif
c
c 'W'
c
                if (aspect (i, j2).eq.4) then
                   add (i, j) = add (i, j) + q (1, i, j2)
                   dup (i, j) = dup (i, j) + d (1, i, j2)
                 amm_up (4) = ammonium (1, i, j2)
                 nitr_up (4) = nitrate (1, i, j2)
                 phos_up (4) = phosphorus (1, i, j2)
                 do phi = 1, 6
                    add_soil (phi,i, j) = add_soil (phi, i, j)
     &                                  + q_soil (phi, 1, i, j2)
                 enddo
                endif
c
c     Eva: routing of nutrient concentration: calculation of average concentration
c     in the incoming flow on the basis of the flows and concentrations of surrounding
c     cells
c     Calculate addammonium(i,j) only if add(i,j) is larger than zero
                if (add(i,j).gt.0.0d0) then
                   addammonium (i, j) = (amm_up (1) * q (1, i2, j) +
     &                                  amm_up (2) * q (1, i, j1) +
     &                                amm_up (3) * q (1, i1, j) +
     &                                  amm_up (4) * q (1, i, j2))
     &                                / add (i,j)

                   addnitrate (i, j) = (nitr_up (1) * q (1, i2, j) +
     &                                 nitr_up (2) * q (1, i, j1) +
     &                               nitr_up (3) * q (1, i1, j) +
     &                                 nitr_up (4) * q (1, i, j2))
     &                               / add (i,j)

                   addphosphorus (i, j) = (phos_up (1) * q (1, i2, j) +
     &                                    phos_up (2) * q (1, i, j1) +
     &                                  phos_up (3) * q (1, i1, j) +
     &                                    phos_up (4) * q (1, i, j2))
     &                                  / add (i,j)
                endif

             enddo
          enddo
       endif

       return
       end
