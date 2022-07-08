c****************************************************************
c  subroutine to define chemistry flow routing
c****************************************************************
       subroutine route_chemistry

       use shared_data
       implicit double precision (a - h, o - z)
       implicit integer (i - n)
c
c     alpha from Walton et al. 2000, Hydrological Processes 14, 1139 - 1158
c     ammonium (k,i,j) in mg/l      concentration in runoff
c     Cs in mg/l        concentration of ammonium in soil solution
c     alpha in mm/s   infiltration rate
c     depth in mm       surface water depth
c     q in mm2/s      flow per unit width
c
cLTOct2007 Think that 'add' is discharge added to cell
c
       do i = 2, nr
          do j = 2, nc
c
c   use low threshold of 0.1 mm to stop initial shocks of nutrient

cLTOct2007    below if depth of water in cell is less than 0.1mm, then nutrient concentration = zero??

             if (d (2, i, j).le.0.1d0) then
              ammonium (2, i, j) = 0.0d0
              nitrate (2, i, j) = 0.0d0
              phosphorus (2, i, j) = 0.0d0
             elseif (d (2, i, j).gt.0.0d0.and.rmask (i, j).gt.0.0d0)
     &          then
c               ammonium (2, i, j) = (d (1, i, j) / d (2, i, j)) *
c     &                               ammonium(1,i,j) +
c     &                            (dt / d (2, i, j)) *
c     &                               (alpha (1) * Cs (1) -            !ignored subtracting C
c     &                            ksat (i, j) * ammonium (1, i, j)  !LT ksat used here, assuming that time step is 1 second, when this should be amount infiltrated per time step
c     &                            + (1. / dx) * (add (i, j) *
c     &                               addammonium(i,j) -
c     &                               q (1, i, j) * ammonium (1, i, j)))
cc
cLTOct2007 to account for nutrient concentration in rainfall as in Havis et al, 1992 (WRR)
cLTOct2007 Rn (1) is rainfall nutrient concentration
cLTDec2007 Also changed Ksat to amount of water infiltrated (mm) per timestep (cum_inf_p)

                ammonium (2, i, j) = (- dt / (d (2, i, j) * dx))
     &                                * ((ammonium (1, i, j)
     &                                *  q (1, i, j)) - (add (i, j) *
     &                                addammonium (i, j))) +
     &                                (dt / d (2, i, j)) *
     &                                (alpha (1) * (Cs (1) -
     &                                ammonium (1, i, j)) -
     &                               (cum_inf(i,j)-cum_inf_p(i,j)) ! amount of water infiltrated per timestep
     &                    * ammonium (1, i, j)) +
     &                                (r2 (i, j) * Rn (1)) +
     &                                ammonium (1, i, j)
     &                                * (d (1, i, j) / d (2, i, j))


c               nitrate (2, i, j) = (d (1, i, j) / d (2, i, j)) *
c     &                              nitrate(1,i,j) + (dt/d(2,i,j)) *
c     &                              (alpha (2) * Cs (2) - ksat (i, j) *
c     &                              nitrate (1, i, j) + (1. / dx) *
c     &                              (add (i, j) * addnitrate (i, j) -
c     &                              q (1, i, j) * nitrate (1, i, j)))

                nitrate (2, i, j) = (- dt / (d (2, i, j) * dx))
     &                                * ((nitrate (1, i, j)
     &                                *  q (1, i, j)) - (add (i, j) *
     &                                addnitrate (i, j))) +
     &                                (dt / d (2, i, j)) *
     &                                (alpha (2) * (Cs (2) -
     &                                nitrate (1, i, j)) -
     &                               (cum_inf(i,j)-cum_inf_p(i,j))
     &                    * nitrate (1, i, j)) +
     &                                (r2 (i, j) * Rn (2)) +
     &                                nitrate (1, i, j)
     &                                * (d (1, i, j) / d (2, i, j))



c               phosphorus (2, i, j) = (d (1, i, j) / d (2, i, j)) *
c     &                                 phosphorus (1, i, j) +
c     &                                 (dt / d (2, i, j)) *
c     &                                 (alpha (3) * Cs (3) - ksat(i,j) *
c     &                                phosphorus (1, i, j) + (1. / dx) *
c     &                                (add (i, j) * addphosphorus (i, j)
c     &                                 - q (1, i, j) *
c     &                                 phosphorus (1, i, j)))

                phosphorus (2, i, j) = (- dt / (d (2, i, j) * dx))
     &                                * ((phosphorus (1, i, j)
     &                                *  q (1, i, j)) - (add (i, j) *
     &                                addphosphorus (i, j))) +
     &                                (dt / d (2, i, j)) *
     &                                (alpha (3) * (Cs (3) -
     &                                phosphorus (1, i, j)) -
     &                               (cum_inf(i,j)-cum_inf_p(i,j))
     &                    * phosphorus (1, i, j)) +
     &                                (r2 (i, j) * Rn (3)) +
     &                                phosphorus (1, i, j)
     &                                * (d (1, i, j) / d (2, i, j))

                if (ammonium (2, i, j).lt.0.0d0) then
                 ammonium (2, i, j) = 0.0d0
                endif
                if (nitrate (2, i, j).lt.0.0d0) then
                 nitrate (2, i, j) = 0.0d0
                endif
                if (phosphorus (2, i, j).lt.0.0d0) then
                 phosphorus (2, i, j) = 0.0d0
                endif
c below are the fluxes
                y_amm (i, j) = y_amm (i, j) + ammonium (2, i, j) *
     &                         q (2, i, j) * dx * 1.e-6 * dt
                y_nit (i, j) = y_nit (i, j) + nitrate (2, i, j) *
     &                         q (2, i, j) * dx * 1.e-6 * dt
                y_phos (i, j) = y_phos (i, j) + phosphorus (2, i, j) *
     &                          q (2, i, j) * dx * 1.e-6 * dt

             endif
          enddo
       enddo
c
cj  following output section modified and moved so occurs with main plot hydrograph
cj     and sedigraph outputs
cj
c     Output SW nutrient concentration: first in [mg/l], then in [mg/s]
c       write (60, '(7(e10.4,1x))') iter * dt/60, ammonium(2,63,22),
c     & nitrate(2,63,22), phosphorus(2,63,22),
c     & ammonium(2,63,22) * q(2,63,22) * dx*1.e-6,
c     &    nitrate(2,63,22) * q(2,63,22) * dx*1.e-6,
c     & phosphorus(2,63,22) * q(2,63,22) * dx*1.e-6


       return
       end
