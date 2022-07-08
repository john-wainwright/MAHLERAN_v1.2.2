c****************************************************************
c  subroutine to estimate the probability that a marker will be
c  detached in transitional flow conditions in which both
c  raindrop and flow detachment occur
c
c  JC May 2011
c
c****************************************************************

       subroutine trans_detach_markers

       use shared_data
       implicit double precision (a - h, o - z)
       implicit integer (i - n)

       double precision detach_propn_m

c      Reset detach_tot_m
       do im = 1, nr1
        do jm = 1, nc1
            detach_tot_m (im, jm) = 0.0d0
        enddo
       enddo

c      First use the output from the cellular part of MAHLERAN
c      These lines could be added to the bottom of splash_transport
c      Determine mass of splashed and flow detached soil in all cells for each phi class (kg)
       do k = 1, 6
        do im = 1, nr1
            do jm = 1, nc1
c               Convert detach_soil (mm/s) from output in route_sediment to give total detached soil (kg)
c               First divide detach_soil by 1000 to convert from mm to m
                detach_soil_m (k, im, jm) = detach_soil
     &              (k, im, jm) / 1.0d3
c               Then convert into a density (kg/m3)
                detach_soil_m (k, im, jm) = detach_soil_m
     &              (k, im, jm) * (dx_m ** 2)
c               Convert into flux (kg/s) - density * 1.0d3 is needed to convert g/cm3 to kg/m3
                detach_soil_m (k, im, jm) = detach_soil_m
     &              (k, im, jm) * (density * 1.0d3)
c               Finally convert into mass (kg)
                detach_soil_m (k, im, jm) = detach_soil_m
     &              (k, im, jm) * dt
c               Determine total mass of splashed soil in all cells (kg)
                detach_tot_m (im, jm) = detach_tot_m (im, jm) +
     &              detach_soil_m (k, im, jm)
            enddo
        enddo
       enddo

c      Determine proportion by mass of splashed soil
       detach_propn_m = detach_soil_m (INT (MXY (mi, 3)),
     &                mycell, mxcell) / detach_tot_m (mycell, mxcell)

c      Estimate probability of marker being detached. See Wright (1987) Equation 1
c      Available soil depth for detachment assumed to be equal to one grain diameter
       if (detach_soil_m (INT (MXY (mi, 3)), mycell, mxcell)
     &    .eq.0.0d0) then
        p_trans = 0.0d0
       else
          p_trans = (detach_tot_m (mycell, mxcell) / ((density * 1.0d3)
     &            * (dx_m ** 2) * diameter (INT (MXY (mi, 3))) )) *
     &            (detach_propn_m / sed_propn (INT (MXY (mi, 3)),
     &              mycell, mxcell))
       endif

       return
       end
