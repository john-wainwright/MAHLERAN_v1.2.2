
c****************************************************************
c  subroutine to calculate detachment by flow
c****************************************************************
       subroutine flow_detachment

       use shared_data
       implicit double precision (a - h, o - z)
       implicit integer (i - n)

       double precision dimless_shear, p_const, p_pickup, tptable
c
cJWFeb05   Calculates flow detachment for phi size classes at current cell as
cJWFeb05     defined by im, jm
cJWFeb05   Detachment rate is in mm/s
c

       do phi = 1, 6
          dimless_shear = ustar ** 2 / (sigma * 9.81d0 * diameter (phi))
          p_const = log (0.049d0 / (dimless_shear * 0.25d0))
          p_pickup = 0.5d0 - (0.5d0 * (p_const / abs (p_const)) *
     &            sqrt (1.d0 - exp (p_par * (p_const / 0.702d0) ** 2)))
cJWFeb05
cJWFeb05  need to have temporal scaling in maximum detachment rate so / dt added
cJWFeb05   so units of detach_soil and tptable balance
cJWFeb05
          tptable = sed_propn (phi, im, jm) * hs (phi) / dt
c
cJWFeb05  as hz is in mm, p_pickup and sed_propn are dimensionless, need to
cJWFeb05  divide by dt (rather than multiply) to produce units of mm/s
cJWFeb05
cJWFeb05        detach_soil (phi, im, jm) = p_pickup * hz * dt *
cJWFeb05     &                                sed_propn (phi, im, jm)
cJWJan06 CHECK should hz be function of stream power, as in scour measurements??
          detach_soil (phi, im, jm) = p_pickup * hz *
     &                                sed_propn (phi, im, jm) / dt

          if (detach_soil (phi, im, jm).gt.tptable) then
             detach_soil (phi, im, jm) = tptable
          endif
          if (sed_propn (phi, im, jm).eq.0.0) then
           detach_soil (phi, im, jm) = 0.0
          endif

          sed_temp (phi, im, jm) = tptable - detach_soil (phi, im, jm)
          flow_detach_tot (im, jm) = flow_detach_tot (im, jm) +
     &                               detach_soil (phi, im, jm)
       enddo

       return
       end
