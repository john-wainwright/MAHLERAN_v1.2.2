
c******************************************************************
c  subroutine to calculate transport by concentrated overland flow
c******************************************************************
c
c  modified 11/9/2015 by JW to make particle density an explicit term in the Bagnold threshold term
c
       subroutine conc_flow_transport

       use shared_data
       implicit double precision (a - h, o - z)
       implicit integer (i - n)

       double precision bagnold_thresh, stream_power, xs_sp, travel_dist

c
c   calculate threshold stream power for initial moment based on Bagnold
c
c       bagnold_thresh = 290.0d0 * d50 ** 1.5d0 *
c     &                  log10 (12.d0 * d (1, im, jm) / d50)
c
c   revised version using rearrangement from Bagnold RA (1980) An Empirical Correlation of Bedload Transport
c             Rates in Flumes and Natural Rivers Proc. R. Soc. Lond. A 1980 372 453-473; DOI: 10.1098/rspa.1980.0122.
c      excess_density is calculated in initialize_values_xml as the difference between particle and water density in kg m^-3
c
       bagnold_thresh = 4.554d-3 * (excess_density * d50) ** 1.5d0 *
     &                  log10 (12.d0 * d (1, im, jm) / d50)

cDEC2013 bug from Peng -- should be depth not diameter     &                  log10 (12.d0 * diameter (phi) / d50)
       if (bagnold_thresh.lt.0.0d0) then
          write (6, 9998) iter, im, jm, phi, d50, diameter (phi),
     &                    bagnold_thresh
          write (6, *) ' --> threshold reset to zero '
          bagnold_thresh = 0.0d0
       endif
c
c   9.81d-3 below multiplies by gravity and converts velocity from mm/s to m/s
c   water density term is omitted as it is assumed it cancels with the
c   conversion term for depth from mm to m
cEva: unit of stream_power: Watt/m2 or kg*m2/(s3*m2)
c
       stream_power = 9.81d-3 * d (1, im, jm) * slope (im, jm) *
     &                v (im, jm)

       xs_sp = stream_power - bagnold_thresh
c
c   if no excess stream power, redeposit sediment and exit
c
       if (xs_sp.le.0.0d0) then
        depos_soil (phi, im, jm) = depos_soil (phi, im, jm) +
     &                             detach_soil (phi, im, jm)

          return
       endif
c
c   calculate mean travel distance from Hassan et al. 1991, Equation 2b, in [m]
c
       travel_dist = 2.85d-3 * xs_sp ** 1.31d0 *
     &               diameter (phi) ** (-0.94d0)
c
cEva: calculate median travel distance
c
       travel_dist = travel_dist * 0.693
       if (travel_dist.gt.30) then
!         write (6, 9999) iter, im, jm, phi, travel_dist, stream_power,
!     &                    xs_sp, bagnold_thresh
        travel_dist = 30.0d0
       endif
c
cEva  Calculate virtual rate of travel [m/h] using Equation 5 in Hassan et al. 1992
c
cJWMar2006   v_soil (phi, im, jm) = 2.56d-4 * xs_sp ** 2.02
cJWMar2006  use ordinary regression corresponding to Equation 5 in Hassan et al. 1992
       v_soil (phi, im, jm) = 1.92d-2 * xs_sp ** 1.01
c
cEva convert units from [m/h] into [mm/s]  (1000.d0 / 3600.d0 = 0.278)
c
       v_soil (phi, im, jm) = v_soil (phi, im, jm) * 2.777777777778d-1
       if (v_soil (phi, im, jm).gt.v (im, jm)) then
            v_soil (phi, im, jm) = v (im, jm)
       endif

       call flow_distrib (travel_dist, nmax_conc_flow (phi))

9998   format (' WARNING at iteration ', i5, ' cell: ', 2(i3, 1x),
     &         ' phi: ', i1, ' d50: ', f8.4, ' diameter(phi): ', f8.4,
     &         ' produces Bagnold threshold < 0 (= ', f8.4, ')')
9999   format (' iteration: ', i5, ' cell: ', 2(i3, 1x), ' phi: ', i1,
     &         ' travel distance: ', f8.4, ' stream power: ', f8.4,
     &         ' excess SP: ', f8.4, ' Bagnold threshold: ', f8.4)
       return
       end
