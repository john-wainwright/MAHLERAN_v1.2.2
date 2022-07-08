c****************************************************************
c  subroutine to calculate transport by suspension
c****************************************************************
c
cJW modified 11/9/2015 to use Bagnold's suggestion for scaling according to density effects
c   will be identical to previous version if particle density is 2650 kg/m3

c
       subroutine suspended_transport

       use shared_data
       implicit double precision (a - h, o - z)
       implicit integer (i - n)

       double precision stream_power, travel_dist
       double precision spf
c
c   9.81d-3 below multiplies by gravity and converts velocity from mm/s to m/s
c   water density term is omitted as it is assumed it cancels with the
c   conversion term for depth from mm to m
c
       stream_power = 9.81d-3 * d (1, im, jm) * slope (im, jm) *
     &                v (im, jm)
c
c   calculate travel distance based on empirical analysis of results from
c      using Anderson 1987 to estimate suspension trajectories in flows
c
       spf = 7.331976d-3 * stream_power
c
c   stop exp overflowing
c
       if (spf.gt.100.0d0) then
          spf = 100.0d0
       endif
c
cEva: diameter has to be in mm
cEva  travel_dist in[m]
c
       travel_dist = 727.51805244d0 * exp (spf)
     &               * exp (-6.12683698 * diameter (phi)*1000)
c
cEva  change mean travel_dist to median for the step length distribution functionc
c
       travel_dist = travel_dist * 0.693
c
c   revised version using rearrangement from Bagnold RA (1980) An Empirical Correlation of Bedload Transport
c             Rates in Flumes and Natural Rivers Proc. R. Soc. Lond. A 1980 372 453-473; DOI: 10.1098/rspa.1980.0122.
c        bagnold_density_scale is calculated in initialize_values_xml as (supported particle density / 1650) ^ -0.5
c
         travel_dist = travel_dist * bagnold_density_scale
c
cEva  assume that suspended particles travel with the same velocity as the water flow
c
       v_soil (phi, im, jm) = v (im, jm)

       call flow_distrib (travel_dist, nmax_susp_flow (phi))

       return
       end
