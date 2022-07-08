
c*******************************************************************
c  subroutine to calculate transport by unconcentrated overland flow
c*******************************************************************
       subroutine diffuse_flow_transport

       use shared_data
       implicit double precision (a - h, o - z)
       implicit integer (i - n)

       double precision travel_median, red_par

c
c   if there is surface water present, distribute sediment by flow
c
       if (slope (im, jm).eq.0.) then
c
c   move nothing if there is no surface slope
c
          sed_temp (phi, im, jm) = sed_temp (phi, im, jm) +
     &                             detach_soil (phi, im, jm)
       else
c
cEva  Kinetic energy in[J/ (m2 * per timestep dt)]
c
          ke = (11.9d0 + 8.73d0 * log10 (r2 (im, jm) * 3.6d3)) *
     &         r2 (im, jm)
c
cEva  Flow energy in [J/(m2 * s)]
c
          flow_energy = 9.81d-3 * d (1, im, jm) * v (im, jm) *
     &                  slope (im, jm)
c
cEva  Virtual velocity in cm/min
c   Median virtual velocity based on Parsons et al. ESPL 23, 365-375.
c      Md = 0.525 * ke ** 2.35 * flow_energy ** 0.981 * particle_mass ** -1
cJWFeb05     radius (max (phi, 3)) stops virtual velocities from becoming too
cJWFeb05        high for smallest particle sizes
cJWFeb05
          p_mass = (density * 1.d6 * fourth * pi *
     &              radius (phi) ** 3)
cJWJan2006  modified version of equation should be able to deal with fine grain sizes
c     &              radius (max (phi, 3)) ** 3)
          v_soil (phi, im, jm) = 0.525 * ke ** 2.35 *
     &                           flow_energy ** 0.981 /
     &                         p_mass
c
cEva  Travel distance unit conversion from cm/min to mm/s
c
          v_soil (phi, im, jm) = v_soil (phi, im, jm) * 0.166666666667d0
c
cEva  Preliminary I set the travel distance to a constant 3 metres
c
c          tm_m = 3.0d0
c          tm_m = 0.25d0
c
cJWFeb05   below is reanalysis of original Parsons et al data giving
cJWFeb05     Mdist = 0.19943 * flow_energy ** 1.606514 * ke ** 1.445592 * part_mass^-0.96035
c
          tm_m = 5.0d-2 * ke ** 1.85d0 * flow_energy ** 0.481
     &           * p_mass ** (-0.425)
cJWFeb05        if (tm_m.gt.0.5d0) then
cJWFeb05           tm_m = 0.5d0
cJWFeb05        endif
c
c   limit the sediment virtual velocity from exceeding the flow velocity
c
        if (v_soil (phi, im, jm).gt.v (im, jm)) then
           v_soil (phi, im, jm) = v (im, jm)
        endif

          call flow_distrib (tm_m, nmax_diffuse_flow (phi))

       endif

       return
       end
