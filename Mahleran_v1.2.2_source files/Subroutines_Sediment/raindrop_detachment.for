
c****************************************************************
c  subroutine to calculate detachment by raindrops
c****************************************************************
       subroutine raindrop_detachment

       use shared_data
       use parameters_from_xml

       implicit double precision (a - h, o - z)
       implicit integer (i - n)

       double precision tptable
c
cJWFeb05   Calculates raindrop detachment for phi size classes at current cell as
cJWFeb05     defined by im, jm
cJWFeb05   Detachment rate is in mm/s
c
c   Kinetic energy
c      -- 3600 converts from mm/s to mm/h rainfall
c      -- term on second line reduces energy according to vegetation cover
c         based on Wainwright et al. Journal of Arid Environments 43, 111-20.
c
       if (veg (im, jm).ge.0.0d0) then
           if (KE_model_type.eq.1) then
               ke = (11.9d0 + 8.73d0 * log10 (r2 (im, jm) * 3.6d3)) *
     &              (1.0d0 - 8.1d-3 * veg (im, jm))
           elseif (KE_model_type.eq.2) then
c
c   JW equation if Verstraeten et al is KE=0.29*(1-0.72*EXP(-0.05*rain_intensity))
c   terms multiplied through to avoid additional calculations
c
               ke = 29.0d0 - 20.88d0 * exp (-180.d0 * r2 (im, jm)) *
     &              (1.0d0 - 8.1d-3 * veg (im, jm))
           else
               write (6, *) 'KE_model_type is unknown, shouldn''t',
     &                      ' have been able to reach this point'
               stop
           endif
       else
           if (KE_model_type.eq.1) then
               ke = (11.9d0 + 8.73d0 * log10 (r2 (im, jm) * 3.6d3))
           elseif (KE_model_type.eq.2) then
c
c   JW equation if Verstraeten et al is KE=0.29*(1-0.72*EXP(-0.05*rain_intensity))
c   terms multiplied through to avoid additional calculations
c
               ke = 29.0d0 - 20.88d0 * exp (-180.d0 * r2 (im, jm))
           else
               write (6, *) 'KE_model_type is unknown, shouldn''t',
     &                      ' have been able to reach this point'
               stop
           endif
       endif
       do phi = 1, 6
c
c   calculate amount detached from cell
c
c   100 below converts slope to % as per Quansah (1981)
cEva  Equation includes temporal scaling, i.e.   splash (kg/m2/s)
c
cJWFeb05 3.6d3 * (1. / 3.) in version below simplified so calculations not
cJWFeb05    repeated needlessly.  3.6d3 converts r2 back to mm/h while 1/3 is
cJWFeb05    the scaling factor from Quansah's original measurements
cJWFeb05          detach_soil (phi, im, jm) = (spa (phi) * ((ke * r2 (im, jm) *
cJWFeb05     &                                3.6d3 * (1. / 3.)) ** spb (phi)) *
cJWFeb05     &                                ((slope (im, jm) * 100.) **
cJWFeb05     &                                spc (phi)))
          detach_soil (phi, im, jm) = (spa (phi) * ((ke * r2 (im, jm) *
     &                                1.2d3) ** spb (phi)) *
     &                                ((slope (im, jm) * 100.) **
     &                                spc (phi)))
c
cE   convert weight to height by dividing by density and NOT multiply by timestep,
cE    because this is already done in the mass balance equation
cE    detach_soil(phi): [mm]
cJWFeb05 division by density [g/cm^3] only as conversion from g to kg and from m to mm
cJWFeb05 balance out
cJWAug05 Multiplying by 2 is because the original data
cJWAug05    only give upslope/downslope values
cJWAug05
          detach_soil (phi, im, jm) = (2.0d0 *
     &                                detach_soil (phi, im, jm)) /
     &                                density
     &                                / dt
cJWJan06 check in line above for scaling in time
          if (d (1, im, jm).gt.0.0d0) then
c
cEva: depth should be given in cm, not in metres, according to Equation 3,
cEva: Parsons et al. (2004) ESPL
c
             detach_soil (phi, im, jm) = detach_soil (phi, im, jm) *
     &                                   exp (-spq (phi) *
     &                                   (d (1, im, jm) / 10.0d0))
          endif
          if (detach_soil (phi, im, jm).lt.0.) then
             detach_soil (phi, im, jm) = 0.
          endif
cJWFeb05
cJWFeb05  need to have temporal scaling in maximum detachment rate so / dt added
cJWFeb05   so units of detach_soil and tptable balance
cJWFeb05
          tptable = sed_propn (phi, im, jm) * hs (phi) / dt
          if (phi.eq.2.and.detach_soil (phi, im, jm).gt.tptable) then
c            write (6, *) ' hs limitation ', iter, phi, im, jm,
c     &                    d (2, im, jm),
c     &                    detach_soil (phi, im, jm), tptable
             detach_soil (phi, im, jm) = tptable
          endif
          if (sed_propn (phi, im, jm).eq.0) then
           detach_soil (phi, im, jm) = 0.
          endif
c
c   these lines contain feedback between gravel content & amounts of
c      fines detached -- see Wainwright et al. ESPL 1995
c
          if (phi.lt.5) then
             detach_soil (phi, im, jm) = detach_soil (phi, im, jm) *
     &                                   exp (grav_propn)
          endif

          sed_temp (phi, im, jm) = tptable - detach_soil (phi, im, jm)
          raindrop_detach_tot (im, jm) = raindrop_detach_tot (im, jm) +
     &                                   detach_soil (phi, im, jm)

       enddo

       return
       end
