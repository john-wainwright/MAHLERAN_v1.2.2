c****************************************************************
c  subroutine to calculate transport by splash
c****************************************************************
       subroutine splash_transport

       use shared_data
       implicit double precision (a - h, o - z)
       implicit integer (i - n)

       double precision splash_mv (4), splash_mv1 (4)
       double precision sedsum, sedq1, vge
       double precision tptable, splash_away
       double precision splash_across, splash_down, splash_up
       double precision depos_fract
       double precision u_limit, l_limit
       double precision bal_check

       integer *4 ipos, jpos
       integer ncelldistr, nsteps

       do phi = 1,6

c     Temporal Scaling of detachment component (this has to be done here,
c     as for d_water>1mm, the temporal scaling is done in the kinematic wave equation)
          detach_soil (phi, im, jm) = detach_soil (phi, im, jm) * dt

cJWAug2005      depos_soil (phi, im, jm) = depos_soil (phi, im, jm) +
cJWAug2005     &                               (detach_soil (phi, im, jm) - 200.
cJWAug2005     &                               / dx * detach_soil (phi, im, jm))

c     Calculation of sediment that leaves the cell to be deposited in surrounding cells:
c     it is assumed that the sediment in the current cell moves about 20 cm=200 mm per timestep
cJWAug2005      amve = 200.0 / dx
c
cJWAug2005          splash_across = 0.25d0 * amve * detach_soil (phi, im, jm)
cJWAug2005          splash_down = (0.5d0 * amve * detach_soil (phi, im, jm)) *
cJWAug2005     &                (1.0d0 - 0.5d0 * exp (-242.0d0 * slope (im, jm)))
cJWAug2005          splash_up = (0.5d0 * amve * detach_soil (phi, im, jm))
cJWAug2005     &                - splash_down
cJWAug2005
cJWAug2005   also, the scaling for downslope splash seems too high -- 242 causes there
cJWAug2005      never to be any upslope splash.  But the proportion seems right if
cJWAug2005      2.42 is used
cJWAug2005
cJWNov2005   use variable travel distance from spd parameter
cJWNov2005
          bal_check = detach_soil (phi, im, jm)
          depos_fract = 1.d0 - exp (-spd (phi) * dx_m)
          depos_soil (phi, im, jm) = depos_soil (phi, im, jm) +
     &                               detach_soil (phi, im, jm) *
     &                               depos_fract
          splash_across = 0.25d0 * detach_soil (phi, im, jm)
          splash_down = (0.5d0 * detach_soil (phi, im, jm)) *
     &                (1.0d0 - 0.5d0 * exp (-2.42d0 * slope (im, jm)))
          splash_up = (0.5d0 * detach_soil (phi, im, jm))
     &              - splash_down
          bal_check = bal_check - detach_soil (phi, im, jm) *
     &                            depos_fract
c         write (6, *) splash_across, splash_up, splash_down
c
c   determine direction of maximum slope
c
c
c   no slope -- distribute in all directions equally
c
          if (slope (im, jm).eq.0.0d0) then
             do l = 1, 4
                splash_mv (l) = splash_across
             enddo
c
c  slope to E
c
          elseif (aspect (im, jm).eq.2) then
             splash_mv (1) = splash_down
             splash_mv (3) = splash_up
             splash_mv (2) = splash_across
             splash_mv (4) = splash_across
c
c   slope to S
c
          elseif (aspect (im, jm).eq.3) then
             splash_mv (2) = splash_down
             splash_mv (4) = splash_up
             splash_mv (1) = splash_across
             splash_mv (3) = splash_across

c
c   slope to W
c
          elseif (aspect (im, jm).eq.4) then
             splash_mv (3) = splash_down
             splash_mv (1) = splash_up
             splash_mv (2) = splash_across
             splash_mv (4) = splash_across
c
c   slope to N
c
          else
             splash_mv (4) = splash_down
             splash_mv (2) = splash_up
             splash_mv (1) = splash_across
             splash_mv (3) = splash_across
          endif
          nsteps = nmax_splash (phi)
c
c   redistribute splashed material
c
c
c   left
c
c
c
c   start loop to redeposit sediment
c
          ncelldistr = 0
          vge = 10.d-50 * dt
          ipos = im
          jpos = jm - 1
          do while (ipos.ge.1.and.ipos.le.nr2.and.
     &              jpos.ge.1.and.jpos.le.nc2.and.
     &              ncelldistr.lt.nsteps.and.
     &              depos_fract.gt.vge)
             u_limit = (dble (ncelldistr) + 2.0d0) * dx_m
             l_limit = u_limit - dx_m
             if (l_limit.lt.0.0d0) then
                l_limit = 0.0d0
                u_limit = 1.0d0
             endif
             depos_fract = exp (-l_limit * spd (phi)) -
     &                   exp (-u_limit * spd (phi))
             depos_soil (phi, ipos, jpos) =
     &              depos_soil (phi, ipos, jpos) +
     &              (depos_fract * splash_mv (3))
             bal_check = bal_check -
     &              (depos_fract * splash_mv (3))
c
c   update deposition cell
c
             jpos = jpos - 1
             ncelldistr = ncelldistr + 1
          enddo
c
c   right
c
c
c
c   start loop to redeposit sediment
c
          ncelldistr = 0
          ipos = im
          jpos = jm + 1
          do while (ipos.ge.1.and.ipos.le.nr2.and.
     &              jpos.ge.1.and.jpos.le.nc2.and.
     &              ncelldistr.lt.nsteps.and.
     &              depos_fract.gt.vge)
             u_limit = (dble (ncelldistr) + 2.0d0) * dx_m
             l_limit = u_limit - dx_m
             if (l_limit.lt.0.0d0) then
                l_limit = 0.0d0
                u_limit = 1.0d0
             endif
             depos_fract = exp (-l_limit * spd (phi)) -
     &                       exp (-u_limit * spd (phi))
             depos_soil (phi, ipos, jpos) =
     &                     depos_soil (phi, ipos, jpos) +
     &                     (depos_fract * splash_mv (1))
             bal_check = bal_check -
     &                     (depos_fract * splash_mv (1))
c
c   update deposition cell
c
             jpos = jpos + 1
             ncelldistr = ncelldistr + 1
          enddo
c
c   down
c
c
c   start loop to redeposit sediment
c
          ncelldistr = 0
          ipos = im + 1
          jpos = jm
          do while (ipos.ge.1.and.ipos.le.nr2.and.
     &              jpos.ge.1.and.jpos.le.nc2.and.
     &              ncelldistr.lt.nsteps.and.
     &              depos_fract.gt.vge)
             u_limit = (dble (ncelldistr) + 2.0d0) * dx_m
             l_limit = u_limit - dx_m
             if (l_limit.lt.0.0d0) then
                l_limit = 0.0d0
                u_limit = 1.0d0
             endif
             depos_fract = exp (-l_limit * spd (phi)) -
     &                   exp (-u_limit * spd (phi))
             depos_soil (phi, ipos, jpos) =
     &                     depos_soil (phi, ipos, jpos) +
     &                     (depos_fract * splash_mv (2))
             bal_check = bal_check -
     &                     (depos_fract * splash_mv (2))
c
c   update deposition cell
c
             ipos = ipos + 1
             ncelldistr = ncelldistr + 1
          enddo
c
c   up
c
c
c   start loop to redeposit sediment
c
          ncelldistr = 0
          ipos = im - 1
          jpos = jm
          do while (ipos.ge.1.and.ipos.le.nr2.and.
     &              jpos.ge.1.and.jpos.le.nc2.and.
     &              ncelldistr.lt.nsteps.and.
     &              depos_fract.gt.vge)
             u_limit = (dble (ncelldistr) + 2.0d0) * dx_m
             l_limit = u_limit - dx_m
             if (l_limit.lt.0.0d0) then
                l_limit = 0.0d0
                u_limit = 1.0d0
             endif
             depos_fract = exp (-l_limit * spd (phi)) -
     &                   exp (-u_limit * spd (phi))
             depos_soil (phi, ipos, jpos) =
     &                     depos_soil (phi, ipos, jpos) +
     &                     (depos_fract * splash_mv (4))
             bal_check = bal_check -
     &                     (depos_fract * splash_mv (4))

c
c   update deposition cell
c
             ipos = ipos - 1
             ncelldistr = ncelldistr + 1
          enddo
c        if (bal_check.gt.0.0d0) then
c           write (6, *) ' splash balance error of ', bal_check,
c     &                   ' at phi, i, j', phi, im, jm
c        endif
cJWAug05
cJWAug05   New version accounts for edge cells
cJWAug05
          u_limit = 2.0d0 * dx_m
          depos_fract = exp (-dx_m * spd (phi)) -
     &                exp (-u_limit * spd (phi))
          if (jm.eq.2.and.aspect (im, jm).eq.3) then
             depos_soil (phi, im, jm) = depos_soil (phi, im, jm) +
     &                                  (depos_fract * splash_mv (1))
          endif
cJWAug05
cJWAug05   New version accounts for edge cells
cJWAug05
          if (jm.eq.nc.and.aspect (im, jm).eq.3) then
             depos_soil (phi, im, jm) = depos_soil (phi, im, jm) +
     &                                  (depos_fract * splash_mv (3))
          endif
cJWAug05
cJWAug05   New version accounts for edge cells
cJWAug05
          if (im.eq.nr.and.aspect (im, jm).eq.3) then
             depos_soil (phi, im, jm) = depos_soil (phi, im, jm) +
     &                                  (depos_fract * splash_mv (4))
          endif
       enddo

       return
       end
