
c******************************************************************
c  subroutine to redeposit sediment in flow
c******************************************************************
c
c   uses exponential distribution function with single parameter
c   travel_dist_ave (i.e. the mean travel distance) and deposits
c   sediment to nsteps cells downslope, or until zero slope conditions
c   are encountered
c
       subroutine flow_distrib (travel_dist_ave, nsteps)

       use shared_data
       implicit double precision (a - h, o - z)
       implicit integer (i - n)

       double precision u_limit, l_limit, depos_fract
       double precision travel_dist_ave

       integer *4 nsteps
       integer ncelldistr

c       write (6, *) ' flow_distrib ', travel_dist_ave, nsteps

cEva  travel_dist_mean is needed for the step length distribution function
cEva  reciproce of mean travel distance is used in the exp. model below, in [m]
       travel_dist_par = 1.d0 / travel_dist_ave

c     change detach and depos from height in mm to weight for one whole cell in kg
cJWFeb05  added * dt to give total volume
cJWAug05
cJWAug05  test to see if converting to volume and then back to depth makes a difference
cJWAug05
cJWAug05     detach_soil (phi, im, jm) = detach_soil (phi, im, jm) *
cJWAug05     &                             density * area (im, jm) * dt
cJWAug05
cJWAug05  there is no difference so this line stays commented out
cJWAug05

c
cEva  this is the amount that does not leave the cell, i.e. the particles that are
c     detached are immediately deposited
c
       depos_fract = 1.d0 - exp (-travel_dist_par * dx_m)
       depos_soil (phi, im, jm) = depos_soil (phi, im, jm) +
     &                       depos_fract * detach_soil (phi, im, jm)

c**** this part of routine deals with flow routing
c
c
c   initialize parameters for routing
c
c
c   flow to 'N'
c
       if (aspect (im, jm).eq.1) then
          ipos = im - 1
          jpos = jm
          wid = dx
c
c   flow to 'E'
c
       elseif (aspect (im, jm).eq.2) then
          ipos = im
          jpos = jm + 1
          wid = dy
c
c   flow to 'S'
c
       elseif (aspect (im, jm).eq.3) then
          ipos = im + 1
          jpos = jm
          wid = dx
c
c   flow to 'W'
c
       else
          ipos = im
          jpos = jm - 1
          wid = dy
       endif
       ncelldistr = 0
       vge = 10.d-20 * dt
c
c
c   start loop to redeposit sediment
c
cJWFeb05  added lower limit on depos_fract to prevent unnecessary looping
c
cJWAug05     do while (ipos.ge.2.and.ipos.le.nr2.and.
cJWAug05     &           jpos.ge.2.and.jpos.le.nc2.and.
cJWAug05     &           ncelldistr.lt.nsteps.and.
cJWAug05     &           depos_fract.gt.vge)
       do while (ipos.ge.1.and.ipos.le.nr2.and.
     &           jpos.ge.1.and.jpos.le.nc2.and.
     &           ncelldistr.lt.nsteps.and.
     &           depos_fract.gt.vge)
c
c   calculate amount of sediment to be deposited from
c     amount of sediment in flux and step-length distribution
c
          u_limit = (dble (ncelldistr) + 2.0d0) * dx_m
          l_limit = u_limit - dx_m
          if (l_limit.lt.0.0d0) then
             l_limit = 0.0d0
             u_limit = 1.0d0
          endif
          depos_fract = exp (-l_limit * travel_dist_par) -
     &                exp (-u_limit * travel_dist_par)
c
cEva  the downslope cell ipos,jpos gets a fraction of the detached particles from
c     the current cell im,jm:
c
          depos_soil (phi, ipos, jpos) = depos_soil (phi, ipos, jpos) +
     &                                   detach_soil (phi, im, jm) *
     &                                   depos_fract
c
c   choose next cell for material to be deposited
c
c
c   "N"
c
          if (aspect (ipos, jpos).eq.1) then
             wid = dy
             ipos = ipos - 1
c
c    "E"
c
          elseif (aspect (ipos, jpos).eq.2) then
             wid = dx
             jpos = jpos + 1
c
c    "S"
c
          elseif (aspect (ipos, jpos).eq.3) then
             wid = dy
             ipos = ipos + 1
c
c    "W"
c
          elseif (aspect (ipos, jpos).eq.4) then
             wid = dx
             jpos = jpos - 1

cEva  aspect of boundary cells is zero, therefore exit do loop
        elseif(aspect(ipos, jpos).eq.0) then
            exit
        endif

c
c   update deposition cell
c
          ncelldistr = ncelldistr + 1
       enddo

       return
       end
