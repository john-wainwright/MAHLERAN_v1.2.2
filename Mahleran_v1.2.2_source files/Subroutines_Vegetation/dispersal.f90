!****************************************************************
!  subroutine to calculate seed dispersal and establishment
!****************************************************************
subroutine dispersal

use vegdynamics_shared_data
use interstorm_shared_data
use shared_data

implicit none

real :: mean_sm, dummy1, dummy2, d_0s, cd, dist

integer :: i, j, k, n, m, o, layer, counter, duration_season, dm, dn

!sm(layer,rday,i,j) in mm
!c_veg(status 1 or 2,no. of species,i,j) in %
!d_0s: constant for exponential dispersal declcine with distance (dimensionless)
!cd: constant for exponential dispersal decline with distance

d_0s = 0.5
cd = 0.53
!calculated mortality occurs at the end of the growing season as a result of unfavourable soil moisture conditions during the whole season.
!duration_season = 110 !JW Apr 2017 should be defined as a parameter
duration_season = stop_season - start_season !JW Apr 2017 -- same as in mortality.f90
disp (:, :, :) = 0.
dm = int ((dist_max / dx_m) + 0.5)
dn = dm

do i = 2, nr
   do j = 2, nc
      if (rmask (i, j).ge.0.0d0) then
         do k = 1, no_species       !loop through number of species, at the moment: 1: grass & 2 shrub
!check mean seasonal water availability (for Sevilleta: July- end of October, i.e. 110 days)
            mean_sm = 0.
            do n = rday - duration_season + 1, rday
               mean_sm = mean_sm + sm (1, n, i, j)      !calculate only for upper layer
            enddo
            mean_sm = mean_sm / real (duration_season)  !mean soil moisture of previous 14 days in layer 1
            mean_sm = mean_sm / depth (1)   !mean soil moisture (m3/m3) of upper layer
!calculate dispersal for grass
            if (c_veg (1, 1, i, j).gt.0.and.k.eq.1.and.mean_sm.le.theta_WP (k)) then    !for grass
               disp (k, i, j) = 0.
            elseif (c_veg (1, 1, i, j).gt.0.and.k.eq.1.and.mean_sm.gt.theta_WP (k)) then    !for grass
!calculate mean grass cover for the directly surrounding grid (9 cells)
               dummy1 = 0.
               counter = 0
               do m = -1, 1
                  do n = -1, 1
                     dummy1 = dummy1 + c_veg (1, 1, i+m, j+n)
                     counter = counter + 1
                  enddo
               enddo
               dummy1 = dummy1 / 9.
               disp (k, i, j) = dummy1 * e(k) * max (1 - c_veg (1, 1, i, j) - c_veg (1, 2, i, j),0.)
            endif
!calculate dispersal for shrub
            if (k.eq.2.and.c_veg (1, 2, i, j).gt.0.30) then !check if shrub exists in current cell (assumed that cover should be at least 30%)
               !disperse the seeds of current shrub cell to surrounding cells with a radius of dist_max (currently set to 20m)
               do m = -dm, dm
                  do n = -dn, dn
                  !check mean seasonal water availability (for Sevilleta: July- end of October, i.e. 110 days)
                     if (i + m.gt.2.and.i + m.le.nr.and.j + n.gt.2.and.j + n.le.nc) then    !check edges
                        mean_sm = 0.
                        do o = rday - duration_season + 1, rday
                           mean_sm = mean_sm + sm (1, o, i + m, j + n)      !calculate only for upper layer
                        enddo
                        mean_sm = mean_sm / real (duration_season)  !mean soil moisture of previous 14 days in layer 1
                        mean_sm = mean_sm / depth(1)    !mean soil moisture (m3/m3) of upper layer
                        if (mean_sm.le.theta_WP (k)) then
                           disp (k, i + m, j + n) = 0.
                        else
                           dist = abs (m / dx_m)
                           disp (k, i + m, j + n) = disp (k, i + m, j + n) + &
                                                    c_veg (1, 2, i, j) * e (k) * d_0s * exp (-cd * dist) * &
                                                    max (1 - c_veg (1, 1, i + m, j + n) - c_veg (1, 2, i + m, j + n), 0.)
!                           write (*, *) max(1-c_veg(1,1,i+m,j+n)-c_veg(1,2,i+m,j+n),0.), disp(k,i+m,j+n), exp(-cd*dist)
!Eva: a cell should receive seeds from a multiple of surrounding cells
                        endif
                     endif
                  enddo
               enddo
            endif               !end of shrub dispersal calculation
!
!JW Apr 2017 at present nothing happens unless k=1 (grass) or k=2 (shrub)
!
         enddo      !k (no. of species)
      endif   !rmask greater zero
   enddo        !j
enddo       !i

end






