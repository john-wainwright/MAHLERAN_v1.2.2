!****************************************************************
!  subroutine to estimate the probability that a marker will be
!  detached in transitional flow conditions in which both
!  raindrop and flow detachment occur
!
!  JC May 2011
!  JW May 2014 modified to account for xml input changes
!
!****************************************************************

subroutine trans_detach_markers_xml

use shared_data
use parameters_from_xml

implicit none

double precision :: detach_propn_m

integer :: k

!    Reset detach_tot_m
do im = 1, nr1
   do jm = 1, nc1
      detach_tot_m (im, jm) = 0.0d0
   enddo
enddo

!    First use the output from the cellular part of MAHLERAN
!    These lines could be added to the bottom of splash_transport
!    Determine mass of splashed and flow detached soil in all cells for each phi class (kg)
do k = 1, 6
   do im = 1, nr1
      do jm = 1, nc1
!               Convert detach_soil (mm/s) from output in route_sediment to give total detached soil (kg)
!               First divide detach_soil by 1000 to convert from mm to m
         detach_soil_m (k, im, jm) = detach_soil (k, im, jm) / 1.0d3
!               Then convert into a density (kg/m3)
         detach_soil_m (k, im, jm) = detach_soil_m (k, im, jm) * (dx_m ** 2)
!               Convert into flux (kg/s) - density * 1.0d3 is needed to convert g/cm3 to kg/m3
         detach_soil_m (k, im, jm) = detach_soil_m (k, im, jm) * (density * 1.0d3)
!               Finally convert into mass (kg)
         detach_soil_m (k, im, jm) = detach_soil_m (k, im, jm) * dt
!               Determine total mass of splashed soil in all cells (kg)
         detach_tot_m (im, jm) = detach_tot_m (im, jm) + detach_soil_m (k, im, jm)
      enddo
   enddo
enddo

!    Determine proportion by mass of splashed soil
detach_propn_m = detach_soil_m (int (MXY (mi, 3)), mycell, mxcell) / detach_tot_m (mycell, mxcell)

!    Estimate probability of marker being detached. See Wright (1987) Equation 1
!    Available soil depth for detachment assumed to be equal to one grain diameter
if (detach_soil_m (INT (MXY (mi, 3)), mycell, mxcell).eq.0.0d0) then
   p_trans = 0.0d0
else
   p_trans = (detach_tot_m (mycell, mxcell) / ((density * 1.0d3) * (dx_m ** 2) * diameter (int (MXY (mi, 3))) )) * &
             (detach_propn_m / sed_propn (int (MXY (mi, 3)), mycell, mxcell))
endif

return
end
