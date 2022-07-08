!*****************************************************************
!  subroutine to output hydrograph and hydraulics data from a plot
!
!*****************************************************************
!
!  modified
!
!  27/9/2 to output sediment information also
!LTOct2007 particulate-bound nutreint output here too.
!JW June 2014 for xml format information
!
subroutine output_hydro_data_xml

use shared_data
use parameters_from_xml

implicit none
!
!  n.b. reals used deliberately here to prevent d.p output
!       confusing MATLAB visualization techniques
!JW q_plot and sed_plot moved to shared_data so that they can be accessed elsewhere
!real :: q_plot
!real :: sed_plot
real :: q_1, q_2, q_3, v_1, v_2, v_3
real :: d_1, d_2, d_3
real :: qsum_max
real :: sed_1, sed_2, sed_3
real :: sedy_plot, sedy_1, sedy_2, sedy_3
real :: ps_plot (6)
real :: amm_plot, nit_plot, phos_plot
real :: amm_1, amm_2, amm_3
real :: nit_1, nit_2, nit_3
real :: phos_1, phos_2, phos_3
real :: det_soil_tot (6), dpst_soil_tot (6), dep_soil_tot (6)
real :: vel_soil_tot (6), qs_tot (6), sed_frac_tot (6)
real :: p_amm_plot (6), p_nit_plot (6), p_TN_plot (6)
real :: p_TP_plot (6), p_IC_plot (6), p_TC_plot (6)
real :: sed_conc_tot (6)
real :: sed_hypoints (n_hypoints)
real :: d_S1, v_S1, q_S1, qs_S1, d_S2, v_S2, q_S2, qs_S2

double precision :: plot_wid, ps_sum, t_out, uc
!    integer phi

integer :: non_zero_S1, non_zero_S2
integer :: irow1, irow2, irow3
integer :: non_zero1, non_zero2, non_zero3
integer :: i, k

save qsum_max

data qsum_max / -999.999d0 /

!
!   initialize output variables
!b  The subscripts 1 - 3 refer to the location of the three transects
!b  at 25%, 50% and 75% from top of plot
!
q_plot = 0.0d0
q_1 = 0.0d0
q_2 = 0.0d0
q_3 = 0.0d0
v_1 = 0.0d0
v_2 = 0.0d0
v_3 = 0.0d0
d_1 = 0.0d0
d_2 = 0.0d0
d_3 = 0.0d0
sed_plot = 0.0d0
sed_1 = 0.0d0
sed_2 = 0.0d0
sed_3 = 0.0d0
do phi = 1, 6
   ps_plot (phi) = 0.0d0
   det_soil_tot (phi) = 0.0d0
   dpst_soil_tot (phi) = 0.0d0
   dep_soil_tot (phi) = 0.0d0
   vel_soil_tot (phi) = 0.0d0
   qs_tot (phi) = 0.0d0
   sed_frac_tot (phi) = 0.0d0
   sed_conc_tot (phi) = 0.0d0
   p_amm_plot (phi) = 0.0d0
   p_nit_plot (phi) = 0.0d0
   p_TN_plot (phi) = 0.0d0
   p_TP_plot (phi) = 0.0d0
   p_IC_plot (phi) = 0.0d0
   p_TC_plot (phi) = 0.0d0
enddo
amm_plot = 0.0
nit_plot = 0.0
phos_plot = 0.0
amm_1 = 0.0d0
amm_2 = 0.0d0
amm_3 = 0.0d0
nit_1 = 0.0d0
nit_2 = 0.0d0
nit_3 = 0.0d0
phos_1 = 0.0d0
phos_2 = 0.0d0
phos_3 = 0.0d0
!
!  locations of transects at 25%, 50% and 75% from top of plot
!
irow1 = .25 * dble (nr - 2)
irow2 = .5 * dble (nr - 2)
irow3 = .75 * dble (nr - 2)
plot_wid = 0.0d0
!
!   calculate whole plot runoff and sediment
!
do i = 2, nr
   do k = 2, nc
!
!   do these calculations for the whole plot
!
! LTOct2007 TASK?? Add in sediment-bound nutrient routine here for outputting??
!
      do phi = 1, 6
         det_soil_tot (phi) = det_soil_tot (phi) + detach_soil (phi, i, k)
     dpst_soil_tot (phi) = dpst_soil_tot (phi) + depos_soil (phi, i, k)
     dep_soil_tot (phi) = dep_soil_tot (phi) + d_soil (phi, 2, i, k)
     vel_soil_tot (phi) = vel_soil_tot (phi) + v_soil (phi, i, k)
      enddo
!
!LTOct2007 added in particulate nutrients for each size class
!
!JW Apr 2017 should be able to simplify to single loop with condition:
      if ((aspect (i, k).eq.1.and.rmask (i, k).gt.0.0d0.and.rmask (i - 1, k).lt.0.0d0).or. &
          (aspect (i, k).eq.2.and.rmask (i, k).gt.0.0d0.and.rmask (i, k + 1).lt.0.0d0).or. &
          (aspect (i, k).eq.3.and.rmask (i, k).gt.0.0d0.and.rmask (i + 1, k).lt.0.0d0).or. &
          (aspect (i, k).eq.4.and.rmask (i, k).gt.0.0d0.and.rmask (i, k - 1).lt.0.0d0)) then
         q_plot = q_plot + q (2, i, k)
     do phi = 1, 6
        ps_plot (phi) = ps_plot (phi) + q_soil (phi, 2, i, k)
            sed_plot = sed_plot + q_soil (phi, 2, i, k)
            qs_tot (phi) = qs_tot (phi) + q_soil (phi, 2, i, k)
        sed_frac_tot (phi) = sed_frac_tot (phi) + sed_propn (phi, i, k)
!
!LTOct2007 plot particulate nutrients (grams).
!
            p_amm_plot (phi) = p_amm_plot (phi) + amm_q (phi, i, k)
            p_nit_plot (phi) = p_nit_plot (phi) + nit_q (phi, i, k)
            p_TN_plot (phi) = p_TN_plot (phi) + TN_q (phi, i, k)
            p_TP_plot (phi) = p_TP_plot (phi) + TP_q (phi, i, k)
            p_IC_plot (phi) = p_IC_plot (phi) + IC_q (phi, i, k)
            p_TC_plot (phi) = p_TC_plot (phi) + TC_q (phi, i, k)

            if (q (2, i, k).gt.0.0d0) then
           sed_conc_tot (phi) = sed_conc_tot (phi) + (q_soil (phi, 2, i, k) / q (2, i, k))
        endif
     enddo
     amm_plot = amm_plot + ammonium (2, i, k)
     nit_plot = nit_plot + nitrate (2, i, k)
     phos_plot = phos_plot + phosphorus (2, i, k)
     plot_wid = plot_wid + dx
      endif
!JW Apr 2017 end of simplified condition

!      if (aspect (i, k).eq.1.and.rmask (i, k).gt.0.0d0.and.rmask (i - 1, k).lt.0.0d0) then
!    q_plot = q_plot + q (2, i, k)
!    do phi = 1, 6
!       ps_plot (phi) = ps_plot (phi) + q_soil (phi, 2, i, k)
!            sed_plot = sed_plot + q_soil (phi, 2, i, k)
!            qs_tot (phi) = qs_tot (phi) + q_soil (phi, 2, i, k)
!       sed_frac_tot (phi) = sed_frac_tot (phi) + sed_propn (phi, i, k)
!
!LTOct2007 plot particulate nutrients (grams).
!
!            p_amm_plot (phi) = p_amm_plot (phi) + amm_q (phi, i, k)
!            p_nit_plot (phi) = p_nit_plot (phi) + nit_q (phi, i, k)
!            p_TN_plot (phi) = p_TN_plot (phi) + TN_q (phi, i, k)
!            p_TP_plot (phi) = p_TP_plot (phi) + TP_q (phi, i, k)
!            p_IC_plot (phi) = p_IC_plot (phi) + IC_q (phi, i, k)
!            p_TC_plot (phi) = p_TC_plot (phi) + TC_q (phi, i, k)
!
!            if (q (2, i, k).gt.0.0d0) then
!          sed_conc_tot (phi) = sed_conc_tot (phi) + (q_soil (phi, 2, i, k) / q (2, i, k))
!       endif
!    enddo
!    amm_plot = amm_plot + ammonium (2, i, k)
!    nit_plot = nit_plot + nitrate (2, i, k)
!    phos_plot = phos_plot + phosphorus (2, i, k)
!    plot_wid = plot_wid + dx
!
!      elseif (aspect (i, k).eq.2.and.rmask (i, k).gt.0.0d0.and.rmask (i, k + 1).lt.0.0d0) then
!    q_plot = q_plot + q (2, i, k)
!    do phi = 1, 6
!       ps_plot (phi) = ps_plot (phi) + q_soil (phi, 2, i, k)
!       sed_plot = sed_plot + q_soil (phi, 2, i, k)
!            qs_tot (phi) = qs_tot (phi) + q_soil (phi, 2, i, k)
!            sed_frac_tot (phi) = sed_frac_tot (phi) + sed_propn (phi, i, k)
!
!cLTOct2007 plot particulate nutrients (grams)
!
!            p_amm_plot (phi) = p_amm_plot (phi) + amm_q (phi, i, k)
!            p_nit_plot (phi) = p_nit_plot (phi) + nit_q (phi, i, k)
!            p_TN_plot (phi) = p_TN_plot (phi) + TN_q (phi, i, k)
!            p_TP_plot (phi) = p_TP_plot (phi) + TP_q (phi, i, k)
!            p_IC_plot (phi) = p_IC_plot (phi) + IC_q (phi, i, k)
!            p_TC_plot (phi) = p_TC_plot (phi) + TC_q (phi, i, k)
!       if (q (2, i, k).gt.0.0d0) then
!          sed_conc_tot (phi) = sed_conc_tot (phi) + (q_soil (phi, 2, i, k) / q (2, i, k))
!       endif
!    enddo
!    amm_plot = amm_plot + ammonium (2, i, k)
!    nit_plot = nit_plot + nitrate (2, i, k)
!    phos_plot = phos_plot + phosphorus (2, i, k)
!    plot_wid = plot_wid + dy
!      elseif (aspect (i, k).eq.3.and.rmask (i, k).gt.0.0d0.and.rmask (i + 1, k).lt.0.0d0) then
!    q_plot = q_plot + q (2, i, k)
!    do phi = 1, 6
!       ps_plot (phi) = ps_plot (phi)  + q_soil (phi, 2, i, k)
!       sed_plot = sed_plot + q_soil (phi, 2, i, k)
!            qs_tot (phi) = qs_tot (phi) + q_soil (phi, 2, i, k)
!       sed_frac_tot (phi) = sed_frac_tot (phi) + sed_propn (phi, i, k)
!
!LTOct2007 plot particulate nutrients (grams)
!
!            p_amm_plot (phi) = p_amm_plot (phi) + amm_q (phi, i, k)
!            p_nit_plot (phi) = p_nit_plot (phi) + nit_q (phi, i, k)
!            p_TN_plot (phi) = p_TN_plot (phi) + TN_q (phi, i, k)
!            p_TP_plot (phi) = p_TP_plot (phi) + TP_q (phi, i, k)
!            p_IC_plot (phi) = p_IC_plot (phi) + IC_q (phi, i, k)
!            p_TC_plot (phi) = p_TC_plot (phi) + TC_q (phi, i, k)
!            if (q (2, i, k).gt.0.0d0) then
!               sed_conc_tot (phi) = sed_conc_tot (phi) + (q_soil (phi, 2, i, k) / q (2, i, k))
!       endif
!    enddo
!    amm_plot = amm_plot + ammonium (2, i, k)
!    nit_plot = nit_plot + nitrate (2, i, k)
!    phos_plot = phos_plot + phosphorus (2, i, k)
!    plot_wid = plot_wid + dx
!      elseif (aspect (i, k).eq.4.and.rmask (i, k).gt.0.0d0.and.rmask (i, k - 1).lt.0.0d0) then
!    q_plot = q_plot + q (2, i, k)
!    do phi = 1, 6
!       ps_plot (phi) = ps_plot (phi)  + q_soil (phi, 2, i, k)
!       sed_plot = sed_plot + q_soil (phi, 2, i, k)
!       qs_tot (phi) = qs_tot (phi) + q_soil (phi, 2, i, k)
!       sed_frac_tot (phi) = sed_frac_tot (phi) + sed_propn (phi, i, k)
!
!LTOct2007 plot particulate nutrients (grams)
!
!            p_amm_plot (phi) = p_amm_plot (phi) + amm_q (phi, i, k)
!       p_nit_plot (phi) = p_nit_plot (phi) + nit_q (phi, i, k)
!            p_TN_plot (phi) = p_TN_plot (phi) + TN_q (phi, i, k)
!            p_TP_plot (phi) = p_TP_plot (phi) + TP_q (phi, i, k)
!            p_IC_plot (phi) = p_IC_plot (phi) + IC_q (phi, i, k)
!            p_TC_plot (phi) = p_TC_plot (phi) + TC_q (phi, i, k)
!            if (q (2, i, k).gt.0.0d0) then
!               sed_conc_tot (phi) = sed_conc_tot (phi) + (q_soil (phi, 2, i, k) / q (2, i, k))
!            endif
!    enddo
!    amm_plot = amm_plot + ammonium (2, i, k)
!    nit_plot = nit_plot + nitrate (2, i, k)
!    phos_plot = phos_plot + phosphorus (2, i, k)
!    plot_wid = plot_wid + dy
!      endif
   enddo
enddo
!
!  convert back from depth to weight and calculate yield per unit area
!
!Eva    uc: unit converter from fluxes (mm2/s * cell width [mm] * density [g/cm3] to
!   discharge in [kg/s]
uc = dx * density * 1.e-6
sed_plot = sed_plot * uc
sedy_plot = sed_plot / dble (nr)
do phi = 1, 6
   ps_plot (phi) = ps_plot (phi) * uc
enddo
!
!  calculate average nutrient concentrations
!
amm_plot = amm_plot / (plot_wid / dy)
nit_plot = nit_plot / (plot_wid / dy)
phos_plot = phos_plot / (plot_wid / dy)
!
!   calculate transect data
!
non_zero1 = 0
non_zero2 = 0
non_zero3 = 0
do k = 2, nc
!
!   only add discharge if flowing S (towards outlet)
!bq  The following piece of code is site-specific as it assumes that the outlet
!bq  is in the south direction.
!
   if (aspect (irow1, k).eq.3) then
      q_1 = q_1 + q (2, irow1, k)
   endif
   if (aspect (irow2, k).eq.3) then
      q_2 = q_2 + q (2, irow2, k)
   endif
   if (aspect (irow3, k).eq.3) then
      q_3 = q_3 + q (2, irow3, k)
   endif
!
!   base depths and velocities on non-zero values only
!   and nutrient concentrations
!
   if (d (2, irow1, k).gt.0.0d0) then
      non_zero1 = non_zero1 + 1
      v_1 = v_1 + v (irow1, k)
      d_1 = d_1 + d (2, irow1, k)
      amm_1 = amm_1 + ammonium (2, irow1, k)
      nit_1 = nit_1 + nitrate (2, irow1, k)
      phos_1 = phos_1 + phosphorus (2, irow1, k)
   endif
   if (d (2, irow2, k).gt.0.0d0) then
      non_zero2 = non_zero2 + 1
      v_2 = v_2 + v (irow2, k)
      d_2 = d_2 + d (2, irow2, k)
      amm_2 = amm_2 + ammonium (2, irow2, k)
      nit_2 = nit_2 + nitrate (2, irow2, k)
      phos_2 = phos_2 + phosphorus (2, irow2, k)
   endif
   if (d (2, irow3, k).gt.0.0d0) then
      non_zero3 = non_zero3 + 1
      v_3 = v_3 + v (irow3, k)
      d_3 = d_3 + d (2, irow3, k)
      amm_3 = amm_3 + ammonium (2, irow3, k)
      nit_3 = nit_3 + nitrate (2, irow3, k)
      phos_3 = phos_3 + phosphorus (2, irow3, k)
   endif
!
!  sediment added under all flow conditions because of splash
!
   do phi = 1, 6
      sed_1 = sed_1 + q_soil (phi, 2, irow1, k)
      sed_2 = sed_2 + q_soil (phi, 2, irow2, k)
      sed_3 = sed_3 + q_soil (phi, 2, irow3, k)
   enddo
enddo
!
!  convert back from from fluxes (mm2/s * cell width [mm] * density [g/cm3] to
!   discharge in [kg/s]
!
sed_1 = sed_1 * uc
sed_2 = sed_2 * uc
sed_3 = sed_3 * uc
!
!   calculate averages based on non-zero values
!
if (non_zero1.gt.0) then
   d_1 = d_1 / dble (non_zero1)
   v_1 = v_1 / dble (non_zero1)
   amm_1 = amm_1 / dble (non_zero1)
   nit_1 = nit_1 / dble (non_zero1)
   phos_1 = phos_1 / dble (non_zero1)
endif
if (non_zero2.gt.0) then
   d_2 = d_2 / dble (non_zero2)
   v_2 = v_2 / dble (non_zero2)
   amm_2 = amm_2 / dble (non_zero2)
   nit_2 = nit_2 / dble (non_zero2)
   phos_2 = phos_2 / dble (non_zero2)
endif
if (non_zero3.gt.0) then
   d_3 = d_3 / dble (non_zero3)
   v_3 = v_3 / dble (non_zero3)
   amm_3 = amm_3 / dble (non_zero3)
   nit_3 = nit_3 / dble (non_zero3)
   phos_3 = phos_3 / dble (non_zero3)
endif
!
!  calculate sediment fluxes per unit width
!   - n.b. entire widths used because of splash
!   - / 1.0d3 converts from mm to m
!
sed_1 = sed_1 / (dble (nc - 1) * dx / 1.0d3)
sed_2 = sed_2 / (dble (nc - 1) * dx / 1.0d3)
sed_3 = sed_3 / (dble (nc - 1) * dx / 1.0d3)
!
!  calculate sediment yields per unit area
!   - n.b. entire widths used because of splash
!   - / 1.0d3 converts from mm to m
!
sedy_1 = sed_1 / (dble (irow1 - 1) * dx / 1.0d3)
sedy_2 = sed_2 / (dble (irow2 - 1) * dx / 1.0d3)
sedy_3 = sed_3 / (dble (irow3 - 1) * dx / 1.0d3)
!
!   normalize particle size fraction at outlet to sum to 1
!
ps_sum = 0.0d0
do phi = 1, 6
   ps_sum = ps_sum + ps_plot (phi)
enddo
do phi = 1, 6
   if (ps_sum.ne.0.0d0) then
      ps_plot (phi) = ps_plot (phi) / ps_sum
   endif
enddo

t_out = iter * dt
!
!   output: time, rainfall, plot discharge, cross-section discharges, c-s depths, c-s velocities
!
if (f_hydrofile) then
   write (57, '(12(e10.4, 1x))') t_out, rval, q_plot * dx, q_1 * dx, q_2 * dx, q_3 * dx, d_1, d_2, d_3, v_1, v_2, v_3
endif
!
!   output: time, plot sediment output, plot sediment flux, c-s fluxes, plot yield per unit area, c-s yields per unit area, plot particle size outb
!
if (f_sedfile) then
   write (58, '(16(e10.4, 1x))') t_out, sed_plot, sed_plot / (plot_wid / 1.0d3), sed_1, sed_2, sed_3, &
                                 sedy_plot / (plot_wid / 1.0d3), sedy_1, sedy_2, sedy_3, (ps_plot (phi), phi = 1, 6)
endif
!
!   Output nutrient [NH4 NO3 P] concentrations in [mg/l], and then fluxes in [mg/s]
!         for plot and then cross sections
!
if (f_nutrientfile) then
   write (60, '(25(e10.4,1x))') t_out, amm_plot, nit_plot, phos_plot, amm_plot * q_plot * dx * 1.e-6, &
                                nit_plot * q_plot * dx * 1.e-6, phos_plot * q_plot * dx * 1.e-6, amm_1, nit_1, phos_1, &
                                amm_1 * q_1 * dx * 1.e-6, nit_1 * q_1 * dx * 1.e-6, phos_1 * q_1 * dx * 1.e-6, &
                                amm_2, nit_2, phos_2, amm_2 * q_2 * dx * 1.e-6, nit_2 * q_2 * dx * 1.e-6, &
                                phos_2 * q_2 * dx * 1.e-6, amm_3, nit_3, phos_3, amm_3 * q_3 * dx * 1.e-6, &
                                nit_3 * q_3 * dx * 1.e-6, phos_3 * q_3 * dx * 1.e-6
endif
uc = dx * density * 1.e-6
!
!   detach_soil [kg/m2]
!
if (f_seddetfile) then
   write (100,'(7(e10.4,1x))') t_out, (det_soil_tot (phi) * uc, phi = 1, 6)
endif
!
!   depos_soil in [kg/m2]
!
if (f_seddeposfile) then
   write (101,'(7(e10.4,1x))') t_out, (dpst_soil_tot (phi) * uc, phi = 1, 6)
endif
!
!   soil depth [m]
!
if (f_seddepthfile) then
   write (102,'(7(e10.4,1x))') t_out, (dep_soil_tot (phi),  phi = 1, 6)
endif
!
!   virtual velocity
!     ((nr - 1) * (nc - 1)) scales to number of cells but doesn't
!     account for cells with no flow
!
if (f_sedvelfile) then
   write (103,'(7(e10.4,1x))') t_out, (vel_soil_tot (phi) /((nr - 1) * (nc - 1)), phi = 1, 6)
endif
!
!     sediment discharge
!
!Eva    uc: unit converter from fluxes (mm2/s * cell width [mm] * density [g/cm3] to
!   discharge in [kg/s]
if (f_seddischfile) then
   write (104,'(7(e10.4,1x))') t_out, (qs_tot (phi) * uc,  phi = 1, 6)
endif
!
!     sediment fractions
!
if (f_sedpropnfile) then
   write (105,'(7(e10.4,1x))') t_out, (sed_frac_tot (phi),  phi = 1, 6)
endif
!
!LTOct2007 output particulate-bound nutrient discharge (g) in each phi class
if (f_p_nutfile) then
   write (55,'(37(e10.4,1x))') t_out, (p_amm_plot (phi),  phi = 1, 6), (p_nit_plot (phi),  phi = 1, 6), &
                               (p_TN_plot (phi), phi = 1, 6), (p_TP_plot (phi), phi = 1, 6), &
                               (p_IC_plot (phi), phi = 1, 6), (p_TC_plot (phi), phi = 1, 6)
endif

!
!     sediment concentration
!
!   uc: unit converter to calculate the concentration of sediment in water flow
!   (q_soil*dx*10-6*density) / (q_water*dx*10-9)= 1000* density, sediment concentration in g/l
uc = 1000.d0 * density
if (f_sedconcfile) then
   write (106,'(7(e10.4,1x))') t_out, (sed_conc_tot (phi) * uc, phi = 1, 6)
endif
!
!   save maximum depth and velocity if peak discharge reached
!
if (q_plot.gt.qsum_max) then
   qsum_max = q_plot
   do i = 1, nr1
      do k = 1, nc1
         dmax (i, k) = d (2, i, k)
     vmax (i, k) = v (i, k)
     dmax_soil (i, k) = 0.0d0
     vmax_soil (i, k) = 0.0d0
     do phi = 1, 6
        dmax_soil (i, k) = dmax_soil (i, k) + d_soil (phi, 2, i, k)
        vmax_soil (i, k) = vmax_soil (i, k) + v_soil (phi, i, k)
     enddo
      enddo
   enddo
endif

if (f_v_xsect) then
   write (107, '(1000(e10.4, 1x))') t_out, (v (irow1, k), k = 2, nc), (v (irow2, k), k = 2, nc), (v (irow3, k), k = 2, nc), &
                                    dble (non_zero1), dble (non_zero2), dble (non_zero3)
endif
if (f_d_xsect) then
   write (108, '(1000(e10.4, 1x))') t_out, (d (2, irow1, k), k = 2, nc), (d (2, irow2, k), k = 2, nc), (d (2, irow3, k), k = 2, nc)
endif
if (f_q_xsect) then
   write (109, '(1000(e10.4, 1x))') t_out, (q (2, irow1, k) * dx, k = 2, nc), (q (2, irow2, k) * dx, k = 2, nc), &
                                    (q (2, irow3, k) * dx, k = 2, nc)
endif
!
!JWMar2006
!JWMar2006  Output cross-section data for shrubland comparison assuming 0.61-m cells
!JWMar2006
if (shrubland_xsect) then
   d_S1 = 0.0e0
   v_S1 = 0.0e0
   q_S1 = 0.0e0
   qs_S1 = 0.0e0
   non_zero_S1 = 0
   d_S2 = 0.0e0
   v_S2 = 0.0e0
   q_S2 = 0.0e0
   qs_S2 = 0.0e0
   non_zero_S2 = 0
   do k = 2, nc
      if (q (2, 21, k).gt.0.0d0) then
     non_zero_S1 = non_zero_S1 + 1
         d_S1 = d_S1 + d (2, 21, k)
     v_S1 = v_S1 + v (21, k)
     q_S1 = q_S1 + q (2, 21, k)
     do phi = 1, 6
        qs_S1 = qs_S1 + q_soil (phi, 2, 21, k)
     enddo
      endif
      if (q (2, 35, k).gt.0.0d0) then
     non_zero_S2 = non_zero_S2 + 1
         d_S2 = d_S2 + d (2, 35, k)
     v_S2 = v_S2 + v (35, k)
     q_S2 = q_S2 + q (2, 35, k)
     do phi = 1, 6
        qs_S2 = qs_S2 + q_soil (phi, 2, 35, k)
         enddo
      endif
   enddo
   if (non_zero_S1.gt.0) then
      d_S1 = d_S1 / non_zero_S1
      v_S1 = v_S1 / non_zero_S1
      qs_S1 = qs_S1 * dx * density * 1.e-6
   endif
   if (non_zero_S2.gt.0) then
      d_S2 = d_S2 / non_zero_S2
      v_S2 = v_S2 / non_zero_S2
      qs_S2 = qs_S2 * dx * density * 1.e-6
   endif

   write (99, '(9(e10.4, 1x), i2, 1x, i2)') t_out, d_S1, v_S1, q_S1, qs_S1, d_S2, v_S2, q_S2, qs_S2, non_zero_S1, non_zero_S2
endif

!
!JWAug2005
!JWAug2005   output spatial hydro-, sedi- and nutrigraphs if required
!JWAug2005
if (hydro_out) then
   if (f_qpointfile) then
      write (110, '(1000(e10.4, 1x))') t_out, (q (2, x_hypoints (k), y_hypoints (k)) * dx, k = 1, n_hypoints)
      do k = 1, n_hypoints
         sed_hypoints (k) = 0.0e0
      enddo
      uc = dx * density * 1.e-6
      do k = 1, n_hypoints
         do phi = 1, 6
        sed_hypoints (k) = sed_hypoints (k) + q_soil (phi, 2, x_hypoints (k), y_hypoints (k))
         enddo
     sed_hypoints (k) = sed_hypoints (k) * uc
      enddo
      if (f_qspointfile) then
         write (111, '(1000(e10.4, 1x))') t_out, (sed_hypoints (k), k = 1, n_hypoints)
      endif
      if (f_nutpointfile) then
         write (112, '(1000(e10.4, 1x))') t_out, (nitrate (2, x_hypoints (k), y_hypoints (k)),&
                                          ammonium (2, x_hypoints (k), y_hypoints (k)), &
                                          phosphorus (2, x_hypoints (k), y_hypoints (k)), k = 1, n_hypoints)
      endif
   endif
endif

return
end
