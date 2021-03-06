
c*****************************************************************
c  subroutine to output hydrograph and hydraulics data from a plot
c
c*****************************************************************
c
c  modified
c
c  27/9/2 to output sediment information also
cLTOct2007 particulate-bound nutreint output here too.
c
       subroutine output_hydro_data

       use shared_data
       implicit double precision (a - h, o - z)
       implicit integer (i - n)
c
c  n.b. reals used deliberately here to prevent d.p output
c       confusing MATLAB visualization techniques
c
       real q_plot, q_1, q_2, q_3, v_1, v_2, v_3
       real d_1, d_2, d_3
       real qsum_max
       real sed_plot, sed_1, sed_2, sed_3
       real sedy_plot, sedy_1, sedy_2, sedy_3
       real ps_plot (6)
       real amm_plot, nit_plot, phos_plot
       real amm_1, amm_2, amm_3
       real nit_1, nit_2, nit_3
       real phos_1, phos_2, phos_3
       real det_soil_tot (6), dpst_soil_tot (6), dep_soil_tot (6)
       real vel_soil_tot (6), qs_tot (6), sed_frac_tot (6)
       real p_amm_plot (6), p_nit_plot (6), p_TN_plot (6)
       real p_TP_plot (6), p_IC_plot (6), p_TC_plot (6)
       real sed_conc_tot (6)
       real sed_hypoints (n_hypoints)
       real d_S1, v_S1, q_S1, qs_S1, d_S2, v_S2, q_S2, qs_S2

c      integer phi

       integer non_zero_S1, non_zero_S2

       save qsum_max

       data qsum_max / -999.999d0 /

c
c   initialize output variables
cb  The subscripts 1 - 3 refer to the location of the three transects
cb  at 25%, 50% and 75% from top of plot
c
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
c
c  locations of transects at 25%, 50% and 75% from top of plot
c
       irow1 = .25 * dble (nr - 2)
       irow2 = .5 * dble (nr - 2)
       irow3 = .75 * dble (nr - 2)
       plot_wid = 0.0d0
c
c   calculate whole plot runoff and sediment
c
       do i = 2, nr
        do j = 2, nc
c
c   do these calculations for the whole plot
c
c LTOct2007 TASK?? Add in sediment-bound nutrient routine here for outputting??
c
             do phi = 1, 6
                det_soil_tot (phi) = det_soil_tot (phi) +
     &                               detach_soil (phi, i, j)
        dpst_soil_tot (phi) = dpst_soil_tot (phi) +
     &                                depos_soil (phi, i, j)
        dep_soil_tot (phi) = dep_soil_tot (phi) +
     &                               d_soil (phi, 2, i, j)
              vel_soil_tot (phi) = vel_soil_tot (phi) +
     &                               v_soil (phi, i, j)

             enddo

cLTOct2007 added in particulate nutrients for each size class

             if (aspect (i, j).eq.1.and.rmask (i,j).gt.0.0d0.and.
     &           rmask (i - 1, j).lt.0.0d0) then
              q_plot = q_plot + q (2, i, j)
              do phi = 1, 6
                 ps_plot (phi) = ps_plot (phi) +
     &                             q_soil (phi, 2, i, j)
                 sed_plot = sed_plot + q_soil (phi, 2, i, j)
           qs_tot (phi) = qs_tot (phi) + q_soil (phi, 2, i, j)
           sed_frac_tot (phi) = sed_frac_tot (phi) +
     &                                  sed_propn (phi, i, j)
cLTOct2007 plot particulate nutrients (grams).
           p_amm_plot (phi) = p_amm_plot (phi) +
     &                    amm_q (phi, i, j)
           p_nit_plot (phi) = p_nit_plot (phi) +
     &                    nit_q (phi, i, j)
           p_TN_plot (phi) = p_TN_plot (phi) +
     &                   TN_q (phi, i, j)
           p_TP_plot (phi) = p_TP_plot (phi) +
     &                   TP_q (phi, i, j)
           p_IC_plot (phi) = p_IC_plot (phi) +
     &                       IC_q (phi, i, j)
           p_TC_plot (phi) = p_TC_plot (phi) +
     &                       TC_q (phi, i, j)

                 if (q (2, i, j).gt.0.0d0) then
                    sed_conc_tot (phi) = sed_conc_tot (phi) +
     &                                     (q_soil (phi, 2, i, j) /
     &                                     q (2, i, j))
                 endif
              enddo
              amm_plot = amm_plot + ammonium (2, i, j)
              nit_plot = nit_plot + nitrate (2, i, j)
              phos_plot = phos_plot + phosphorus (2, i, j)
              plot_wid = plot_wid + dx

             elseif (aspect (i, j).eq.2.and.rmask (i,j).gt.0.0d0.and.
     &               rmask (i, j + 1).lt.0.0d0) then
              q_plot = q_plot + q (2, i, j)
              do phi = 1, 6
                 ps_plot (phi) = ps_plot (phi) +
     &                             q_soil (phi, 2, i, j)
                 sed_plot = sed_plot + q_soil (phi, 2, i, j)
           qs_tot (phi) = qs_tot (phi) + q_soil (phi, 2, i, j)
           sed_frac_tot (phi) = sed_frac_tot (phi) +
     &                                  sed_propn (phi, i, j)
cLTOct2007 plot particulate nutrients (grams)
           p_amm_plot (phi) = p_amm_plot (phi) +
     &                            amm_q (phi, i, j)
           p_nit_plot (phi) = p_nit_plot (phi) +
     &                            nit_q (phi, i, j)
                   p_TN_plot (phi) = p_TN_plot (phi) +
     &                           TN_q (phi, i, j)
           p_TP_plot (phi) = p_TP_plot (phi) +
     &                           TP_q (phi, i, j)
           p_IC_plot (phi) = p_IC_plot (phi) +
     &                           IC_q (phi, i, j)
           p_TC_plot (phi) = p_TC_plot (phi) +
     &                           TC_q (phi, i, j)
                 if (q (2, i, j).gt.0.0d0) then
                    sed_conc_tot (phi) = sed_conc_tot (phi) +
     &                                     (q_soil (phi, 2, i, j) /
     &                                      q (2, i, j))
                 endif
              enddo
              amm_plot = amm_plot + ammonium (2, i, j)
              nit_plot = nit_plot + nitrate (2, i, j)
              phos_plot = phos_plot + phosphorus (2, i, j)
              plot_wid = plot_wid + dy

           elseif (aspect (i, j).eq.3.and.rmask (i,j).gt.0.0d0.and.
     &               rmask (i + 1, j).lt.0.0d0) then
              q_plot = q_plot + q (2, i, j)
              do phi = 1, 6
                 ps_plot (phi) = ps_plot (phi)  +
     &                             q_soil (phi, 2, i, j)
                 sed_plot = sed_plot + q_soil (phi, 2, i, j)
           qs_tot (phi) = qs_tot (phi) +
     &                            q_soil (phi, 2, i, j)
           sed_frac_tot (phi) = sed_frac_tot (phi) +
     &                                  sed_propn (phi, i, j)
cLTOct2007 plot particulate nutrients (grams)
           p_amm_plot (phi) = p_amm_plot (phi) +
     &                            amm_q (phi, i, j)
           p_nit_plot (phi) = p_nit_plot (phi) +
     &                            nit_q (phi, i, j)
           p_TN_plot (phi) = p_TN_plot (phi) +
     &                           TN_q (phi, i, j)
           p_TP_plot (phi) = p_TP_plot (phi) +
     &                           TP_q (phi, i, j)
           p_IC_plot (phi) = p_IC_plot (phi) +
     &                           IC_q (phi, i, j)
           p_TC_plot (phi) = p_TC_plot (phi) +
     &                           TC_q (phi, i, j)
                 if (q (2, i, j).gt.0.0d0) then
                    sed_conc_tot (phi) = sed_conc_tot (phi) +
     &                                     (q_soil (phi, 2, i, j) /
     &                                      q (2, i, j))
                 endif
              enddo
              amm_plot = amm_plot + ammonium (2, i, j)
              nit_plot = nit_plot + nitrate (2, i, j)
              phos_plot = phos_plot + phosphorus (2, i, j)
              plot_wid = plot_wid + dx
           elseif (aspect (i, j).eq.4.and.rmask (i,j).gt.0.0d0.and.
     &               rmask (i, j - 1).lt.0.0d0) then
              q_plot = q_plot + q (2, i, j)
              do phi = 1, 6
                 ps_plot (phi) = ps_plot (phi)  +
     &                             q_soil (phi, 2, i, j)
                 sed_plot = sed_plot + q_soil (phi, 2, i, j)
           qs_tot (phi) = qs_tot (phi) +
     &                            q_soil (phi, 2, i, j)
           sed_frac_tot (phi) = sed_frac_tot (phi) +
     &                                  sed_propn (phi, i, j)
cLTOct2007 plot particulate nutrients (grams)
           p_amm_plot (phi) = p_amm_plot (phi) +
     &                            amm_q (phi, i, j)
           p_nit_plot (phi) = p_nit_plot (phi) +
     &                            nit_q (phi, i, j)
           p_TN_plot (phi) = p_TN_plot (phi) +
     &                           TN_q (phi, i, j)
           p_TP_plot (phi) = p_TP_plot (phi) +
     &                           TP_q (phi, i, j)
           p_IC_plot (phi) = p_IC_plot (phi) +
     &                           IC_q (phi, i, j)
           p_TC_plot (phi) = p_TC_plot (phi) +
     &                           TC_q (phi, i, j)
                 if (q (2, i, j).gt.0.0d0) then
                    sed_conc_tot (phi) = sed_conc_tot (phi) +
     &                                     (q_soil (phi, 2, i, j) /
     &                                      q (2, i, j))
                 endif
              enddo
              amm_plot = amm_plot + ammonium (2, i, j)
              nit_plot = nit_plot + nitrate (2, i, j)
              phos_plot = phos_plot + phosphorus (2, i, j)
              plot_wid = plot_wid + dy
           endif
        enddo
       enddo
c
c  convert back from depth to weight and calculate yield per unit area
c
cEva  uc: unit converter from fluxes (mm2/s * cell width [mm] * density [g/cm3] to
c     discharge in [kg/s]
       uc = dx * density * 1.e-6
       sed_plot = sed_plot * uc
       sedy_plot = sed_plot / dble (nr)
       do phi = 1, 6
        ps_plot (phi) = ps_plot (phi) * uc
       enddo
c
c  calculate average nutrient concentrations
c
       amm_plot = amm_plot / (plot_wid / dy)
       nit_plot = nit_plot / (plot_wid / dy)
       phos_plot = phos_plot / (plot_wid / dy)
c
c   calculate transect data
c
       non_zero1 = 0
       non_zero2 = 0
       non_zero3 = 0
       do j = 2, nc
c
c   only add discharge if flowing S (towards outlet)
cbq  The followig piece of code is site-specific as it assumes that the outlet
cbq  is in the south direction.
c
          if (aspect (irow1, j).eq.3) then
           q_1 = q_1 + q (2, irow1, j)
        endif
          if (aspect (irow2, j).eq.3) then
           q_2 = q_2 + q (2, irow2, j)
        endif
          if (aspect (irow3, j).eq.3) then
           q_3 = q_3 + q (2, irow3, j)
        endif
c
c   base depths and velocities on non-zero values only
c   and nutrient concentrations
c
        if (d (2, irow1, j).gt.0.0d0) then
           non_zero1 = non_zero1 + 1
           v_1 = v_1 + v (irow1, j)
           d_1 = d_1 + d (2, irow1, j)
           amm_1 = amm_1 + ammonium (2, irow1, j)
           nit_1 = nit_1 + nitrate (2, irow1, j)
           phos_1 = phos_1 + phosphorus (2, irow1, j)
        endif
        if (d (2, irow2, j).gt.0.0d0) then
           non_zero2 = non_zero2 + 1
           v_2 = v_2 + v (irow2, j)
           d_2 = d_2 + d (2, irow2, j)
             amm_2 = amm_2 + ammonium (2, irow2, j)
             nit_2 = nit_2 + nitrate (2, irow2, j)
             phos_2 = phos_2 + phosphorus (2, irow2, j)
        endif
        if (d (2, irow3, j).gt.0.0d0) then
           non_zero3 = non_zero3 + 1
           v_3 = v_3 + v (irow3, j)
           d_3 = d_3 + d (2, irow3, j)
             amm_3 = amm_3 + ammonium (2, irow3, j)
             nit_3 = nit_3 + nitrate (2, irow3, j)
             phos_3 = phos_3 + phosphorus (2, irow3, j)
        endif
c
c  sediment added under all flow conditions because of splash
c
          do phi = 1, 6
             sed_1 = sed_1 + q_soil (phi, 2, irow1, j)
             sed_2 = sed_2 + q_soil (phi, 2, irow2, j)
             sed_3 = sed_3 + q_soil (phi, 2, irow3, j)
        enddo
       enddo
c
c  convert back from from fluxes (mm2/s * cell width [mm] * density [g/cm3] to
c     discharge in [kg/s]
c
       sed_1 = sed_1 * uc
       sed_2 = sed_2 * uc
       sed_3 = sed_3 * uc
c
c   calculate averages based on non-zero values
c
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
c
c  calculate sediment fluxes per unit width
c   - n.b. entire widths used because of splash
c   - / 1.0d3 converts from mm to m
c
       sed_1 = sed_1 / (dble (nc - 1) * dx / 1.0d3)
       sed_2 = sed_2 / (dble (nc - 1) * dx / 1.0d3)
       sed_3 = sed_3 / (dble (nc - 1) * dx / 1.0d3)
c
c  calculate sediment yields per unit area
c   - n.b. entire widths used because of splash
c   - / 1.0d3 converts from mm to m
c
       sedy_1 = sed_1 / (dble (irow1 - 1) * dx / 1.0d3)
       sedy_2 = sed_2 / (dble (irow2 - 1) * dx / 1.0d3)
       sedy_3 = sed_3 / (dble (irow3 - 1) * dx / 1.0d3)
c
c   normalize particle size fraction at outlet to sum to 1
c
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
c
c   output: time, rainfall, plot discharge, cross-section discharges, c-s depths, c-s velocities
c
       if (f_hydrofile) then
          write (57, '(12(e10.4, 1x))') t_out, rval,
     &                q_plot * dx, q_1 * dx, q_2 * dx, q_3 * dx,
     &                d_1, d_2, d_3, v_1, v_2, v_3
       endif
c
c   output: time, plot sediment output, plot sediment flux, c-s fluxes, plot yield per unit area, c-s yields per unit area, plot particle size outb
c
       if (f_sedfile) then
          write (58, '(16(e10.4, 1x))') t_out, sed_plot,
     &                               sed_plot / (plot_wid / 1.0d3),
     &                               sed_1, sed_2, sed_3,
     &                               sedy_plot / (plot_wid / 1.0d3),
     &                               sedy_1, sedy_2, sedy_3,
     &                               (ps_plot (phi), phi = 1, 6)
       endif
c
c     Output nutrient [NH4 NO3 P] concentrations in [mg/l], and then fluxes in [mg/s]
c         for plot and then cross sections
c
       if (f_nutrientfile) then
          write (60, '(25(e10.4,1x))') t_out, amm_plot,
     &                                 nit_plot, phos_plot,
     &                                 amm_plot * q_plot * dx * 1.e-6,
     &                                 nit_plot * q_plot * dx * 1.e-6,
     &                                 phos_plot * q_plot * dx * 1.e-6,
     &                                 amm_1, nit_1, phos_1,
     &                                 amm_1 * q_1 * dx * 1.e-6,
     &                                 nit_1 * q_1 * dx * 1.e-6,
     &                                 phos_1 * q_1 * dx * 1.e-6,
     &                                 amm_2, nit_2, phos_2,
     &                                 amm_2 * q_2 * dx * 1.e-6,
     &                                 nit_2 * q_2 * dx * 1.e-6,
     &                                 phos_2 * q_2 * dx * 1.e-6,
     &                                 amm_3, nit_3, phos_3,
     &                                 amm_3 * q_3 * dx * 1.e-6,
     &                                 nit_3 * q_3 * dx * 1.e-6,
     &                                 phos_3 * q_3 * dx * 1.e-6
       endif
       uc = dx * density * 1.e-6
c
c     detach_soil [kg/m2]
c
       if (f_seddetfile) then
          write (100,'(7(e10.4,1x))') t_out,
     &                        (det_soil_tot (phi) * uc, phi = 1, 6)
       endif
c
c     depos_soil in [kg/m2]
c
       if (f_seddeposfile) then
          write (101,'(7(e10.4,1x))') t_out,
     &                         (dpst_soil_tot (phi) * uc, phi = 1, 6)
       endif
c
c     soil depth [m]
c
       if (f_seddepthfile) then
          write (102,'(7(e10.4,1x))') t_out,
     &                         (dep_soil_tot (phi),  phi = 1, 6)
       endif
c
c     virtual velocity
c     ((nr - 1) * (nc - 1)) scales to number of cells but doesn't
c     account for cells with no flow
c
       if (f_sedvelfile) then
          write (103,'(7(e10.4,1x))') t_out,
     &                (vel_soil_tot (phi) /((nr - 1) * (nc - 1)),
     &                phi = 1, 6)
       endif
c
c     sediment discharge
c
cEva  uc: unit converter from fluxes (mm2/s * cell width [mm] * density [g/cm3] to
c     discharge in [kg/s]
       if (f_seddischfile) then
          write (104,'(7(e10.4,1x))') t_out,
     &                          (qs_tot (phi) * uc,  phi = 1, 6)
       endif
c
c     sediment fractions
c
       if (f_sedpropnfile) then
          write (105,'(7(e10.4,1x))') t_out,
     &                          (sed_frac_tot (phi),  phi = 1, 6)
       endif
c
cLTOct2007 output particulate-bound nutrient discharge (g) in each phi class
       if (f_p_nutfile) then
          write (55,'(37(e10.4,1x))') t_out,
     &                           (p_amm_plot (phi),  phi = 1, 6),
     &                           (p_nit_plot (phi),  phi = 1, 6),
     &                           (p_TN_plot (phi), phi = 1, 6),
     &                           (p_TP_plot (phi), phi = 1, 6),
     &                           (p_IC_plot (phi), phi = 1, 6),
     &                           (p_TC_plot (phi), phi = 1, 6)
       endif

c
c     sediment concentration
c
c     uc: unit converter to calculate the concentration of sediment in water flow
c     (q_soil*dx*10-6*density) / (q_water*dx*10-9)= 1000* density, sediment concentration in g/l
       uc = 1000.d0 * density
       if (f_sedconcfile) then
          write (106,'(7(e10.4,1x))') t_out,
     &                         (sed_conc_tot (phi) * uc, phi = 1, 6)
       endif
c
c   save maximum depth and velocity if peak discharge reached
c
       if (q_plot.gt.qsum_max) then
          qsum_max = q_plot
          do i = 1, nr1
             do j = 1, nc1
                dmax (i, j) = d (2, i, j)
              vmax (i, j) = v (i, j)
              dmax_soil (i, j) = 0.0d0
              vmax_soil (i, j) = 0.0d0
              do phi = 1, 6
                 dmax_soil (i, j) = dmax_soil (i, j) +
     &                                d_soil (phi, 2, i, j)
                 vmax_soil (i, j) = vmax_soil (i, j) +
     &                                v_soil (phi, i, j)
              enddo
           enddo
        enddo
       endif

       if (f_v_xsect) then
          write (107, '(1000(e10.4, 1x))') t_out,
     &                                  (v (irow1, j), j = 2, nc),
     &                                  (v (irow2, j), j = 2, nc),
     &                                  (v (irow3, j), j = 2, nc),
     &                                  dble (non_zero1),
     &                                  dble (non_zero2),
     &                                  dble (non_zero3)
       endif
       if (f_d_xsect) then
          write (108, '(1000(e10.4, 1x))') t_out,
     &                                  (d (2, irow1, j), j = 2, nc),
     &                                  (d (2, irow2, j), j = 2, nc),
     &                                  (d (2, irow3, j), j = 2, nc)
       endif
       if (f_q_xsect) then
          write (109, '(1000(e10.4, 1x))') t_out,
     &                             (q (2, irow1, j) * dx, j = 2, nc),
     &                             (q (2, irow2, j) * dx, j = 2, nc),
     &                             (q (2, irow3, j) * dx, j = 2, nc)
       endif
cJWMar2006
cJWMar2006  Output cross-section data for shrubland comparison assuming 0.61-m cells
cJWMar2006
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
       do j = 2, nc
          if (q (2, 21, j).gt.0.0d0) then
           non_zero_S1 = non_zero_S1 + 1
             d_S1 = d_S1 + d (2, 21, j)
           v_S1 = v_S1 + v (21, j)
           q_S1 = q_S1 + q (2, 21, j)
           do phi = 1, 6
              qs_S1 = qs_S1 + q_soil (phi, 2, 21, j)
           enddo
        endif
        if (q (2, 35, j).gt.0.0d0) then
           non_zero_S2 = non_zero_S2 + 1
             d_S2 = d_S2 + d (2, 35, j)
           v_S2 = v_S2 + v (35, j)
           q_S2 = q_S2 + q (2, 35, j)
           do phi = 1, 6
              qs_S2 = qs_S2 + q_soil (phi, 2, 35, j)
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

       if (f_slope) then
        write (99, '(9(e10.4, 1x), i2, 1x, i2)') t_out,
     &                d_S1, v_S1, q_S1, qs_S1, d_S2, v_S2, q_S2,
     &                qs_S2, non_zero_S1, non_zero_S2
       endif


cJWAug2005
cJWAug2005   output spatial hydro-, sedi- and nutrigraphs if required
cJWAug2005
       if (hydro_out) then
          if (f_qpointfile) then
             write (110, '(1000(e10.4, 1x))') t_out,
     &                    (q (2, x_hypoints (j), y_hypoints (j)) * dx,
     &                    j = 1, n_hypoints)
           do j = 1, n_hypoints
                sed_hypoints (j) = 0.0e0
           enddo
           uc = dx * density * 1.e-6
           do j = 1, n_hypoints
              do phi = 1, 6
                 sed_hypoints (j) = sed_hypoints (j) +
     &                 q_soil (phi, 2, x_hypoints (j), y_hypoints (j))
              enddo
              sed_hypoints (j) = sed_hypoints (j) * uc
           enddo
             if (f_qspointfile) then
                write (111, '(1000(e10.4, 1x))') t_out,
     &                      (sed_hypoints (j), j = 1, n_hypoints)
             endif
             if (f_nutpointfile) then
                write (112, '(1000(e10.4, 1x))') t_out,
     &               (nitrate (2, x_hypoints (j), y_hypoints (j)),
     &               ammonium (2, x_hypoints (j), y_hypoints (j)),
     &               phosphorus (2, x_hypoints (j), y_hypoints (j)),
     &               j = 1, n_hypoints)
             endif
          endif
       endif

       return
       end
