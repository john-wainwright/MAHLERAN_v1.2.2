module shared_data
save

double precision, allocatable :: slope (:,:), profc (:,:), planc (:,:)
integer*4, allocatable :: aspect (:,:), order (:,:), is_rill (:,:)
double precision, allocatable :: veg (:,:)
double precision, allocatable :: d (:,:,:), q (:,:,:)
double precision, allocatable :: d_soil (:,:,:,:), q_soil (:,:,:,:)
double precision, allocatable :: qsedin (:,:,:,:) !JWMar2017 for Crank-Nicolson for sediment flows
double precision, allocatable :: v_soil (:,:,:), add_soil (:,:,:)
double precision, allocatable :: detach_soil (:,:,:), depos_soil(:,:,:)
double precision, allocatable :: ammonium (:,:,:), nitrate (:,:,:), phosphorus (:,:,:)
double precision, allocatable :: amm_q (:,:,:), nit_q (:,:,:), TN_q (:,:,:)
double precision, allocatable :: TP_q (:,:,:), IC_q (:,:,:), TC_q (:,:,:)
double precision, allocatable :: addammonium (:,:), addnitrate (:,:), addphosphorus (:,:)
double precision, allocatable :: v (:,:),excess (:,:), cum_inf (:,:)
double precision, allocatable :: cum_drain (:,:), stmax (:,:), r2 (:,:)
double precision, allocatable :: add (:,:), theta (:,:), dup (:,:)
double precision, allocatable :: qin (:,:,:), v1 (:,:), qsum (:,:), dmax (:,:)
double precision, allocatable :: y_amm (:,:), y_nit (:,:), y_phos (:,:)
double precision, allocatable :: vmax (:,:)
double precision, allocatable :: ks (:,:), psi (:,:), ff (:,:), drain_par (:,:)
double precision, allocatable :: ksat (:,:), pave (:,:), rmask (:,:)
double precision, allocatable :: sed_propn (:,:,:), splash_tot (:,:)
double precision, allocatable :: sed_temp (:,:,:), sedch (:,:)
double precision, allocatable :: zs (:,:), sed_tot (:,:)
double precision, allocatable :: detach_tot (:,:), depos_tot (:,:), net_erosion_season (:,:)
double precision, allocatable :: z_change (:,:)
double precision, allocatable :: amm_tot (:,:), nit_tot (:,:), TN_tot (:,:)
double precision, allocatable :: TP_tot (:,:), IC_tot (:,:), TC_tot (:,:)
double precision, allocatable :: raindrop_detach_tot (:,:), flow_detach_tot (:,:)
double precision, allocatable :: dmax_soil (:,:), vmax_soil (:,:)
double precision, allocatable :: z (:,:), ps_init_ave (:,:,:)
double precision, allocatable :: area (:,:)
double precision, allocatable :: d_thresh (:,:)
double precision, allocatable :: theta_0 (:,:), theta_sat (:,:)
double precision, allocatable :: ciinit (:,:), sminit (:,:), cum_inf_p (:,:)
double precision, allocatable :: grav (:,:), fines (:,:)
double precision, allocatable :: MZ (:, :)  !cJCMay2011 for MiC
double precision, allocatable :: MXY (:, :) !cJCDec2011 for MiC
!double precision, allocatable :: MX_1 (:, :), MY_1 (:, :)  !cJCMay2011 for MiC Initial marker x&y coordinates (used for calculating net change in marker coverage)
double precision, allocatable :: motion_susp (:), vel_susp (:)  !cJCMay2011 for MiC
double precision, allocatable :: motion_bl (:), rest_bl (:), vel_bl (:) !cJCMay2011 for MiC
double precision, allocatable :: motion_diffuse (:), vel_diffuse (:)    !cJCMay2011 for MiC
double precision, allocatable :: detach_soil_m (:, :, :), detach_tot_m (:,:)    !cJCMay2011 for MiC
double precision, allocatable :: marker_runoff (:,:), marker_status (:, :)  !cJCMay2011 for MiC
double precision :: re, p_splash, randn_splash, p_trans !cJCMay2011 for MiC

!JW these variables are real for when they are needed externally for MATLAB access
real :: q_plot
real :: sed_plot

integer*4, allocatable :: contrib (:,:) , cover (:,:)
integer, allocatable :: x_hypoints (:), y_hypoints (:)
integer, allocatable :: status_bl (:)   !cJCMay2011 for MiC

character (len = 100) :: pfadin         !Name of input folder
character (len = 100) :: pfadout            !Name of output folder
integer pfadi                   !Length of input folder name
integer pfado                   !Length of output folder name
character (len = 180) :: hydrofile, sedfile, dischfile, nutrientfile
character (len = 180) :: seddetfile, seddeposfile, seddepthfile, sedvelfile
character (len = 180) :: seddischfile, sedpropnfile, sedconcfile
character (len = 180) :: p_nutfile
character (len = 180) :: topo_file
character (len = 180) :: veg_file
character (len = 180) :: ksat_file
character (len = 180) :: pavement_file
character (len = 180) :: rmask_file, rainfile, rain_asc_file ! cJCOct2010
character (len = 180) :: cover_file
character (len = 180) :: sm_file
character (len = 180) :: phi_1_file
character (len = 180) :: phi_2_file
character (len = 180) :: phi_3_file
character (len = 180) :: phi_4_file
character (len = 180) :: phi_5_file
character (len = 180) :: phi_6_file
character (len = 180) :: theta_sat_file
character (len = 180) :: qpointfile, qspointfile, nutpointfile
character (len = 180) :: ff_file
character (len = 180) :: inundfile  !cJCOct2010 for total inundated area
character (len = 180) :: MXYfile, dmapfile, mstatusfile !cJCMay2011 for MiC
character (len = 180) :: markerfile !cJCDec2011 for MiC

!flags for selection which output files will be created
logical :: f_hydrofile, f_sedfile, f_dischfile, f_nutrientfile, f_seddetfile, &
f_seddeposfile, f_seddepthfile, f_sedvelfile, f_seddischfile, f_sedpropnfile, &
f_sedconcfile, f_d_xsect, f_v_xsect, f_q_xsect, f_shrubland_xsect,f_p_nutfile, &
f_qpointfile, f_qspointfile, f_nutpointfile
logical :: f_thetafileimg, f_nutrifileimg, f_qfileimg, f_aspectfileimg, f_dfileimg,&
f_vfileimg, f_sedfileimg, f_detfileimg, f_depfileimg, f_soildepfileimg, f_soilvelfileimg, &
f_neterosimg, f_raindetimg, f_flowdetimg, f_p_nitrateimg, f_p_ammoniumimg,f_p_TNimg, &
f_p_TPimg, f_p_ICimg,f_p_TCimg, f_paramfile, f_ksat, f_pave, f_rainmask, f_topog, f_veg, &
f_contrib, f_order, f_slope
logical :: f_markerfileimg, f_markerofffile, f_mstatusfile, f_MXYfile, f_MXYfinalfile, f_dmapfile

double precision ke, grav_propn, p_par
double precision ksat_mod, psi_mod

integer ff_type
integer rain_type
integer phi
integer n_hypoints
integer nt_top_up
integer MiC !cJCMay2011 for MiC
integer iout

integer*4 mxcell, mycell, mi !cJCMay2011 for MiC
integer*4 mnum !cJCDec2011 for MiC
integer*4 seed, nit !cJCMay2011 for MiC

logical update_topography, reorder
logical hydro_out
double precision ksave, kssd, nomov
double precision psiave, psisd
double precision ffave, ffsd

parameter ( pi = 3.1415926535897932384626433832795d0,&
                    fourth = 1.3333333333333333333333333333333d0)

common / topog1 / ncell, ncell1, nc, nc1, nc2, nr, nr1, nr2

common / ctrl / dt, dtdx, dt2dx, dx_m, ndirn, &
               iter, iter_variable, iroute
common / params / ksave (4), psiave (4),    &
               ffave (4), kssd (4), psisd (4), ffsd (4), &
              soil_thick (4), drain_par_ave (4), drain_par_sd (4),&
      ps_init_sd (6), model_type,  &
        inf_type, ff_type, inf_model
common / rainfall / rf_mean, stormlength, &
                          rfvar1, rfvar2, rval, rain_type
common / rill_params / slpmin, slpmax, &
              prcmin, prcmax, plcmin, plcmax, &
               scamin, scamax, sdamin, sdamax, itype
common / file_data / doc_file, topo_file, &
                           veg_doc_file, veg_file, &
                           ksat_doc_file, ksat_file, &
                           rmask_doc_file, rmask_file,  &
                           pavement_doc_file, pavement_file,  &
                           rainfile, iout,  &
                           cover_doc_file, cover_file,  &
                           sm_doc_file, sm_file,  &
                           phi_1_doc_file, phi_1_file,  &
                           phi_2_doc_file, phi_2_file,  &
                           phi_3_doc_file, phi_3_file,  &
                           phi_4_doc_file, phi_4_file,  &
                           phi_5_doc_file, phi_5_file,  &
                           phi_6_doc_file, phi_6_file,  &
                          theta_sat_doc_file, theta_sat_file, &
                          ff_file
common / idrisi / xmin, xmax, ymin, ymax
common / nutrients / Cs (3), alpha (3), Rn (3)
common / p_nutrients / p_ammonium (6), p_nitrate (6),  &
                             p_TN (6), p_TP (6), p_IC (6), p_TC (6)
common / sed_params / sed_in_flow (6), hs (6), density, hz, &
                      excess_density, bagnold_density_scale

common / distributions / spa (6), spb (6), spc (6), spq (6),  &
                               nomov (6), spdist (6, 300), sma (6),  &
                               smb (6), redist (6, 300), radius (6), &
                               diameter (6), spd (6)

common / dlimits / nmax_splash (6), nmax_diffuse_flow (6),  &
                         nmax_conc_flow (6), nmax_susp_flow (6)
common / cell_size / dx, dy
common / sed_pars2 / detach (6), settling_vel (6), ke,  &
                           grav_propn, sigma, d50, ustar,  &
                           viscosity, p_par, dstar_const, phi
common / cell_locn / im, jm
common / calib / ksat_mod, psi_mod
common / hy_space / n_hypoints, qpointfile, qspointfile,  &
                          nutpointfile, hydro_out


data sma / 1.373d0, 34.52d0, 17.459d0, 6.572d0, 3.d0, 0.d0 /
data smb / .069d0, .091d0, .083d0, .081d0, .080d0, 0.06d0 /
data spq / 2.72d0, 1.61d0, .92d0, .85d0, .75d0, .30d0 /
data radius / 3.125d-5, 7.1825d-5, 1.875d-4, 6.25d-4, 3.5d-3, 1.2d-2/
data viscosity / 1.003d-6 /



end module shared_data
