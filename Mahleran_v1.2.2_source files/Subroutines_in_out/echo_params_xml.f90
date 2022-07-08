!****************************************************************
!  subroutine to echo input parameters to a given unit
!****************************************************************
subroutine echo_params_xml (iunit)

use shared_data
use parameters_from_xml

implicit none

integer *4 iunit
integer *4 time_array (8)
integer *4 i

!JWMar09 Change to use f90 intrinsic call to date_and_time subroutine
! time_array(1)    year
! time_array(2)    month of the year
! time_array(3)    day of the month
! time_array(4)    time offset with respect to UTC in minutes
! time_array(5)    hour of the day
! time_array(6)    minutes of the hour
! time_array(7)    seconds of the minute
! time_array(8)    milliseconds of the second

!       character *8 time, edate
!
!  these are Compaq FORTRAN routines
!
!       character *8 CLOCK, DATE
!
!  these are Salford FORTRAN routines
!
!       character *8 time@, edate@
!
!
!   echo parameters
!
!   REB changed second character statement to etime edate from time@ and edate@
!
!
!   function time@() is SALFORD FORTRAN-specific function giving current time
!      as hh:mm:ss
!   function edate@() is SALFORD FORTRAN-specific function giving current date
!      as dd/mm/yy
!
!   REB changed time@ to time and edate@ to date
!   and wrote etime edate instead of time@ and edate@
!
!       time = 'hh:mm:ss'
!       edate = 'dd/mm/yy'
!       write (iunit, 9999) time, edate
!       write (iunit, 9999) time@(), edate@()
!
!   JW changed to Compaq Fortran subroutines
!
!       time = CLOCK ()
!    edate = DATE ()
!       write (iunit, 9999) time, edate
call date_and_time (values = time_array)
write (iunit, 9999) time_array (5), time_array (6), time_array (7), time_array (3), time_array (2), time_array (1)
write (iunit, 9946) model_name
write (iunit, *) 'Input folder: ', input_folder (1: input_folder_length)
write (iunit, *) 'Output folder: ', output_folder (1:output_folder_length)
!
!write (iunit, *) 'nt_top_up =',nt_top_up
!
write (iunit, 9998) input_folder (1:input_folder_length) // dem_file
if (inf_type.eq.1) then
   write (iunit, 9997) 'deterministic using pavement map and mean rainfall intensity '
   write (iunit, 9994) (psiave (i), i = 1, n_types)
   write (iunit, 9975) (drain_par_ave (i), i = 1, n_types)
elseif (inf_type.eq.2) then
   write (iunit, 9997) 'deterministic or stochastic using parameters from input file '
   write (iunit, 9996) (ksave (i) * 3600, i = 1, n_types)
   write (iunit, 9995) (kssd (i), i = 1, n_types)
   write (iunit, 9994) (psiave (i), i = 1, n_types)
   write (iunit, 9993) (psisd (i), i = 1, n_types)
   write (iunit, 9975) (drain_par_ave (i), i = 1, n_types)
   write (iunit, 9974) (drain_par_sd (i), i = 1, n_types)
elseif (inf_type.eq.3) then
   write (iunit, 9967) pavement_file
   write (iunit, 9994) (psiave (i), i = 1, n_types)
   write (iunit, 9975) (drain_par_ave (i), i = 1, n_types)
elseif (inf_type.eq.4) then
   write (iunit, 9997) 'deterministic using ksat map    '
   write (iunit, 9994) (psiave (i), i = 1, n_types)
   write (iunit, 9975) (drain_par_ave (i), i = 1, n_types)
endif
if (inf_model.eq.1) then
   write (iunit, 9964), input_folder (1:input_folder_length) // final_infiltration_map
else
   write (iunit, 9963)
endif
!LTOct07 changed
!LTOct07 write (iunit, 9976) (theta_0 (i), i = 1, n_types) to
write (iunit, 9976) input_folder (1:input_folder_length) // initial_soil_moisture_map
!LTOct07 changed
!        write (iunit, 9992) (theta_sat (i), i = 1, n_types) to
write (iunit, 9973) input_folder (1:input_folder_length) // saturated_soil_moisture_map
write (iunit, 9991) (soil_thick (i), i = 1, n_types)
if (ff_type.eq.1) then
   write (iunit, 9990) 'deterministic'
   write (iunit, 9989) (ffave (i), i = 1, n_types)
elseif (ff_type.eq.2) then
   write (iunit, 9990) 'stochastic   '
   write (iunit, 9989) (ffave (i), i = 1, n_types)
   write (iunit, 9988) (ffsd (i), i = 1, n_types)
elseif (ff_type.eq.3) then
   write (iunit, 9990) 'deterministic with feedback based on flow depth (Scoging et al., 1992)'
elseif (ff_type.eq.4) then
   write (iunit, 9990) 'deterministic with feedback based on Reynolds number (Abrahams et al., 1995)'
elseif (ff_type.eq.5) then
   write (iunit, 9990) 'deterministic with feedback based on Reynolds number (MODIFIED Abrahams et al., 1995)'
elseif (ff_type.eq.6) then
   write (iunit, 9990) 'deterministic with feedback based on Reynolds number^0.33'
elseif (ff_type.eq.7) then
   write (iunit, 9990) 'deterministic with feedback based on Discharge (Abrahams et al., 1996)'
elseif (ff_type.eq.8) then
   write (iunit, 9947) input_folder (1:input_folder_length) // friction_factor_map
endif
write (iunit, 9955) input_folder (1:input_folder_length) // vegetation_cover_map
if (rain_type.eq.1) then
   write (iunit, 9987) rf_mean, stormlength
else
   write (iunit, 9986) rainfile
endif
!CJMHJul13 added name of rmask_file to output:
write (iunit, 9978) input_folder (1:input_folder_length) // rainfall_scaling_map
write (iunit, 9985) ndirn
if (iroute.eq.1) then
   write (iunit, 9931)
elseif (iroute.eq.2) then
!CJMHFeb13 Crank-Nicolson with Newton-Raphson solution added
   write (iunit, 9932)
elseif (iroute.eq.3) then
!CJMHMar13 Lax-Wendroff method removed
   write (iunit, 9933)
   stop
elseif (iroute.eq.4) then
!CJMHMar13 Two-step Lax-Wendroff method removed
   write (iunit, 9934)
   stop
elseif (iroute.eq.5) then
   write (iunit, 9935)
elseif (iroute.eq.6) then
   write (iunit, 9936)
elseif (iroute.eq.7) then
!CJMHFeb13 C-N method with Newton-Raphson solution - test version
   write (iunit, 9937)
endif
if (sediment_routing_solution_method.eq.1) then
!JW2017 Use original explicit solution for sediment routing
   write (iunit, 9929)
elseif (sediment_routing_solution_method.eq.2) then
   write (iunit, 9930)
endif
if (update_topography) then
   write (iunit, 9940)
   write (iunit, 9942) nt_top_up
else
   write (iunit, 9941)
endif
write (iunit, 9984) dt
write (iunit, 9966) density, hz
!       write (iunit, 9965) (ps_init_ave (phi), phi = 1, 6)
write (iunit, 9962) (Cs (i), i = 1, 3)
write (iunit, 9961) (alpha (i), i = 1, 3)
write (iunit, 9960) (spa (i), i = 1, 6)
write (iunit, 9959) (spb (i), i = 1, 6)
write (iunit, 9958) (spc (i), i = 1, 6)
write (iunit, 9957) (hs (i), i = 1, 6)
write (iunit, 9956) ksat_mod, psi_mod
!LTOct2007 added
write (iunit, 9954) (p_ammonium (i), i = 1, 6)
write (iunit, 9953) (p_nitrate (i), i = 1, 6)
write (iunit, 9952) (p_TN (i), i = 1, 6)
write (iunit, 9951) (p_TP (i), i = 1, 6)
write (iunit, 9950) (p_IC (i), i = 1, 6)
write (iunit, 9949) (p_TC (i), i = 1, 6)
write (iunit, 9948) (Rn (i), i = 1, 3)

9999   format (' ---------------------------------------------------',/&
               ' |               MAHLERAN  v1.2.2                  |',/&
               ' |      code (c) J. Wainwright, E.N. Mueller,      |',/&
               ' |    L. Turnbull, R.E. Brazier, J.R. Cooper,      |',/&
               ' |       C.J.M. Hewett, B. Fenti, B. Tjetien       |',/&
               ' |                  1997 - 2015                    |',/&
               ' | contact John.Wainwright@dur.ac.uk for details   |',/&
               ' |      Output time ', i2, ':', i2, ':', i2, ' on ', &
                                   i2, '/', i2, '/', i4, '         |',/&
               ' ---------------------------------------------------',/)
9998   format (' Topography read in from ', a80)
9997   format (' Infiltration parameter type: ', a32)
9996   format (' Mean Ksat: ', e10.4, ' mm/h')
9995   format (' Standard deviation of Ksat: ', e10.4)
9994   format (' Mean wetting-front suction: ', e10.4, ' mm')
9993   format (' Standard deviation of wetting-front suction: ', e10.4, ' mm')
9992   format (' Theta_sat read in from file: ', a80)
9991   format (' Soil thickness: ', e10.4, ' m')
9990   format (' Friction factor parameter type: ', a20)
9989   format (' Mean friction factor: ', e10.4)
9988   format (' Standard deviation of friction factor: ', e10.4)
9987   format (' Rainfall model: 1. constant average rainfall ', ' of ', f7.2, ' mm/h for ', f7.2, ' s ')
9986   format (' Rainfall model: 2. defined in file: ', a80)
9985   format (' Flow routing using ', i1, ' flow directions ')
9984   format (' Storm timestep: ', f7.2, ' s ')
9978   format (' Rainfall mask data read in from file: ', a80)
9976   format (' Initial soil moisture read in from file: ', a80)
9973   format (' Saturated soil moisture read in from file: ', a80)
9975   format (' Mean drainage parameter: ', e10.4, ' mm/h')
9974   format (' S.d. drainage parameter: ', e10.4, ' mm/h')
9929   format (' Sediment routing using Euler Method ')
9930   format (' Sediment routing using Crank-Nicolson with Newton-Raphson solution ')
9931   format (' Flow routing using Scoging Method ')
9932   format (' Flow routing using Crank-Nicolson with Newton-Raphson solution ')
9933   format (' Lax-Wendroff method (removed)')
9934   format (' Two-step Lax-Wendroff method (removed)')
9935   format (' Flow routing using Crank-Nicolson method with bisection solution')
9936   format (' Flow routing using Crank-Nicolson method with bisection solution & dynamic overtopping of sinks')
9937   format (' Flow routing using Crank-Nicolson method with Newton-Raphson solution (test version)')
9940   format (' With topography updating ')
9941   format (' Without topography updating ')
9942   format ( I10,' time steps between topography updates')
9967   format (' Infiltration parameter type: deterministic as dynamic function of rainfall intensity, using ', &
                 'pavement file ', a80)
9966   format (' Particle density: ', f7.2, ' g/m3',/ ' Active layer sensitivity: ', f7.4)
9965   format (' Initial particle size fractions:', /, 33x, ' phi(1) = ', f5.3, /, 33x, ' phi(2) = ', f5.3, /, &
               33x, ' phi(3) = ', f5.3, /, 33x, ' phi(4) = ', f5.3, /, 33x, ' phi(5) = ', f5.3, /, 33x, ' phi(6) = ', f5.3, /)
9964   format (' Smith and Parlange (1978) infiltration model with ksat as parameterized in input file ', a120)
9963   format (' Smith and Parlange (1978) infiltration model with ksat derived from Hawkins exponential based on ',&
                'pavement cover and rainfall intensity ')
9962   format (' Nutrient parameter CS : ammonium, ', e10.4, '; nitrate, ', e10.4, '; phosphorus, ' e10.4)
9961   format (' Mass-transfer coefficients : ammonium, ', e10.4, '; nitrate, ', e10.4, '; phosphorus, ' e10.4)
9960   format (' Raindrop detachment a parameter:', 6 (1x, e10.4))
9959   format (' Raindrop detachment b parameter:', 6 (1x, e10.4))
9958   format (' Raindrop detachment c parameter:', 6 (1x, e10.4))
9957   format (' Raindrop detachment maximum parameter:', 6 (1x, e10.4))
9956   format (' Ksat multiplication factor: ', e10.4, / ' Psi  multiplication factor: ', e10.4)
9955   format (' Vegetation read in from file: ', a80)
!LTOct2007 added
9954   format (' particulate-bound ammonium conc:', 6 (1x, e10.4))
9953   format (' particulate-bound nitrate conc:', 6 (1x, e10.4))
9952   format (' particulate-bound TN conc:', 6 (1x, e10.4))
9951   format (' particulate-bound TP conc:', 6 (1x, e10.4))
9950   format (' particulate-bound IC conc:', 6 (1x, e10.4))
9949   format (' particulate-bound TC conc:', 6 (1x, e10.4))
9948   format (' Soluble rainfall nutrients : ammonium, ', e10.4, '; nitrate, ', e10.4, '; phosphorus, ' e10.4)
9947   format (' Friction factor in file: ', a80)
9946   format (' ---------------------------------------------------',/ / &
               ' Run: ',/ /&
               ' ', a80,/ / &
               ' ---------------------------------------------------',/)

return
end
