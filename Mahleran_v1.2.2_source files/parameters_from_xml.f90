module parameters_from_xml
save

character (len = 80) :: model_name
character (len = 80) :: model_version
character (len = 80) :: model_run_type
character (len = 100) :: input_folder
character (len = 100) :: output_folder
character (len = 100) :: output_points 
character (len = 80) :: dem_file 
character (len = 80) :: vegetation_cover_map
character (len = 80) :: rainfall_data
character (len = 80) :: final_infiltration_map
character (len = 80) :: suction_map
character (len = 80) :: rainfall_scaling_map
character (len = 80) :: pavement_map
character (len = 80) :: surface_type_map
character (len = 80) :: drainage_map_map
character (len = 80) :: initial_soil_moisture_map 
character (len = 80) :: saturated_soil_moisture_map
character (len = 80) :: friction_factor_map 
character (len = 1) :: update_topography_xml
character (len = 80), dimension (1:6) :: particle_size_map 
character (len = 80) :: interstorm_input_folder
character (len = 80) :: continuous_temperature_data
character (len = 80) :: continuous_rainfall_data
character (len = 80) :: one_min_rainfall_data
character (len = 80) :: shrub_cover_map
character (len = 80) :: marker_file
character (len = 13) :: final_infiltration_rate_distribution
character (len = 13) :: wetting_front_suction_distribution
character (len = 13) :: drainage_parameter_distribution
character (len = 13) :: friction_factor_distribution
character (len = 13) :: initial_soil_moisture_distribution
character (len = 13) :: saturated_soil_moisture_distribution
character (len = 13) :: particle_size_distribution
integer :: model_type
integer :: infiltration_parameter_type
integer :: drainage_parameter_type
integer :: infiltration_model
integer :: friction_factor_type 
integer :: rain_type_xml
integer :: flow_direction
integer :: flow_routing_solution_method
integer :: sediment_routing_solution_method
integer :: itype
integer :: flow_routing
integer :: start_year
integer :: end_year
integer :: start_month
integer :: end_month
integer :: input_folder_length
integer :: output_folder_length
integer :: interstorm_input_folder_length
integer :: n_types
integer :: KE_model_type  !added by JW FEb 2017 for COST modelling comparison
double precision, dimension (10) :: final_infiltration_rate_mean      !controlled by number of surface types -- 10 as maximum
double precision, dimension (10) :: final_infiltration_rate_std_dev
double precision, dimension (10) :: wetting_front_suction_mean
double precision, dimension (10) :: wetting_front_suction_std_dev
double precision, dimension (10) :: drainage_parameter_mean
double precision, dimension (10) :: drainage_parameter_std_dev
double precision, dimension (10) :: initial_soil_moisture_mean
double precision, dimension (10) :: initial_soil_moisture_std_dev
double precision, dimension (10) :: saturated_soil_moisture_mean
double precision, dimension (10) :: saturated_soil_moisture_std_dev
double precision, dimension (10) :: soil_thickness 
double precision, dimension (10) :: friction_factor_mean
double precision, dimension (10) :: friction_factor_std_dev
double precision, dimension (10, 6) :: mean_particle_size 
double precision, dimension (10, 6) :: std_dev_particle_size
double precision, dimension (6) :: Raindrop_detachment_a_parameter_size
double precision, dimension (6) :: Raindrop_detachment_b_parameter_size
double precision, dimension (6) :: Raindrop_detachment_c_parameter_size 
double precision, dimension (6) :: Raindrop_detachment_d_parameter_size 
double precision, dimension (6) :: Raindrop_detachment_max_parameter_size
double precision, dimension (6) :: Particulate_bound_NH4
double precision, dimension (6) :: Particulate_bound_NO3
double precision, dimension (6) :: Particulate_bound_TN 
double precision, dimension (6) :: Particulate_bound_TP 
double precision, dimension (6) :: Particulate_bound_IC 
double precision, dimension (6) :: Particulate_bound_TC 
double precision :: stormlength_xml
double precision :: mean_rainfall
double precision :: std_dev_rainfall
double precision :: skewness_rainfall
double precision :: topography_update_interval
double precision :: time_step
double precision :: particle_density
double precision :: active_layer_sensitivity
double precision :: mass_transfer_coefficient_NH4
double precision :: mass_transfer_coefficient_NO3
double precision :: mass_transfer_coefficient_PO
double precision :: Cs_NH4
double precision :: Cs_NO3
double precision :: Cs_PO
double precision :: rainfall_dissolved_NH4_conc
double precision :: rainfall_dissolved_NO3_conc
double precision :: rainfall_dissolved_PO_conc
double precision :: threshold_rain_xml
double precision :: latitude_xml
double precision :: latitude_degrees
double precision :: depth_of_upper_layer
double precision :: depth_of_lower_layer
double precision :: residual_water_content
double precision :: water_content_for_stomatal_closure
double precision :: crusting_evapotranspiration_reduction
double precision :: infiltration_from_bare_to_layer_2
double precision :: infiltration_from_shrub_to_layer_2
double precision :: ksat_xml
double precision :: diffusion_coefficient
double precision :: initial_value_for_sm_1
double precision :: initial_value_for_sm_2
double precision :: within_storm_interval
double precision :: nodata_value_from_topog
logical :: use_final_infiltration_map 
logical :: use_suction_map
logical :: use_drainage_map
logical :: use_friction_factor_map
logical :: use_initial_soil_moisture_map
logical :: use_saturated_soil_moisture_map
logical :: use_map_phi  
logical :: aspectfileimg
logical :: contrib_xml
logical :: d_xsect
logical :: depfileimg
logical :: detfileimg
logical :: dfileimg
logical :: dischfile_xml
logical :: dmapfile_xml
logical :: flowdetimg
logical :: hydrofile_xml
logical :: hypointsfile
logical :: inundfile
logical :: ksat_output
logical :: markerfileimg_xml
logical :: markerofffile_xml
logical :: mstatusfile_xml
logical :: MXYfile_xml
logical :: neterosimg
logical :: nutpointfile_xml
logical :: nutrientfile_xml
logical :: nutrifileimg
logical :: nitratefileimg
logical :: phosfileimg
logical :: order_xml
logical :: p_ammoniumimg
logical :: p_ICimg
logical :: p_nitrateimg
logical :: p_nutfile_xml
logical :: p_TCimg
logical :: p_TNimg
logical :: p_TPimg
logical :: paramfile
logical :: pave_xml
logical :: q_xsect
logical :: qfileimg
logical :: qpointfile_xml
logical :: qspointfile_xml
logical :: raindetimg
logical :: rainmask
logical :: sedconcfile_xml
logical :: seddeposfile_xml
logical :: seddepthfile_xml
logical :: seddetfile_xml
logical :: seddischfile_xml
logical :: sedfile_xml
logical :: sedfileimg
logical :: sedpropnfile_xml
logical :: sedvelfile_xml
logical :: shrubland_xsect
logical :: slope_xml
logical :: soildepfileimg
logical :: soilvelfileimg
logical :: thetafileimg_xml
logical :: topog
logical :: v_xsect
logical :: veg_xml 
logical :: vfileimg
logical :: verbose_output
logical :: within_storm_output

end module parameters_from_xml

