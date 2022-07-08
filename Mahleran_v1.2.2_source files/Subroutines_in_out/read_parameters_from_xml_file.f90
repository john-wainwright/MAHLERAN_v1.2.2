subroutine read_parameters_from_xml_file (fname)

!
! first version written by J. Wainwright May 2014
! for Mahleran v.1.2.0 and up -- reads standard format data from xml file (produced by Gustav interface)
!

use xmlparse
use parameters_from_xml
use vegdynamics_shared_data

implicit none

integer :: ierr
integer :: k, layer

! variables for reading in from xml file
character (len = 80) :: fname
logical :: mustread
type (XML_PARSE) :: info
character (len = 100) :: tag
logical :: endtag
character (len = 100), dimension (1:2, 1:20) :: attribs
integer :: no_attribs
character (len = 200), dimension (1:100) :: data
integer :: no_data
integer :: i
integer :: j
integer :: ifile
!local variables to this subroutine to store parameters before passing them on to relevant shared data modules

n_types = 1
ifile = 18

!set no_species to initial value for error checking
no_species = -9999

!
!  give this file a name so it's not hidden in fort.18
!
open (ifile, FILE='current_parameters_from_xml.dat', status = 'unknown')
rewind (ifile)


! Assign the xml-filename to fname and open the file
mustread = .true.
write (6, *) ' calling xml_open'
call xml_open(info, fname, mustread)
! Check for errors
if (xml_error (info)) then
    ! handle the errors
    write (6, *)
    write (6, *) 'ERROR opening ', trim (fname)
    write (6, *) 'Stopping'
    write (6, *)
else
    ! Start reading the file
    write (6, *) 'Start reading the file'
    call xml_options(info,ignore_whitespace = .true.)
    do
        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
        if (xml_error(info)) then
        ! handle the errors
           write (6, *)
           write (6, *) 'ERROR opening ', trim (fname)
           write (6, *) 'Stopping'
           write (6, *)
        endif
!        write (*,*) 'tag: ', trim(tag), endtag
!        do i = 1, no_attribs
!            write(*,*) 'attrib: ', i,'>', trim(attribs(1, i)), '<=', trim(attribs(2,i))
!        enddo
!        write(*,*) ('data element: ', i, '>' , trim (data(i)), '<', i = 1, no_data)
        select case (trim (tag))
            case ('mahleran_input')
                if (.not.endtag) then
                    write (ifile, *) '------------------------------------------------------------------------------'
                    model_name = trim (attribs (2, 1))
                    write (ifile, *) 'Starting to read Mahleran input file: ', model_name
                else
                    write (ifile, *) 'End of Mahleran input file'
                    write (ifile, *) '------------------------------------------------------------------------------'
                endif
            case ('version')
                model_version = trim (attribs (2, 1))
                write (ifile, *) 'File for model version: ', model_version
            case ('runtype')
                model_run_type = trim (attribs (2, 1))
                write (ifile, *) 'Model run type: ', model_run_type
            case ('input_folder')
                input_folder = trim (attribs (2, 1))
                write (ifile, *) 'Input folder: ', input_folder
            case ('output_folder')
                output_folder = trim (attribs (2, 1))
                write (ifile, *) 'Output folder: ', output_folder
            case ('output_points')
                output_points = trim (attribs (2, 1))
                write (ifile, *) 'Output points: ', output_points
            case ('dem')
                dem_file = trim (attribs (2, 1))
                write (ifile, *) 'DEM file: ', dem_file
            case ('vegetation-cover_map')
                vegetation_cover_map = trim (attribs (2, 1))
                write (ifile, *) 'Vegetation-cover map: ', vegetation_cover_map
            case ('rainfall_data')
                rainfall_data = trim (attribs (2, 1))
                write (ifile, *) 'Rainfall data: ', rainfall_data
            case ('final_infiltration_map')
                final_infiltration_map = trim (attribs (2, 1))
                write (ifile, *) 'Final_infiltration map: ', final_infiltration_map
            case ('suction_map')
                suction_map = trim (attribs (2, 1))
                write (ifile, *) 'Suction map: ', suction_map
            case ('rainfall-scaling_map')
                rainfall_scaling_map = trim (attribs (2, 1))
                write (ifile, *) 'Rainfall-scaling map: ', rainfall_scaling_map
            case ('pavement_map')
                pavement_map = trim (attribs (2, 1))
                write (ifile, *) 'Pavement map: ', pavement_map
            case ('surface-type_map')
                surface_type_map = trim (attribs (2, 1))
                write (ifile, *) 'Surface-type map: ', surface_type_map
            case ('drainage-map_map')
                drainage_map_map = trim (attribs (2, 1))
                write (ifile, *) 'Drainage-map map: ', drainage_map_map
            case ('model_type')
                read (attribs (2, 1), *) model_type
                write (ifile, *) 'Model type: ', model_type
            case ('infiltration-parameter_type')
                read (attribs (2, 1), *) infiltration_parameter_type
                write (ifile, *) 'Infiltration-parameter type: ', infiltration_parameter_type
            case ('drainage-parameter_type')
                read (attribs (2, 1), *) drainage_parameter_type
                write (ifile, *) 'Drainage-parameter type: ', drainage_parameter_type
            case ('infiltration_model')
                read (attribs (2, 1), *) infiltration_model
                write (ifile, *) 'Infiltration model: ', infiltration_model
            case ('number_of_surface_types')
                read (attribs (2, 1), *) n_types
                write (ifile, *) 'Number of surface types: ', n_types
                if (n_types.gt.10) then
                    write (ifile, *) 'Error in input file: specified ', n_types, &
                                     ' surface types, but there should be no more than 10'
                    stop
                endif
            case ('final_infiltration_rate_mean')
                if (.not.endtag) then
                    write (ifile, *) 'Final infiltration rate mean: ', trim (attribs (2, 1))
                    do i = 1, n_types
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                        read (data (1), *) final_infiltration_rate_mean (i)
                        write (ifile, *) 'Type ', i, ': ', final_infiltration_rate_mean (i)
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                    enddo
                else
                    write (ifile, *) 'Completed reading final infiltration rate mean'
                endif
            case ('final_infiltration_std_dev')
                if (.not.endtag) then
                    write (ifile, *) 'Final infiltration rate std dev: ', trim (attribs (2, 1))
                    do i = 1, n_types
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                        read (data (1), *) final_infiltration_rate_std_dev (i)
                        write (ifile, *) 'Type ', i, ': ', final_infiltration_rate_std_dev (i)
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                    enddo
                else
                    write (ifile, *) 'Completed reading final infiltration rate std dev'
                endif
            case ('wetting_front_suction_mean')
                if (.not.endtag) then
                    write (ifile, *) 'Wetting-front suction mean: ', trim (attribs (2, 1))
                    do i = 1, n_types
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                        read (data (1), *) wetting_front_suction_mean (i)
                        write (ifile, *) 'Type ', i, ': ', wetting_front_suction_mean (i)
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                    enddo
                else
                    write (ifile, *) 'Completed reading wetting-front suction mean'
                endif
            case ('wetting_front_suction_std_dev')
                if (.not.endtag) then
                    write (ifile, *) 'Wetting-front suction std dev: ', trim (attribs (2, 1))
                    do i = 1, n_types
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                        read (data (1), *) wetting_front_suction_std_dev (i)
                        write (ifile, *) 'Type ', i, ': ', wetting_front_suction_std_dev (i)
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                    enddo
                else
                    write (ifile, *) 'Completed reading wetting-front suction std dev'
                endif
            case ('drainage_parameter_mean')
                if (.not.endtag) then
                    write (ifile, *) 'Drainage parameter mean: ', trim (attribs (2, 1))
                    do i = 1, n_types
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                        read (data (1), *) drainage_parameter_mean (i)
                        write (ifile, *) 'Type ', i, ': ', drainage_parameter_mean (i)
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                    enddo
                else
                    write (ifile, *) 'Completed reading drainage parameter mean'
                endif
            case ('drainage_parameter_std_dev')
                if (.not.endtag) then
                    write (ifile, *) 'Drainage parameter std dev: ', trim (attribs (2, 1))
                    do i = 1, n_types
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                        read (data (1), *) drainage_parameter_std_dev (i)
                        write (ifile, *) 'Type ', i, ': ', drainage_parameter_std_dev (i)
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                    enddo
                else
                    write (ifile, *) 'Completed reading drainage parameter std dev'
                endif
            case ('initial_soil_moisture_mean')
                if (.not.endtag) then
                    write (ifile, *) 'Initial soil moisture mean: ', trim (attribs (2, 1))
                    do i = 1, n_types
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                        read (data (1), *) initial_soil_moisture_mean (i)
                        write (ifile, *) 'Type ', i, ': ', initial_soil_moisture_mean (i)
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                    enddo
                else
                    write (ifile, *) 'Completed reading initial soil moisture mean'
                endif
            case ('initial_soil_moisture_std_dev')
                if (.not.endtag) then
                    write (ifile, *) 'Initial soil moisture std dev: ', trim (attribs (2, 1))
                    do i = 1, n_types
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                        read (data (1), *) initial_soil_moisture_std_dev (i)
                        write (ifile, *) 'Type ', i, ': ', initial_soil_moisture_std_dev (i)
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                    enddo
                else
                    write (ifile, *) 'Completed reading initial soil moisture std dev'
                endif
            case ('saturated_soil_moisture_mean')
                if (.not.endtag) then
                    write (ifile, *) 'Saturated soil moisture mean: ', trim (attribs (2, 1))
                    do i = 1, n_types
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                        read (data (1), *) saturated_soil_moisture_mean (i)
                        write (ifile, *) 'Type ', i, ': ', saturated_soil_moisture_mean (i)
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                    enddo
                else
                    write (ifile, *) 'Completed reading saturated soil moisture mean'
                endif
            case ('saturated_soil_moisture_std_dev')
                if (.not.endtag) then
                    write (ifile, *) 'Saturated soil moisture std dev: ', trim (attribs (2, 1))
                    do i = 1, n_types
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                        read (data (1), *) saturated_soil_moisture_std_dev (i)
                        write (ifile, *) 'Type ', i, ': ', saturated_soil_moisture_std_dev (i)
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                    enddo
                else
                    write (ifile, *) 'Completed reading saturated soil moisture std dev'
                endif
            case ('soil_thickness')
                if (.not.endtag) then
                    write (ifile, *) 'Soil thickness: ', trim (attribs (2, 1))
                    do i = 1, n_types
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                        read (data (1), *) soil_thickness (i)
                        write (ifile, *) 'Type ', i, ': ', soil_thickness (i)
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                    enddo
                else
                    write (ifile, *) 'Completed reading soil thickness'
                endif
            case ('friction_factor_mean')
                if (.not.endtag) then
                    write (ifile, *) 'Friction factor mean: ', trim (attribs (2, 1))
                    do i = 1, n_types
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                        read (data (1), *) friction_factor_mean (i)
                        write (ifile, *) 'Type ', i, ': ', friction_factor_mean (i)
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                    enddo
                else
                    write (ifile, *) 'Completed reading friction factor mean'
                endif
            case ('friction_factor_std_dev')
                if (.not.endtag) then
                    write (ifile, *) 'Friction factor std dev: ', trim (attribs (2, 1))
                    do i = 1, n_types
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                        read (data (1), *) friction_factor_std_dev (i)
                        write (ifile, *) 'Type ', i, ': ', friction_factor_std_dev (i)
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                    enddo
                else
                    write (ifile, *) 'Completed reading friction factor std dev'
                endif
            case ('initial_soil-moisture_map')
                initial_soil_moisture_map = trim (attribs (2, 1))
                write (ifile, *) 'Initial_soil-moisture_map: ', initial_soil_moisture_map
            case ('saturated_soil-moisture_map')
                saturated_soil_moisture_map = trim (attribs (2, 1))
                write (ifile, *) 'Saturated_soil-moisture_map: ', saturated_soil_moisture_map
            case ('friction_factor_type')
                read (attribs (2, 1), *) friction_factor_type
                write (ifile, *) 'Friction_factor_type: ', friction_factor_type
            case ('friction_factor_map')
                friction_factor_map = trim (attribs (2, 1))
                write (ifile, *) 'Friction_factor_map: ', friction_factor_map
            case ('use_final_infiltration_map')
                read (attribs (2, 1), *) use_final_infiltration_map
                write (ifile, *) 'Use_final_infiltration_map: ', use_final_infiltration_map
            case ('use_suction_map')
                read (attribs (2, 1), *) use_suction_map
                write (ifile, *) 'Use_suction_map: ', use_suction_map
            case ('use_drainage_map')
                read (attribs (2, 1), *) use_drainage_map
                write (ifile, *) 'Use_drainage_map: ', use_drainage_map
            case ('use_friction_factor_map')
                read (attribs (2, 1), *) use_friction_factor_map
                write (ifile, *) 'Use_friction_factor_map: ', use_friction_factor_map
            case ('use_initial_soil_moisture_map')
                read (attribs (2, 1), *) use_initial_soil_moisture_map
                write (ifile, *) 'Use_initial_soil_moisture_map: ', use_initial_soil_moisture_map
            case ('use_saturated_soil_moisture_map')
                read (attribs (2, 1), *) use_saturated_soil_moisture_map
                write (ifile, *) 'Use_saturated_soil_moisture_map: ', use_saturated_soil_moisture_map
            case ('finalInfiltrationRateDistribution')
                read (attribs (2, 1), *) final_infiltration_rate_distribution
                write (ifile, *) 'Final_infiltration_rate_distribution: ', final_infiltration_rate_distribution
            case ('wettingFrontSuctionDistribution')
                read (attribs (2, 1), *) wetting_front_suction_distribution
                write (ifile, *) 'WettingFrontSuctionDistribution: ', wetting_front_suction_distribution
            case ('drainageParameterDistribution')
                read (attribs (2, 1), *) drainage_parameter_distribution
                write (ifile, *) 'Drainage_parameter_distribution: ', drainage_parameter_distribution
            case ('frictionFactorDistribution')
                read (attribs (2, 1), *) friction_factor_distribution
                write (ifile, *) 'Friction_factor_distribution: ', friction_factor_distribution
            case ('initialSoilMoistureDistribution')
                read (attribs (2, 1), *) initial_soil_moisture_distribution
                write (ifile, *) 'Initial_soil_moisture_distribution: ', initial_soil_moisture_distribution
            case ('saturatedSoilMoistureDistribution')
                read (attribs (2, 1), *) saturated_soil_moisture_distribution
                write (ifile, *) 'Saturated_soil_moisture_distribution: ', saturated_soil_moisture_distribution
            case ('particleSizeDistribution')
                read (attribs (2, 1), *) particle_size_distribution
                write (ifile, *) 'Particle_size_distribution: ', particle_size_distribution
            case ('rain_type')
                read (attribs (2, 1), *) rain_type_xml
                write (ifile, *) 'Rain_type: ', rain_type_xml
            case ('stormlength')
                read (attribs (2, 1), *) stormlength_xml
                write (ifile, *) 'Stormlength: ', stormlength_xml
            case ('mean_rainfall')
                read (attribs (2, 1), *) mean_rainfall
                write (ifile, *) 'Mean_rainfall: ', mean_rainfall
            case ('std_dev_rainfall')
                read (attribs (2, 1), *) std_dev_rainfall
                write (ifile, *) 'Std_dev_rainfall: ', std_dev_rainfall
            case ('skewness_rainfall')
                read (attribs (2, 1), *) skewness_rainfall
                write (ifile, *) 'Skewness_rainfall: ', skewness_rainfall
            case ('flow_direction')
                read (attribs (2, 1), *) flow_direction
                write (ifile, *) 'Flow_direction: ', flow_direction
            case ('flow-routing_solution_method')
                read (attribs (2, 1), *) flow_routing_solution_method
                write (ifile, *) 'flow-routing_solution_method: ', flow_routing_solution_method
            case ('sediment-routing_solution_method')
                read (attribs (2, 1), *) sediment_routing_solution_method
                write (ifile, *) 'sediment-routing_solution_method: ', sediment_routing_solution_method
            case ('update_topography')
                read (attribs (2, 1), *) update_topography_xml
                write (ifile, *) 'Update_topography: ', update_topography_xml
            case ('topography_update_interval')
                read (attribs (2, 1), *) topography_update_interval
                write (ifile, *) 'Topography_update_interval: ', topography_update_interval
            case ('time_step')
                read (attribs (2, 1), *) time_step
                write (ifile, *) 'Time_step: ', time_step
            case ('itype')
                read (attribs (2, 1), *) itype
                write (ifile, *) 'Itype: ', itype
            case ('particle_density')
                read (attribs (2, 1), *) particle_density
                write (ifile, *) 'Particle_density: ', particle_density
            case ('active_layer_sensitivity')
                read (attribs (2, 1), *) active_layer_sensitivity
                write (ifile, *) 'Active_layer_sensitivity: ', active_layer_sensitivity
            case ('particle_size_map')
                if (.not.endtag) then
                    write (ifile, *) 'Particle-size map: ', trim (attribs (2, 1))
                    do i = 1, 6
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                        particle_size_map (i) = trim (data (1))
                        write (ifile, *) 'phi ', i, ': ', particle_size_map (i)
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                    enddo
                else
                    write (ifile, *) 'Completed reading particle-size map'
                endif
            case ('use_map_phi')
                read (attribs (2, 1), *) use_map_phi
                write (ifile, *) 'Use_map_phi: ', use_map_phi
            case ('mean_particle_size')
                if (.not.endtag) then
                    write (ifile, *) 'Mean particle size: ', trim (attribs (2, 1))
                    do i = 1, n_types
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                        write (ifile, *) 'Type ', i, ': ', trim (data (1))
                        do j = 1, 6
                            call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                            call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                            read (data (1), *) mean_particle_size (i, j)
                            write (ifile, *) 'phi ', j, ': ', mean_particle_size (i, j)
                        enddo
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                    enddo
                else
                    write (ifile, *) 'Completed reading mean particle size'
                endif
            case ('std_dev_particle_size')
                if (.not.endtag) then
                    write (ifile, *) 'Particle size std dev: ', trim (attribs (2, 1))
                    do i = 1, n_types
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                        write (ifile, *) 'Type ', i, ': ', trim (data (1))
                        do j = 1, 6
                            call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                            call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                            read (data (1), *) std_dev_particle_size (i, j)
                            write (ifile, *) 'phi ', j, ': ', std_dev_particle_size (i, j)
                        enddo
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                    enddo
                else
                    write (ifile, *) 'Completed reading particle size std dev'
                endif
            case ('Cs_NH4')
               read (attribs (2, 1), *) Cs_NH4
               write (ifile, *) 'Cs_NH4: ', Cs_NH4
            case ('Cs_NO3')
               read (attribs (2, 1), *) Cs_NO3
               write (ifile, *) 'Cs_NO3: ', Cs_NO3
            case ('Cs_PO')
               read (attribs (2, 1), *) Cs_PO
               write (ifile, *) 'Cs_PO: ', Cs_PO
            case ('mass-transfer_coefficient_NH4')
               read (attribs (2, 1), *) mass_transfer_coefficient_NH4
               write (ifile, *) 'Mass-transfer_coefficient_NH4: ', mass_transfer_coefficient_NH4
            case ('mass-transfer_coefficient_NO3')
               read (attribs (2, 1), *) mass_transfer_coefficient_NO3
               write (ifile, *) 'Mass-transfer_coefficient_NO3: ', mass_transfer_coefficient_NO3
            case ('mass-transfer_coefficient_PO')
               read (attribs (2, 1), *) mass_transfer_coefficient_PO
               write (ifile, *) 'Mass-transfer_coefficient_PO: ', mass_transfer_coefficient_PO
            case ('Raindrop_detachment_a_parameter_size')
                if (.not.endtag) then
                    write (ifile, *) 'Raindrop detachment a parameter size: ', trim (attribs (2, 1))
                    do i = 1, 6
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                        read (data (1), *) Raindrop_detachment_a_parameter_size (i)
                        write (ifile, *) 'phi ', i, ': ', Raindrop_detachment_a_parameter_size (i)
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                    enddo
                else
                    write (ifile, *) 'Completed reading raindrop detachment a parameter size'
                endif
            case ('Raindrop_detachment_b_parameter_size')
                if (.not.endtag) then
                    write (ifile, *) 'Raindrop detachment b parameter size: ', trim (attribs (2, 1))
                    do i = 1, 6
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                        read (data (1), *) Raindrop_detachment_b_parameter_size (i)
                        write (ifile, *) 'phi ', i, ': ', Raindrop_detachment_b_parameter_size (i)
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                    enddo
                else
                    write (ifile, *) 'Completed reading raindrop detachment b parameter size'
                endif
            case ('Raindrop_detachment_c_parameter_size')
                if (.not.endtag) then
                    write (ifile, *) 'Raindrop detachment c parameter size: ', trim (attribs (2, 1))
                    do i = 1, 6
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                        read (data (1), *) Raindrop_detachment_c_parameter_size (i)
                        write (ifile, *) 'phi ', i, ': ', Raindrop_detachment_c_parameter_size (i)
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                    enddo
                else
                    write (ifile, *) 'Completed reading raindrop detachment c parameter size'
                endif
            case ('Raindrop_detachment_d_parameter_size')
                if (.not.endtag) then
                    write (ifile, *) 'Raindrop detachment d parameter size: ', trim (attribs (2, 1))
                    do i = 1, 6
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                        read (data (1), *) Raindrop_detachment_d_parameter_size (i)
                        write (ifile, *) 'phi ', i, ': ', Raindrop_detachment_d_parameter_size (i)
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                    enddo
                else
                    write (ifile, *) 'Completed reading raindrop detachment d parameter size'
                endif
            case ('Raindrop_detachment_max_parameter_size')
                if (.not.endtag) then
                    write (ifile, *) 'Raindrop detachment max parameter size: ', trim (attribs (2, 1))
                    do i = 1, 6
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                        read (data (1), *) Raindrop_detachment_max_parameter_size (i)
                        write (ifile, *) 'phi ', i, ': ', Raindrop_detachment_max_parameter_size (i)
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                    enddo
                else
                    write (ifile, *) 'Completed reading raindrop detachment max parameter size'
                endif
            case ('Particulate_bound_NH4')
                if (.not.endtag) then
                    write (ifile, *) 'Particulate bound NH4: ', trim (attribs (2, 1))
                    do i = 1, 6
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                        read (data (1), *) Particulate_bound_NH4 (i)
                        write (ifile, *) 'phi ', i, ': ', Particulate_bound_NH4 (i)
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                    enddo
                else
                    write (ifile, *) 'Completed reading particulate bound NH4'
                endif
            case ('Particulate_bound_NO3')
                if (.not.endtag) then
                    write (ifile, *) 'Particulate bound NO3: ', trim (attribs (2, 1))
                    do i = 1, 6
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                        read (data (1), *) Particulate_bound_NO3 (i)
                        write (ifile, *) 'phi ', i, ': ', Particulate_bound_NO3 (i)
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                    enddo
                else
                    write (ifile, *) 'Completed reading particulate bound NO3'
                endif
            case ('Particulate_bound_TN')
                if (.not.endtag) then
                    write (ifile, *) 'Particulate bound TN: ', trim (attribs (2, 1))
                    do i = 1, 6
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                        read (data (1), *) Particulate_bound_TN (i)
                        write (ifile, *) 'phi ', i, ': ', Particulate_bound_TN (i)
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                    enddo
                else
                    write (ifile, *) 'Completed reading particulate bound TN'
                endif
            case ('Particulate_bound_TP')
                if (.not.endtag) then
                    write (ifile, *) 'Particulate bound TP: ', trim (attribs (2, 1))
                    do i = 1, 6
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                        read (data (1), *) Particulate_bound_TP (i)
                        write (ifile, *) 'phi ', i, ': ', Particulate_bound_TP (i)
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                    enddo
                else
                    write (ifile, *) 'Completed reading particulate bound TP'
                endif
            case ('Particulate_bound_IC')
                if (.not.endtag) then
                    write (ifile, *) 'Particulate bound IC: ', trim (attribs (2, 1))
                    do i = 1, 6
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                        read (data (1), *) Particulate_bound_IC (i)
                        write (ifile, *) 'phi ', i, ': ', Particulate_bound_IC (i)
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                    enddo
                else
                    write (ifile, *) 'Completed reading particulate bound IC'
                endif
            case ('Particulate_bound_TC')
                if (.not.endtag) then
                    write (ifile, *) 'Particulate bound TC: ', trim (attribs (2, 1))
                    do i = 1, 6
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                        read (data (1), *) Particulate_bound_TC (i)
                        write (ifile, *) 'phi ', i, ': ', Particulate_bound_TC (i)
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                    enddo
                else
                    write (ifile, *) 'Completed reading particulate bound TC'
                endif
            case ('rainfall_dissolved_NH4_conc')
                read (attribs (2, 1), *) rainfall_dissolved_NH4_conc
                write (ifile, *) 'Rainfall_dissolved_NH4_conc: ', rainfall_dissolved_NH4_conc
            case ('rainfall_dissolved_NO3_conc')
                read (attribs (2, 1), *) rainfall_dissolved_NO3_conc
                write (ifile, *) 'Rainfall_dissolved_NO3_conc: ', rainfall_dissolved_NO3_conc
            case ('rainfall_dissolved_PO_conc')
                read (attribs (2, 1), *) rainfall_dissolved_PO_conc
                write (ifile, *) 'Rainfall_dissolved_PO_conc: ', rainfall_dissolved_PO_conc
            case ('KE_model_type')
            !
            !  added JW Feb 2017 for COST modelling exercise
            !  KE_model_type = 1 is original US model
            !  KE_model_type = 2 is for Belgium (apologies) using Verstraeten et al. (2001)
            !  used in rainfall_detachment
                read (attribs (2, 1), *) KE_model_type
                if (KE_model_type.eq.1) then
                    write (ifile, *) 'KE_model_type is US model '
                elseif (KE_model_type.eq.2) then
                    write (ifile, *) 'KE_model_type is Vertraeten et al. (2001) for Belgium (apologies)'
                else
                    write (ifile, *) 'Unknowen KE_model_type, resetting to default'
                    KE_model_type = 1
                endif
            case ('verbose_output')
                !
                !added JW Feb 2017 for COST modelling exercise
                !
                read (attribs (2, 1), *) verbose_output
                if (verbose_output) then
                    write (ifile, *) 'Verbose output is turned on '
                else
                    write (ifile, *) 'Verbose output is turned off '
                endif
            case ('within_storm_output')
                !
                !added JW Feb 2017 for COST modelling exercise
                !
                read (attribs (2, 1), *) within_storm_output
                if (within_storm_output) then
                    write (ifile, *) 'Within storm output is turned on '
                else
                    write (ifile, *) 'Within storm output is turned off '
                endif
            case ('within_storm_interval')
                !
                !added JW Feb 2017 for COST modelling exercise
                !
                read (attribs (2, 1), *) within_storm_interval
                write (ifile, *) 'Within storm interval is ', within_storm_interval, ' s'
!
! Parameters for continuous model components start here
!
            case ('mahleran_continuous')
                if (.not.endtag) then
                    write (ifile, *) '---------------------------------------------'
                    write (ifile, *) 'Mahleran_continuous: ', trim (attribs (2, 1))
                else
                    write (ifile, *) 'End of Mahleran_continuous: ', trim (attribs (2, 1))
                    write (ifile, *) '---------------------------------------------'
                endif
            case ('interstorm_input_folder')
                if (.not.endtag) then
                    read (data (1), *) interstorm_input_folder
                    write (ifile, *) 'Interstorm_input_folder: ', interstorm_input_folder
                endif
            case ('start_year')
                if (.not.endtag) then
                    read (data (1), *) start_year
                    write (ifile, *) 'start_year: ', start_year
                endif
            case ('end_year')
                if (.not.endtag) then
                    read (data (1), *) end_year
                    write (ifile, *) 'End_year: ', end_year
                endif
            case ('start_month')
                if (.not.endtag) then
                    read (data (1), *) start_month
                    write (ifile, *) 'Start_month: ', start_month
                endif
            case ('end_month')
                if (.not.endtag) then
                    read (data (1), *) end_month
                    write (ifile, *) 'End_month: ', end_month
                endif
            case ('continuous_temperature_data')
                if (.not.endtag) then
                    continuous_temperature_data = trim (data (1))
                    write (ifile, *) 'Continuous_temperature_data: ', continuous_temperature_data
                endif
            case ('continuous_rainfall_data')
                if (.not.endtag) then
                    continuous_rainfall_data = trim (data (1))
                    write (ifile, *) 'Continuous_rainfall_data: ', continuous_rainfall_data
                endif
            case ('one-min_rainfall_data')
                if (.not.endtag) then
                    one_min_rainfall_data = trim (data (1))
                    write (ifile, *) 'One-min_rainfall_data: ', one_min_rainfall_data
                endif
            case ('shrub-cover_map')
                if (.not.endtag) then
                    shrub_cover_map = trim (data (1))
                    write (ifile, *) 'Shrub-cover_map: ', shrub_cover_map
                endif
            case ('threshold_rain')
                if (.not.endtag) then
                    read (data (1), *) threshold_rain_xml
                    write (ifile, *) 'Threshold_rain: ', threshold_rain_xml
                endif
            case ('latitude')
                if (.not.endtag) then
                    read (data (1), *) latitude_xml
                    write (ifile, *) 'Latitude: ', latitude_xml
                endif
            case ('latitude_degrees')
                if (.not.endtag) then
                    read (data (1), *) latitude_degrees
                    write (ifile, *) 'Latitude in degrees: ', latitude_degrees
                endif
            case ('depth_of_upper_layer')
                if (.not.endtag) then
                    read (data (1), *) depth_of_upper_layer
                    write (ifile, *) 'Depth_of_upper_layer: ', depth_of_upper_layer
                endif
            case ('depth_of_lower_layer')
                if (.not.endtag) then
                    read (data (1), *) depth_of_lower_layer
                    write (ifile, *) 'Depth_of_lower_layer: ', depth_of_lower_layer
                endif
            case ('residual_water_content')
                if (.not.endtag) then
                    read (data (1), *) residual_water_content
                    write (ifile, *) 'Residual_water_content: ', residual_water_content
                endif
            case ('water_content_for_stomatal_closure')
                if (.not.endtag) then
                    read (data (1), *) water_content_for_stomatal_closure
                    write (ifile, *) 'Water_content_for_stomatal_closure: ', water_content_for_stomatal_closure
                endif
            case ('crusting_evapotranspiration_reduction')
                if (.not.endtag) then
                    read (data (1), *) crusting_evapotranspiration_reduction
                    write (ifile, *) 'Crusting_evapotranspiration_reduction: ', crusting_evapotranspiration_reduction
                endif
            case ('infiltration_from_bare_to_layer_2')
                if (.not.endtag) then
                    read (data (1), *) infiltration_from_bare_to_layer_2
                    write (ifile, *) 'Infiltration_from_bare_to_layer_2: ', infiltration_from_bare_to_layer_2
                endif
            case ('infiltration_from_shrub_to_layer_2')
                if (.not.endtag) then
                    read (data (1), *) infiltration_from_shrub_to_layer_2
                    write (ifile, *) 'Infiltration_from_shrub_to_layer_2: ', infiltration_from_shrub_to_layer_2
                endif
            case ('ksat')
                if (.not.endtag) then
                    read (data (1), *) ksat_xml
                    write (ifile, *) 'Ksat: ', ksat_xml
                endif
            case ('diffusion_coefficient')
                if (.not.endtag) then
                    read (data (1), *) diffusion_coefficient
                    write (ifile, *) 'Diffusion_coefficient: ', diffusion_coefficient
                endif
            case ('initial_value_for_sm_1')
                if (.not.endtag) then
                    read (data (1), *) initial_value_for_sm_1
                    write (ifile, *) 'Initial_value_for_sm_1: ', initial_value_for_sm_1
                endif
            case ('initial_value_for_sm_2')
                if (.not.endtag) then
                    read (data (1), *) initial_value_for_sm_2
                    write (ifile, *) 'Initial_value_for_sm_2: ', initial_value_for_sm_2
                endif
!
! Parameters for vegetation submodel components of continuous model start here
!
            case ('number_of_species')   !maximal no. of species in cell
                if (.not.endtag) then
                    read (data (1), *) no_species
                    write (ifile, *) 'Number of species: ', no_species
                endif
                allocate (theta_WP (no_species), source = 0.0e0, stat = ierr)
!                if (ierr /= 0 ) stop "Memory error allocating theta_WP in read_parameters_from_xml_file! "
                allocate (root (2, no_species), source = 0.0e0, stat = ierr)
!                if (ierr /= 0 ) stop "Memory error allocating root in read_parameters_from_xml_file!"
                allocate (uptake (no_species), source = 0.0e0, stat = ierr)
!                if (ierr /= 0 ) stop "Memory error allocating uptake in read_parameters_from_xml_file!"
                allocate (r (no_species), source = 0.0e0, stat = ierr)
!                if (ierr /= 0 ) stop "Memory error allocating r in read_parameters_from_xml_file!"
                allocate (mr (no_species), source = 0.0e0, stat = ierr)
!                if (ierr /= 0 ) stop "Memory error allocating mr in read_parameters_from_xml_file!"
                allocate (e (no_species), source = 0.0e0, stat = ierr)
!                if (ierr /= 0 ) stop "Memory error allocating e in read_parameters_from_xml_file!"
            case ('start_of_growing_season')   !Julian day at start of growing season
                if (.not.endtag) then
                    read (data (1), *) start_season
                    write (ifile, *) 'Julian day at start of growing season: ', start_season
                endif
            case ('end_of_growing_season')   !Julian day at start of growing season
                if (.not.endtag) then
                    read (data (1), *) stop_season
                    write (ifile, *) 'Julian day at end of growing season: ', stop_season
                endif
            case ('wilting_point')   !wilting point for all species in m3/m3
                if (no_species.eq.-9999) then
                   stop "Number of species needs to be set before wilting point in read_interstorm_parameters_xml!"
                endif
                if (.not.endtag) then
                    write (ifile, *) 'Wilting point: ', trim (attribs (2, 1))
                    do k = 1, no_species
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                        read (data (1), *) theta_WP (k)
                        write (ifile, *) 'Species ', k, ': ', theta_WP (k)
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                    enddo
                else
                    write (ifile, *) 'Completed reading wilting point'
                endif
            case ('potential_uptake_rate')   !Potential uptake rate per unit vegetation cover (mm/y)
                if (no_species.eq.-9999) then
                   stop "Number of species needs to be set before potential_uptake_rate in read_interstorm_parameters_xml!"
                endif
                if (.not.endtag) then
                    write (ifile, *) 'Potential uptake rate per unit vegetation cover: ', trim (attribs (2, 1))
                    do k = 1, no_species
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                        read (data (1), *) uptake (k)
                        write (ifile, *) 'Species ', k, ': ', uptake (k)
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                    enddo
                else
                    write (ifile, *) 'Completed reading potential_uptake_rate'
                endif
            case ('root_fractions')  !!Fraction of grass and shrub roots in upper and lower layer (dimensionless)
                if (no_species.eq.-9999) then
                   stop "Number of species needs to be set before root_fractions in read_interstorm_parameters_xml!"
                endif
                if (.not.endtag) then
                    write (ifile, *) 'Fraction of roots : ', trim (attribs (2, 1))
                    do k = 1, no_species
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                        write (ifile, *) 'Species ', k, ': ', trim (data (1))
                        do layer = 1, 2
                            call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                            call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                            read (data (1), *) root (layer, k)
                            write (ifile, *) 'layer ', layer, ': ', root (layer, k)
                        enddo
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                    enddo
                else
                    write (ifile, *) 'Completed reading root_fractions'
                endif
            case ('potential_growth_rate')   !Potential growth rate of vegetation type in mm^-1 yr^-1
                if (no_species.eq.-9999) then
                   stop "Number of species needs to be set before potential_growth_rate in read_interstorm_parameters_xml!"
                endif
                if (.not.endtag) then
                    write (ifile, *) 'Potential growth rate of vegetation type in mm^-1 yr^-1: ', trim (attribs (2, 1))
                    do k = 1, no_species
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                        read (data (1), *) r (k)
                        write (ifile, *) 'Species ', k, ': ', r (k)
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                    enddo
                else
                    write (ifile, *) 'Completed reading potential_growth_rate'
                endif
            case ('mortality_rate')   !mortality rate of vegetation due to water stress in mm^-1 yr^-1
                if (no_species.eq.-9999) then
                   stop "Number of species needs to be set before mortality_rate in read_interstorm_parameters_xml!"
                endif
                if (.not.endtag) then
                    write (ifile, *) 'Mortality rate of vegetation due to water stress in mm^-1 yr^-1: ', trim (attribs (2, 1))
                    do k = 1, no_species
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                        read (data (1), *) mr (k)
                        write (ifile, *) 'Species ', k, ': ', mr (k)
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                    enddo
                else
                    write (ifile, *) 'Completed reading mortality_rate'
                endif
            case ('establishment_rate')   !rate of successful establishment of vegetation
                if (no_species.eq.-9999) then
                   stop "Number of species needs to be set before establishment_rate in read_interstorm_parameters_xml!"
                endif
                if (.not.endtag) then
                    write (ifile, *) 'Rate of successful establishment of vegetation: ', trim (attribs (2, 1))
                    do k = 1, no_species
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                        read (data (1), *) e (k)
                        write (ifile, *) 'Species ', k, ': ', e (k)
                        call xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
                    enddo
                else
                    write (ifile, *) 'Completed reading establishment_rate'
                endif
            case ('shrub_seed_dispersal_max_distance')   !maximal distance of shrub seed dispersal (m)
                if (.not.endtag) then
                    read (data (1), *) dist_max
                    write (ifile, *) 'Maximal distance of shrub seed dispersal (m): ', dist_max
                endif

!
! Parameters for MiC model components start here
!
            case ('mahleran_mic')
                if (.not.endtag) then
                    write (ifile, *) '---------------------------------------------'
                    write (ifile, *) 'Mahleran MiC input: ', trim (data (1))
                else
                    write (ifile, *) 'End of Mahleran MiC input'
                    write (ifile, *) '---------------------------------------------'
                endif
            case ('marker_file')
                if (.not.endtag) then
                    marker_file = trim (data (1))
                    write (ifile, *) 'Marker_file: ', marker_file
                endif
!
! Parameters for selecting output files start here
!
            case ('mahleran_output_files')
                if (.not.endtag) then
                    write (ifile, *) '---------------------------------------------'
                    write (ifile, *) 'Mahleran_output_files: ', trim (data (1))
                else
                    write (ifile, *) 'End of Mahleran_output_files'
                    write (ifile, *) '---------------------------------------------'
                endif
            case ('aspectfileimg')
                if (.not.endtag) then
                    read (data (1), *) aspectfileimg
                    write (ifile, *) 'aspectfileimg: ', aspectfileimg
                endif
            case ('contrib')
                if (.not.endtag) then
                    read (data (1), *) contrib_xml
                    write (ifile, *) 'contrib: ', contrib_xml
                endif
            case ('d_xsect')
                if (.not.endtag) then
                    read (data (1), *) d_xsect
                    write (ifile, *) 'd_xsect: ', d_xsect
                endif
            case ('depfileimg')
                if (.not.endtag) then
                    read (data (1), *) depfileimg
                    write (ifile, *) 'depfileimg: ', depfileimg
                endif
            case ('detfileimg')
                if (.not.endtag) then
                    read (data (1), *) detfileimg
                    write (ifile, *) 'detfileimg: ', detfileimg
                endif
            case ('dfileimg')
                if (.not.endtag) then
                    read (data (1), *) dfileimg
                    write (ifile, *) 'dfileimg: ', dfileimg
                endif
            case ('dischfile')
                if (.not.endtag) then
                    read (data (1), *) dischfile_xml
                    write (ifile, *) 'dischfile: ', dischfile_xml
                endif
            case ('dmapfile')
                if (.not.endtag) then
                    read (data (1), *) dmapfile_xml
                    write (ifile, *) 'dmapfile: ', dmapfile_xml
                endif
            case ('flowdetimg')
                if (.not.endtag) then
                    read (data (1), *) flowdetimg
                    write (ifile, *) 'flowdetimg: ', flowdetimg
                endif
            case ('hydrofile')
                if (.not.endtag) then
                    read (data (1), *) hydrofile_xml
                    write (ifile, *) 'hydrofile: ', hydrofile_xml
                endif
            case ('hypointsfile')
                if (.not.endtag) then
                    read (data (1), *) hypointsfile
                    write (ifile, *) 'hypointsfile: ', hypointsfile
                endif
            case ('inundfile')
                if (.not.endtag) then
                    read (data (1), *) inundfile
                    write (ifile, *) 'inundfile: ', inundfile
                endif
            case ('ksat_output')
                if (.not.endtag) then
                    read (data (1), *) ksat_output
                    write (ifile, *) 'ksat: ', ksat_output
                endif
            case ('markerfileimg')
                if (.not.endtag) then
                    read (data (1), *) markerfileimg_xml
                    write (ifile, *) 'markerfileimg: ', markerfileimg_xml
                endif
            case ('markerofffile')
                if (.not.endtag) then
                    read (data (1), *) markerofffile_xml
                    write (ifile, *) 'markerofffile: ', markerofffile_xml
                endif
            case ('mstatusfile')
                if (.not.endtag) then
                    read (data (1), *) mstatusfile_xml
                    write (ifile, *) 'mstatusfile: ', mstatusfile_xml
                endif
            case ('MXYfile')
                if (.not.endtag) then
                    read (data (1), *) MXYfile_xml
                    write (ifile, *) 'MXYfile: ', MXYfile_xml
                endif
            case ('neterosimg')
                if (.not.endtag) then
                    read (data (1), *) neterosimg
                    write (ifile, *) 'neterosimg: ', neterosimg
                endif
            case ('nutpointfile')
                if (.not.endtag) then
                    read (data (1), *) nutpointfile_xml
                    write (ifile, *) 'nutpointfile: ', nutpointfile_xml
                endif
            case ('nutrientfile')
                if (.not.endtag) then
                    read (data (1), *) nutrientfile_xml
                    write (ifile, *) 'nutrientfile: ', nutrientfile_xml
                endif
            case ('nutrifileimg')
                if (.not.endtag) then
                    read (data (1), *) nutrifileimg
                    write (ifile, *) 'nutrifileimg: ', nutrifileimg
                endif
            case ('nitratefileimg')
                if (.not.endtag) then
                    read (data (1), *) nitratefileimg
                    write (ifile, *) 'nitratefileimg: ', nitratefileimg
                endif
            case ('phosfileimg')
                if (.not.endtag) then
                    read (data (1), *) phosfileimg
                    write (ifile, *) 'phosfileimg: ', phosfileimg
                endif
            case ('order')
                if (.not.endtag) then
                    read (data (1), *) order_xml
                    write (ifile, *) 'order: ', order_xml
                endif
            case ('p_ammoniumimg')
                if (.not.endtag) then
                    read (data (1), *) p_ammoniumimg
                    write (ifile, *) 'p_ammoniumimg: ', p_ammoniumimg
                endif
            case ('p_ICimg')
                if (.not.endtag) then
                    read (data (1), *) p_ICimg
                    write (ifile, *) 'p_ICimg: ', p_ICimg
                endif
            case ('p_nitrateimg')
                if (.not.endtag) then
                    read (data (1), *) p_nitrateimg
                    write (ifile, *) 'p_nitrateimg: ', p_nitrateimg
                endif
            case ('p_nutfile')
                if (.not.endtag) then
                    read (data (1), *) p_nutfile_xml
                    write (ifile, *) 'p_nutfile: ', p_nutfile_xml
                endif
            case ('p_TCimg')
                if (.not.endtag) then
                    read (data (1), *) p_TCimg
                    write (ifile, *) 'p_TCimg: ', p_TCimg
                endif
            case ('p_TNimg')
                if (.not.endtag) then
                    read (data (1), *) p_TNimg
                    write (ifile, *) 'p_TNimg: ', p_TNimg
                endif
            case ('p_TPimg')
                if (.not.endtag) then
                    read (data (1), *) p_TPimg
                    write (ifile, *) 'p_TPimg: ', p_TPimg
                endif
            case ('paramfile')
                if (.not.endtag) then
                    read (data (1), *) paramfile
                    write (ifile, *) 'paramfile: ', paramfile
                endif
            case ('pave')
                if (.not.endtag) then
                    read (data (1), *) pave_xml
                    write (ifile, *) 'pave: ', pave_xml
                endif
            case ('q_xsect')
                if (.not.endtag) then
                    read (data (1), *) q_xsect
                    write (ifile, *) 'q_xsect: ', q_xsect
                endif
            case ('qfileimg')
                if (.not.endtag) then
                    read (data (1), *) qfileimg
                    write (ifile, *) 'qfileimg: ', qfileimg
                endif
            case ('qpointfile')
                if (.not.endtag) then
                    read (data (1), *) qpointfile_xml
                    write (ifile, *) 'qpointfile: ', qpointfile_xml
                endif
            case ('qspointfile')
                if (.not.endtag) then
                    read (data (1), *) qspointfile_xml
                    write (ifile, *) 'qspointfile: ', qspointfile_xml
                endif
            case ('raindetimg')
                if (.not.endtag) then
                    read (data (1), *) raindetimg
                    write (ifile, *) 'raindetimg: ', raindetimg
                endif
            case ('rainmask')
                if (.not.endtag) then
                    read (data (1), *) rainmask
                    write (ifile, *) 'rainmask: ', rainmask
                endif
            case ('sedconcfile')
                if (.not.endtag) then
                    read (data (1), *) sedconcfile_xml
                    write (ifile, *) 'sedconcfile: ', sedconcfile_xml
                endif
            case ('seddeposfile')
                if (.not.endtag) then
                    read (data (1), *) seddeposfile_xml
                    write (ifile, *) 'seddeposfile: ', seddeposfile_xml
                endif
            case ('seddepthfile')
                if (.not.endtag) then
                    read (data (1), *) seddepthfile_xml
                    write (ifile, *) 'seddepthfile: ', seddepthfile_xml
                endif
            case ('seddetfile')
                if (.not.endtag) then
                    read (data (1), *) seddetfile_xml
                    write (ifile, *) 'seddetfile: ', seddetfile_xml
                endif
            case ('seddischfile')
                if (.not.endtag) then
                    read (data (1), *) seddischfile_xml
                    write (ifile, *) 'seddischfile: ', seddischfile_xml
                endif
            case ('sedfile')
                if (.not.endtag) then
                    read (data (1), *) sedfile_xml
                    write (ifile, *) 'sedfile: ', sedfile_xml
                endif
            case ('sedfileimg')
                if (.not.endtag) then
                    read (data (1), *) sedfileimg
                    write (ifile, *) 'sedfileimg: ', sedfileimg
                endif
            case ('sedpropnfile')
                if (.not.endtag) then
                    read (data (1), *) sedpropnfile_xml
                    write (ifile, *) 'sedpropnfile: ', sedpropnfile_xml
                endif
            case ('sedvelfile')
                if (.not.endtag) then
                    read (data (1), *) sedvelfile_xml
                    write (ifile, *) 'sedvelfile: ', sedvelfile_xml
                endif
            case ('shrubland_xsect')
                if (.not.endtag) then
                    read (data (1), *) shrubland_xsect
                    write (ifile, *) 'shrubland_xsect: ', shrubland_xsect
                endif
            case ('slope')
                if (.not.endtag) then
                    read (data (1), *) slope_xml
                    write (ifile, *) 'slope: ', slope_xml
                endif
            case ('soildepfileimg')
                if (.not.endtag) then
                    read (data (1), *) soildepfileimg
                    write (ifile, *) 'soildepfileimg: ', soildepfileimg
                endif
            case ('soilvelfileimg')
                if (.not.endtag) then
                    read (data (1), *) soilvelfileimg
                    write (ifile, *) 'soilvelfileimg: ', soilvelfileimg
                endif
            case ('thetafileimg')
                if (.not.endtag) then
                    read (data (1), *) thetafileimg_xml
                    write (ifile, *) 'thetafileimg: ', thetafileimg_xml
                endif
            case ('topog')
                if (.not.endtag) then
                    read (data (1), *) topog
                    write (ifile, *) 'topog: ', topog
                endif
            case ('v_xsect')
                if (.not.endtag) then
                    read (data (1), *) v_xsect
                    write (ifile, *) 'v_xsect: ', v_xsect
                endif
            case ('veg')
                if (.not.endtag) then
                    read (data (1), *) veg_xml
                    write (ifile, *) 'veg: ', veg_xml
                endif
            case ('vfileimg')
                if (.not.endtag) then
                    read (data (1), *) vfileimg
                    write (ifile, *) 'vfileimg: ', vfileimg
                endif
            case default
                if (endtag) then
                    write (ifile, *) 'Unknown parameter in file: ', trim (tag)
                endif

        end select

        if (.not. xml_ok(info)) exit
    enddo
endif
call xml_close (info)
close (ifile)
!
!   determine length of basic model path names
!
input_folder_length = sizeof (input_folder)
do i = 1, sizeof (input_folder)
   if (input_folder (i:i) == ' ') then
      input_folder_length = i - 1
      exit
   endif
enddo
output_folder_length = sizeof (output_folder)
do i = 1, sizeof (output_folder)
   if (output_folder (i:i) == ' ') then
      output_folder_length = i - 1
      exit
   endif
enddo
interstorm_input_folder_length = sizeof (interstorm_input_folder)
do i = 1, sizeof (interstorm_input_folder)
   if (interstorm_input_folder (i:i) == ' ') then
      interstorm_input_folder_length = i - 1
      exit
   endif
enddo
return
end
