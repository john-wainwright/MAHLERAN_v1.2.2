program MAHLERAN_1_2_2
!
!   Version 1.2.2 (March 2019) - bug fixes
!   Version 1.2.1 (September 2015)
!       John.Wainwright@durham.ac.uk
!
!       Combined Event, Interstorm and MiC version
!       Uses new file input format from xml file

!   Model for Assessing Hillslope-Landscape Erosion, Runoff and Nutrients
!   as described in ESPL (2008):
!   "A Transport-Distance Approach to Scaling Erosion Rates:
!   Model Development and Testing" by John Wainwright, Anthony J. Parsons,
!   Eva N. Mueller, Richard E. Brazier, D. Mark Powell and Banti Fenti
!
!   MiC version as described in JGR-ES (2012):
!   "A new approach for simulating the redistribution of soil particles by
!   water erosion: a marker-in-cell model" by James Cooper, John Wainwright,
!   Antony J Parsons, Yuichi Onda, Tomomi Fukuwara, Eichiro Obana, Ben Kitchener,
!   Edward J Long and Graham H Hargrave, Journal of Geophysical Research –
!   Earth Surface 117, F04027, doi:10.1029/2012JF002499
!
!   Continuous version is described in:
!   Eva N Müller EN, Britta Tietjen, Laura Turnbull, John Wainwright (submitted)
!   "Ecohydrological modelling of land degradation in drylands: feedbacks between
!   water, erosion, vegetation and soil", Journal of Geophysical Research – Biogeosciences.
!
!    Available versions:
!   1.0 Used in ESPL papers
!   1.01 As developed by Laura for thesis
!   1.01.1 As above with infiltration bug fix
!   1.01.2 Change of subroutine structure, output file creation this version)
!       1.01.3 Minor changes for Linux compatibility (modification of overlength lines and non-standard subroutine calls)
!   1.02 Multi-scale version (Laura to complete)
!   1.03 Addition of rule-based vegetation to include vertical water flux (Eva to lead discussion; consult with Jill about solution methods)
!   1.04 Addition of DAYCENT-Lite nutrient dynamics (Laura to lead discussion)
!       1.05 Addition of interstorm dynamics (soil moisture, evapotranspiration, veg dyn.)
!       1.2.0 Version that shifts to use xml input file from GUSTAV interface and combines event, continuous and MiC versions into one structure
!       1.2.1 Adds modifications to concentrated_flow_transport and suspended_transport to allow density differences to be included specifically
!       1.2.2 This version -- bug fixes to 1.2.1


use shared_data
use interstorm_shared_data
use parameters_from_xml
use time_h

implicit none

integer *4 time_array (8)
integer *4 :: idum

character (len = 80) :: fname

integer cont_sim
integer i, k

data idum / -1 /

fname = 'mahleran_input.xml'

call date_and_time (values = time_array)
write (6, 9999) time_array (5), time_array (6), time_array (7), time_array (3), time_array (2), time_array (1)

9999 format (' ---------------------------------------------------',/&
            ' |               MAHLERAN  v1.2.2                  |',/&
            ' |      code (c) J. Wainwright, E.N. Mueller,      |',/&
            ' |    L. Turnbull, R.E. Brazier, J.R. Cooper,      |',/&
            ' |       C.J.M. Hewett, B. Fenti, B. Tjetien       |',/&
            ' |                  1997 - 2019                    |',/&
            ' | contact John.Wainwright@dur.ac.uk for details   |',/&
            ' |   Run start time ', i2, ':', i2, ':', i2, ' on ', &
                                   i2, '/', i2, '/', i4, '         |',/&
            ' ---------------------------------------------------',/)

write (6, *) ' initializing random-number generator'
if (idum.eq.-1) then
!
!   initialize random numbers
!
!JWMar09 Use f90 intrinsic call to date_and_time subroutine to initialize random number generator
! time_array(1)    year
! time_array(2)    month of the year
! time_array(3)    day of the month
! time_array(4)    time offset with respect to UTC in minutes
! time_array(5)    hour of the day
! time_array(6)    minutes of the hour
! time_array(7)    seconds of the minute
! time_array(8)    milliseconds of the second
! multiplying day, hour, min, sec, millisec gives 2675721600 discrete values
!   call date_and_time (values = time_array) !now called above for output of start time
   idum = time_array (3)
   do i = 5, 8
      idum = idum * time_array (i)
   enddo
   idum = -idum
   call ZBQLINI (idum)  ! initialize random-number generator
else
   call ZBQLINI (0)  ! initialize random-number generator
endif
write (6, *) ' back from initializing random-number generator'

call read_parameters_from_xml_file (fname)

write (6, *) ' back from reading parameters'
if (model_version.ne.'1.2.2') then
    write (*, *)
    write (*, *)'******************************************************************************************************'
    write (6, *) ' Warning: this is MAHLERAN v.1.2.2, whereas input file is prepared for model version ', model_version
    write (*, *)'******************************************************************************************************'
    write (*, *)
endif

!***********************************************************
! cont_sim = 1 Mahleran Version 1.06 only (one storm)
! cont_sim = 2 Continuous simulation (instorm and storms)
! cont_sim = 3 Mahleran MiC (one storm)
! cont_sim = 4 Mahleran MiC Continuous simulation (instorm and storms)
!***********************************************************

select case (model_run_type)
    case ('event')
        cont_sim = 1
!***********************************************************
!       calculate individual storms only (Mahleran Version 1.06)
!***********************************************************
        write (*, *)
        write (*, *)'****************************************************************'
        write (*, *)'                    running event model'
        write (*, *)'****************************************************************'
        write (*, *)
        call MAHLERAN_storm_setting_xml
        call MAHLERAN_storm_xml
    case ('continuous')
        cont_sim = 2
!***********************************************************
!       for thunderstorm and interstorm dynamics (Mahleran Version 1.05)
!***********************************************************
        write (*, *)
        write (*, *)'****************************************************************'
        write (*, *)'                    running continuous model'
        write (*, *)'****************************************************************'
        write (*, *)
        call MAHLERAN_storm_setting_xml
        call MAHLERAN_interstorm_setting_xml
        call MAHLERAN_vegdyn_setting_xml
        call MAHLERAN_interstorm_xml
    case ('MiC')
        cont_sim = 3
        write (*, *)
        write (*, *)'****************************************************************'
        write (*, *)'                    running event MiC model'
        write (*, *)'****************************************************************'
        call MAHLERAN_storm_setting_xml
        call read_markers_xml
        call MAHLERAN_storm_xml
    call output_markers_xml
        write (*, *)
    case ('continuous_MiC')
        cont_sim = 4
        write (*, *)
        write (*, *)'****************************************************************'
        write (*, *)'                  running continuous MiC model'
        write (*, *)'****************************************************************'
        call MAHLERAN_storm_setting_xml
        call read_markers_xml
        call MAHLERAN_interstorm_setting_xml
        call MAHLERAN_vegdyn_setting_xml
        call MAHLERAN_interstorm_xml
    call output_markers_xml
        write (*, *)
    case default
        write (*, *)
        write (*, *)'****************************************************************'
        write (*, *)'              unknown model run type -- terminating'
        write (*, *)'****************************************************************'
        write (*, *)
end select

!****************************************************************
!       end of main program
!****************************************************************
write (*, *)
write (*, *)'****************************************************************'
write (*, *)'                      run completed'
write (*, *)'****************************************************************'
write (*, *)

end program



