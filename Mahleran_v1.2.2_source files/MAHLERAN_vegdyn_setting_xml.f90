!****************************************************************************
!  subroutine to set parameters and files for vegetation dynamics calculation
!  modified to use inputs from xml file format from v.1.2.1
!****************************************************************************
subroutine MAHLERAN_vegdyn_setting_xml

use interstorm_shared_data
use shared_data
use vegdynamics_shared_data

implicit none

real :: dummy1

!integer :: i, j, k, layer
integer :: ierr

logical :: fexist

character (len = 120) :: filename, file

!JW Apr 2017 see original code below.  All reading and most allocation now done in read_parameters_from_xml_file
!initialize
dt_vegetation = 14      !turnus in which vegetation dynamics is calculated, at the moment set to 14 days

allocate (c_veg (2, no_species, nr2, nc2), source = 0.0e0, stat = ierr)
if (ierr /= 0 ) stop "Memory error allocating c_veg in MAHLERAN_vegdyn_setting_xml!"
allocate (mort (no_species, nr2, nc2), source = 0.0e0, stat = ierr)
if (ierr /= 0 ) stop "Memory error allocating mort in MAHLERAN_vegdyn_setting_xml!"
allocate (gr (no_species, nr2, nc2), source = 0.0e0, stat = ierr)
if (ierr /= 0 ) stop "Memory error allocating gr in MAHLERAN_vegdyn_setting_xml!"
allocate (disp (no_species, nr2, nc2), source = 0.0e0, stat = ierr)
if (ierr /= 0 ) stop "Memory error allocating disp in MAHLERAN_vegdyn_setting_xml!"

!now included in source terms above
!c_veg (:, :, :, :) = 0.
!mort (:, :, :) = 0.
!gr (:, :, :) = 0.
!disp (:, :, :) = 0.

! initialize vegetation cover (as was read in in interstorm and storm sections)
!JW Apr 2017 Implies hard coding as only shrub and grass cover included
c_veg (1, 2, :, :) = shrub_cover (:, :) / 100.
c_veg (1, 1, :, :) = cover (:, :) / 100. - shrub_cover (:, :) / 100.    !cover contains both the sum of shrub and grass cover

end



!old code -- all reading of data not in read_parameters_from_xml_file
!read in parameters
!filename = 'input_vegdynamics.dat'
!
!check if input parameter file for vegetation dynamics exist
!
!inquire (file = filename, exist = fexist)
!if (.not.fexist) then
!   write(*,*)
!   write(*,*) 'input parameter file for vegetation dynamics does not exist'
!   write(*,*)
!   stop
!endif
!
!if file input_vegdynamics.dat does exist, start reading from file
!
!if (fexist) then
!   open (2, file = filename, status = 'unknown')
!   rewind (2)
!   read (2, *)
!   read (2, *) no_species      !maximal no. of species in cell
!   read (2, *) start_season    !Julian day at start of growing season
!   read (2, *) stop_season     !Julian day at end of growing season
!   read (2, *) (theta_WP (k), k = 1, no_species)   !wilting point for all species in m3/m3
!   read (2, *) (uptake (k), k = 1, no_species)     !Potential uptake rate per unit grass or shrub cover (mm/y)
!   do k = 1, no_species
!      read (2, *) (root (layer, k), layer = 1, 2)  !Fraction of grass and shrub roots in upper and lower layer (dimensionless)
!   enddo
!   read(2,*) (r (k), k = 1, no_species) !potentail growth rate of grass and shrub in mm^-1 yr^-1
!   read(2,*) (mr (k), k = 1,no_species)  !mortality rate of grass and shrubs due to water stress in mm^-1 yr^-1
!   read(2,*) (e (k), k = 1, no_species)    !rate of successful establishment of grass and shrub
!   read(2,*) dist_max  !maximal distance of shrub seed dispersal (m)
!endif
!allocate (theta_WP (no_species), root(2,no_species),uptake(no_species), r(no_species), mr(no_species))
!allocate (e(no_species))


