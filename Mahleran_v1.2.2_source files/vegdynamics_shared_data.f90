module vegdynamics_shared_data
save

integer :: no_species
integer :: dt_vegetation        !interval in which vegetation dynamics is calculated, at the moment set to 14 days
integer :: start_season         !Julian day when growing season starts
integer :: stop_season          !Julian day when season stops

real :: dist_max            !maximal distance of shrub seed dispersal (m)
real, allocatable :: c_veg (:, :, :, :) !veg cover in veg dynamics calculation (%) c_veg(status 1 or 2,no. of species,i,j)
real, allocatable :: gr (:, :, :)   !growth (%), gr(status 1 or 2,no. of species,i,j)
real, allocatable :: mort (:, :, :) !mortality
real, allocatable :: disp (:, :, :) !dispersal
real, allocatable :: theta_WP (:)   !plant specific wilting point (m3/m3)
real, allocatable :: root (:, :)    !root(layer, no_species) Fraction of grass or shrub roots in upper and lower layer (dimensionless)
real, allocatable :: uptake (:)     !uptake(k), k=1, no_species: potential uptake rate per unit grass or shrub cover in mm/y
real, allocatable :: r (:)      !potential growth rate of grass and shrub in mm^-1 yr^-1
real, allocatable :: mr (:)     !mortality rate of grass and shrubs due to water stress in mm^-1 yr^-1
real, allocatable :: e (:)      !rate of successful establishment of grass and shrub

end module vegdynamics_shared_data

