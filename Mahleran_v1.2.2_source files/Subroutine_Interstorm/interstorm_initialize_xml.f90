
!****************************************************************
!  subroutine to initialise all interstorm arrays and variables
!****************************************************************
subroutine interstorm_initialize_xml

use shared_data
use interstorm_shared_data

implicit none
integer i, j, ierr


!****************************************************************
! variables used in this subroutine listed in order of their occurrence
! local
!****************************************************************
! i is running variable for row
! j is running variable for column
!****************************************************************
! global
!****************************************************************
! slope_radiant(:,:)
! nr2
! nc2
! aspect_factor(:,:)
! slope(:,:)
! nr
! nc
! aspect(:,:)
! theta_begin(:,:)
! sm_1(:,:,:,:)
! depth_1(:,:)
! sm_2(:,:,:,:)
! depth_2(:,:)
! grass_cover(:,:)
! cover(:,:)
! shrub_cover(:,:)
! field_cap(:,:)
! theta_sat(:,:)
! et_pot(:,:,:)
! et_1(:,:,:)
! et_2(:,:,:)
! inf_2(:,:,:)
! inf_1(:,:,:)
! drain_1(:,:,:)
! drain_2(:,:,:)
! sm_1to2(:,:,:)
! sm_sf(:,:,:)
! sm_1_init
! sm_2_init
! theta_areal_average(:,:): value for theta_areal_average [mm]
!****************************************************************


!****************************************************************
!       allocates slope_radiant and aspect_factor values for each cell
!****************************************************************
allocate (slope_radiant (nr2, nc2), source = 0.0e0, stat = ierr) ! peter2011_02_17: changed (nr,nr) to (nr1,nc1)
if (ierr /= 0 ) stop "Memory error allocating slope_radiant in interstorm_initialize_xml!"
allocate (aspect_factor (nr2, nc2), source = 0.0e0, stat = ierr)  ! peter2011_02_17: changed (nr,nr) to (nr1,nc1)
if (ierr /= 0 ) stop "Memory error allocating aspect_factor in interstorm_initialize_xml!"

do i = 2, nr2
   do j = 2, nc2
      slope_radiant (i, j) = (atan (slope (i, j))) ! slope2rad
   enddo
enddo

do i = 2, nr
   do j = 2, nc
!****************************************************************
!        setting for aspect_factor
!****************************************************************
      if (aspect(i, j).eq.1) then ! aspect 1 means that cell faces north
         aspect_factor (i, j) = 0.9
      elseif (aspect (i, j).eq.2) then ! aspect 2 means that cell faces east
         aspect_factor(i,j) = 0.98
      elseif (aspect (i, j).eq.3) then ! aspect 3 means that cell faces south
         aspect_factor (i, j) = 1.1
      elseif (aspect (i, j).eq.4) then ! aspect 4 means that cell faces west
         aspect_factor (i, j) = 1.02
      else
         aspect_factor (i, j) = 1.0
      endif
   enddo
enddo

!
!Initialise soil moisture at beginning of simulation run
!
do i = 2, nr
   do j = 2, nc
      if (rmask (i, j).ge.0.0d0) then
         sm (1, 1, i, j) = sm_1_init * depth (1)
         sm (2, 1, i, j) = sm_2_init * depth (2)
      endif
   enddo
enddo

theta_min=0.03

do i = 2, nr
   do j = 2, nc
      grass_cover (i, j) = cover (i, j) - shrub_cover (i, j)        !grass cover in %
      if (grass_cover (i, j).lt.0) then
         grass_cover (i, j) = 0.
      endif
!Eva: preliminary set field capacity to 75 % of saturated soil moisture
      field_cap (i, j) = 0.75 * theta_sat (i, j)
   enddo
enddo


!set all other variables to zero
et_pot (:, :) = 0.
et_1 (:, :) = 0.
et_2 (:, :) = 0.
inf_2 (:, :) = 0.
inf_1 (:, :) = 0.
drain_1 (:, :) = 0.
drain_2 (:, :) = 0.
sm_1to2 (:, :) = 0.
sm_sf (:, :) = 0.
qsum_all (:, :, :) = 0.
sedtotal_all (:, :, :) = 0.
theta_areal_average (:, :) = 0.

end
