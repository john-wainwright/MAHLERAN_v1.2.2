
!****************************************************************
!  subroutine to initialise all interstorm arrays and variables
!****************************************************************
subroutine interstorm_initialise

use shared_data
use interstorm_shared_data
implicit double precision (a - h, o - z)
implicit integer (i - n)
integer i,j


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
allocate (slope_radiant(nr2,nc2)) ! peter2011_02_17: changed (nr,nr) to (nr1,nc1)
allocate (aspect_factor(nr2,nc2))  ! peter2011_02_17: changed (nr,nr) to (nr1,nc1)

do i = 2, nr2
   do j = 2, nc2
    slope_radiant(i,j)=(atan(slope(i,j))) ! slope2rad
   enddo
enddo

do i = 2, nr
   do j = 2, nc
!****************************************************************
!        setting for aspect_factor
!****************************************************************
       if (aspect(i,j)==1) then ! aspect 1 means that cell faces north
    aspect_factor(i,j) = 0.9
       else if (aspect(i,j)==2) then ! aspect 2 means that cell faces east
    aspect_factor(i,j) = 0.98
   else if (aspect(i,j)==3) then ! aspect 3 means that cell faces south
    aspect_factor(i,j) = 1.1
   else if (aspect(i,j)==4) then ! aspect 4 means that cell faces west
    aspect_factor(i,j) = 1.02
   else
    aspect_factor(i,j) = 1.0
   end if
   enddo
enddo


!
!Initialise soil moisture at beginning of simulation run

!

!Eva: preliminary, set theta at beginning of simulation run to 0.05 m3/m3
theta_begin(:,:)=0.05
do i = 2, nr
   do j = 2, nc
        sm_1(1,i,j)= theta_begin(i,j)*depth_1
        sm_2(1,i,j)= theta_begin(i,j)*depth_2
   enddo
enddo


do i = 2, nr
   do j = 2, nc
        grass_cover(i,j)=cover(i,j)/100.-shrub_cover(i,j)/100.
        if (grass_cover(i,j).lt.0) grass_cover(i,j)=0.

        sm_1(1,i,j)= theta_begin(i,j)*depth_1
!Eva: preliminary set field capacity to 75 % of saturated soil moisture
        field_cap(i,j)=0.75 * theta_sat(i,j)
   enddo
enddo


!set all other variables to zero
et_pot(1,:,:)=0.; et_1(1,:,:)=0.; et_2(1,:,:)=0.
inf_2(1,:,:)=0.; inf_1(1,:,:)=0.
drain_1(1,:,:)=0.; drain_2(1,:,:)=0.; sm_1to2(1,:,:)=0.
sm_sf(1,:,:)=0.; sm_1(1,:,:)=sm_1_init; sm_2(1,:,:)=sm_2_init
qsum_all(:,:,:)=0.; sedtotal_all(:,:,:)=0.
theta_areal_average(:,:)=0.

end
