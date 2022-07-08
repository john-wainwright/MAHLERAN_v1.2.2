
!****************************************************************
!  subroutine to calculate vegetation dynamics
!****************************************************************
subroutine veg_dyn
use vegdynamics_shared_data
implicit none


call growth

call mortality

call dispersal




end



