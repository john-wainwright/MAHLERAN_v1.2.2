!****************************************************************
subroutine initialize_allocatables
!****************************************************************

use shared_data

implicit none

integer :: i, j, k

write (6, *) ' About to initialize allocatables'
do i = 1, nr2
   do j = 1, nc2
      slope (i, j) = 0.0d0
      profc (i, j) = 0.0d0
      planc (i, j) = 0.0d0
      aspect (i, j) = 0
      veg (i, j) = 0.0d0
!LTOct2007 added in initialise theta_0 and theta_sat
      theta_0 (i, j) = 0.0d0
      theta_sat (i, j) = 0.0d0
      sminit (i, j) = 0.0d0
      ciinit (i, j) = 0.0d0
      is_rill (i, j) = 0
      do k = 1, 2

!LTOct2007 here k varies from 1 to 2. If K = 1 value from previous time step
! if k = 2, value updated in current time step.
         d (k, i, j) = 0.0d0
         q (k, i, j) = 0.0d0
         qin (k, i, j) = 0.0d0
! need to change it where the balance eq. is all this does is reset all allocatable variables to 0 when 1st set up.
         ammonium (k, i, j) = 0.0d0
         nitrate (k, i, j) = 0.0d0
         phosphorus (k, i, j) = 0.0d0
      enddo
      do phi = 1, 6
         v_soil (phi, i, j) = 0.0d0
         detach_soil (phi, i, j) = 0.0d0
         depos_soil (phi, i, j) = 0.0d0
         add_soil (phi, i, j) = 0.0d0
         amm_q (phi, i, j) = 0.0d0
         nit_q (phi, i, j) = 0.0d0
         TN_q (phi, i, j) = 0.0d0
         TP_q (phi, i, j) = 0.0d0
         IC_q (phi, i, j) = 0.0d0
         TC_q (phi, i, j) = 0.0d0
         do k = 1, 2
            d_soil (phi, k, i, j) = 0.0d0
            q_soil (phi, k, i, j) = 0.0d0
         enddo
      enddo
      v (i, j) = 0.0d0
      excess (i, j) = 0.0d0
      cum_inf (i, j) = 0.0d0
      cum_drain (i, j) = 0.0d0
      stmax (i, j) = 0.0d0
      r2 (i, j) = 0.0d0
      add (i, j) = 0.0d0
      addammonium (i,j) = 0.0d0
      addnitrate (i,j) = 0.0d0
      addphosphorus (i,j) = 0.0d0
      y_amm (i, j) = 0.0d0              !LTOct07 y_amm, y_nit and y_phos
      y_nit (i, j) = 0.0d0              !        are the fluxes
      y_phos (i, j) = 0.0d0
      theta (i, j) = 0.0d0
      dup (i, j) = 0.0d0
      v1 (i, j) = 0.0d0
      qsum (i, j) = 0.0d0
      dmax (i, j) = 0.0d0
      vmax (i, j) = 0.0d0
      dmax_soil (i, j) = 0.0d0
      vmax_soil (i, j) = 0.0d0
      ks (i, j) = 0.0d0
      psi (i, j) = 0.0d0
!      ff (i, j) = 0.0d0
      drain_par (i, j) = 0.0d0
      ksat (i, j) = 0.0d0
      pave (i, j) = 0.0d0
      rmask (i, j) = 0.0d0
      do phi = 1, 6
         sed_propn (phi, i, j) = 0.0d0
         sed_temp (phi, i, j) = 0.0d0
      enddo
      splash_tot (i, j) = 0.0d0
      sedch (i, j) = 0.0d0
      zs (i, j) = 0.0d0
      sed_tot (i, j) = 0.0d0
      detach_tot (i, j) = 0.0d0
      depos_tot (i, j) = 0.0d0
      net_erosion_season (i, j) = 0.0d0
!
!LTOct2007 added in particulate nutrients
!
      amm_tot (i, j) = 0.0d0
      nit_tot (i, j) = 0.0d0
      TN_tot (i, j) = 0.0d0
      TP_tot (i, j) = 0.0d0
      IC_tot (i, j) = 0.0d0
      TC_tot (i, j) = 0.0d0

      raindrop_detach_tot (i, j) = 0.0d0
      flow_detach_tot (i, j) = 0.0d0
!      z (i, j) = 0.0d0
      area (i, j) = 0.0d0
      contrib (i, j) = 0
      d_thresh (i, j) = 0.0d0
   enddo
enddo
write (6, *) ' Completed initialize allocatables - returning'

return
end
