c****************************************************************
c  subroutine to define infiltration and runoff
c****************************************************************
       subroutine infilt

       use shared_data
       implicit double precision (a - h, o - z)
       implicit integer (i - n)

       double precision par_a, par_b, lambda, time
       real constant
       data gfconst /  78480.d0 /
c
c   infiltration model after R.E. Smith and J.-Y. Parlange (1978)
c      'A parameter-efficient hydrologic infiltration model'
c      Water Resources Research 14(3), 533-538.
c   VARIABLES:
c      gfconst = 8g for Darcy-Weisbach equation (mm/s/s)
c      ks (i, j) = Ksat (mm/s)
c      c = soil suction constant (mm2/s)
c      def = local variable for calculations
c      f = potential infiltration rate (mm/s)
c      water_in = potential water for infiltration (rain + ponding) (mm/s)
c      excess (i, j) = runoff generated in timestep
c      cum_inf (i, j) = cumulative infiltration to present (mm)
c      theta (i, j) = present soil moisture content (mm3/mm3)
c      psi (i, j) = wetting front suction (mm)
c
c   this version contains feedback between water depth and suction
c
c
c
cJWFeb05
cJWFeb05          do i = 1, nr1
cJWFeb05             do j = 1, nc1
cJWFeb05   don't calculate edges nor areas outwith plot area
cJWFeb05
       time = (iter * dt) / 60.0d0  !minutes
       do i = 2, nr
          do j = 2, nc

            cum_inf_p (i, j) = cum_inf (i, j)
             if (rmask (i, j).ge.0.0d0) then
                iveg = 1
                if (inf_type.lt.5) then
c
c  if inf_model = 1, used ksat (i, j) directly
c
                 if (inf_model.eq.1) then
                      if (ksat (i, j).ge.0.0d0) then
cJWMay05
cJWMay05   multiply through by calibration factor ksat_mod
cJWMay05
                         final_infilt = ksat (i, j) * ksat_mod
                      else
                         final_infilt = 1.0d-10
                      endif
                 else
c
c   otherwise use version of Hawkins' exponential based on pavement cover and rainfall intensity
c
                      if (pave (i, j).gt.0.0d0) then
                         lambda = -0.022891667 * log (pave (i, j)) -
     &                            0.098575
                      else
                         lambda = 0.16d0
                      endif
                       final_infilt = lambda  * (1 -
     &                                  exp (-r2 (i, j) / lambda))
                 endif
cLTOCT2007 changed
c                c = (psi (i, j) + d (2, i, j)) * (theta_sat (iveg) -      to;
                   c = (psi (i, j) + d (2, i, j)) * (theta_sat (i, j) -
     &                 theta (i, j)) * final_infilt
                     def1 = (cum_inf (i, j) * final_infilt) / c
                   if (def1.le.100.) then
                      def = exp (def1)
                      f = (final_infilt * def) / (def - 1.)
                 else
                      f = final_infilt
                   endif

                   water_in = r2 (i, j) + (d (1, i, j) / dt)
c     Eva: drain is in [mm/s] at the moment, but it is being compared to cum_inf [mm]

cLT_QUESTION Drain keeps water draining out of the soil to reduce soil moisture content during and between event.
cLT_ANSWER   Linear storage model. Bit of a fudge
cLTOct2007 change
cLTOct2007         drain = (theta (i, j) / theta_sat (iveg)) *      to;
                   drain = (theta (i, j) / theta_sat (i, j)) *
     &                     (ksat (i, j) * drain_par (i, j) * dt)

                   if (f.ge.water_in) then
c     complete runon occurs
c   REB changed excess (i, j) = 0.
                      excess (i, j) = 0.
                      d (1, i, j) = 0.
                      q (1, i, j) = 0.
                      v (i, j) = 0.
                      cum_inf (i, j) = cum_inf (i, j) + (water_in * dt)
                      if (cum_inf (i, j).ge.drain) then
                         cum_inf (i, j) = cum_inf (i, j) - drain
                      else
                         drain = cum_inf (i, j)
                         cum_inf (i, j) = 0.
                      endif

c   no runon occurs - infiltration totally satisfied by rainfall
                   elseif (f.le.r2 (i, j)) then
                      excess (i, j) = r2 (i, j) - f
                      cum_inf (i, j) = cum_inf (i, j) + (f * dt)
                      if (cum_inf (i, j).ge.drain) then
                         cum_inf (i, j) = cum_inf (i, j) - drain
                      else
                         drain = cum_inf (i, j)
                         cum_inf (i, j) = 0.
                      endif

cb  (i.e. if f < water_in and f > r2(i,j))
c   partial runon - infiltration satisfied by rainfall and some runon
                 else
                      excess (i, j) = 0.
                      f1 = f - r2 (i, j)
                      d (1, i, j) = d (1, i, j) - (f1 * dt)
                      if (inf_type.eq.5) then
                         if (ff (i, j).lt.0.1d0) then
                            ff (i, j) = 0.1d0
                         endif
                      endif
                      v (i, j) = sqrt ((gfconst * d (1, i, j) *
     &                           slope (i, j)) / ff (i, j))
                      q (1, i, j) = d (1, i, j) * v (i, j)
                      cum_inf (i, j) = cum_inf (i, j) + (f * dt)
                      if (cum_inf (i, j).ge.drain) then
                         cum_inf (i, j) = cum_inf (i, j) - drain
                      else
                         drain = cum_inf (i, j)
                         cum_inf (i, j) = 0.
                      endif
                   endif
                else
                   f = (ksat (i, j) + psi (i, j) / time) / 60.0d0
                   if (f.le.r2 (i, j)) then
                      excess (i, j) = r2 (i, j) - f
                      cum_inf (i, j) = cum_inf (i, j) + (f * dt)
                      if (cum_inf (i, j).ge.drain) then
                         cum_inf (i, j) = cum_inf (i, j) - drain
                      else
                         drain = cum_inf (i, j)
                         cum_inf (i, j) = 0.
                      endif
                 else
                      excess (i, j) = 0.
                      cum_inf (i, j) = cum_inf (i, j) + (water_in * dt)
                      if (cum_inf (i, j).ge.drain) then
                         cum_inf (i, j) = cum_inf (i, j) - drain
                      else
                         drain = cum_inf (i, j)
                         cum_inf (i, j) = 0.
                      endif
                   endif
              endif
c     produces saturation-excess runoff
                if (cum_inf (i, j).gt.stmax (i, j)) then
                   excess (i, j) = excess (i, j) + (cum_inf (i, j) -
     &                             stmax (i, j)) / dt
                   cum_inf (i, j) = stmax (i, j)
                endif

c
c     Updating of soil moisture
cEva: I think the following line is wrong:
c             theta (i, j) = cum_inf (i, j) / stmax (i, j)   (stmax is soil thickness at a point)
cEva: instead it should be divided only by soil thickness (mm)
c
cJWMay05                theta (i, j) = cum_inf (i, j) / (soil_thick (1) * 1000.)
                theta (i, j) = cum_inf (i, j) / stmax (i, j) *
     &                       theta_sat(i,j)
                cum_drain (i, j) = cum_drain (i, j) + drain

             else
cJWFeb05
cJWFeb05   no excess generated outwith the plot area
cJWFeb05
                excess (i, j) = 0.0d0
             endif
          enddo
       enddo

       return
       end
