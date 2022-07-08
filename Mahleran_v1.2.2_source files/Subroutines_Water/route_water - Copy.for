c****************************************************************
c  subroutine to define water flow routing
c****************************************************************
       subroutine route_water


       use shared_data
       implicit double precision (a - h, o - z)
       implicit integer (i - n)
       logical noiters
       integer *4 sdirin (4, 2)
c
c   REB edit from data sdirin / -1, 0, 1, 0, 0, -1, 0, 1 /
c   to as below
c
c       data sdirin / 1, 0, -1, 0, 0, 1, 0, -1 /
c
c   JW changed E-W so flow directions occur correctly in CN scheme
c
       data sdirin / 1, 0, -1, 0, 0, -1, 0, 1 /
       data gfconst /  78480.d0 /
       data tol / 1.d-8 /
c
c   flow routing dependent on value of iroute:
c   1. routing model after H.M. Scoging (1992)
c   2. FTCS scheme - removed by CJMH Feb 2013
c   3. Lax's method - removed by CJMH Feb 2013
c   4. Two-step Lax-Wendroff method - removed by CJMH Feb 2013
c   5. Crank-Nicolson method with bisection method solution
c
c   n.b. it is preferable to use 5 (most stable) or 1 (conditionally stable)
cJWFeb05   Actually, the above statement is something of a euphemism...
cJWFeb05      FTCS, Lax's method and the two-step Lax-Wendroff give incorrect
cJWFeb05      results if flow velocity varies as a function of depth --
cJWFeb05      which is of course what the Darcy-Weisbach relationship uses to
cJWFeb05      calculate depth!
c
c   VARIABLES:
c      gfconst = 8g for Darcy-Weisbach equation (mm/s/s)
c      d (t, i, j) = flow depth (mm) t=1 for last timestep
c                                    t=2 for current timestep
c      q (t, i, j) = unit discharge (mm2/s)  --"--
c      v (i, j) = flow velocity (mm/s)
c      v1 (i, j) = flow velocity in last timestep (mm/s)
c      ff (i, j) = Darcy-Weisbach friction factor (dimensionless)
c      slope (i, j) = local slope (m/m)
c
       if (iroute.eq.1) then
c
c   Use Scoging method
c
          do i = 2, nr
             do j = 2, nc
                d (2, i, j) = (excess (i, j) * dt + d (1, i, j)) +
     &            ((dtdx) * (add (i, j) - q (1, i, j)))
                if (d (2, i, j).lt.0.) then
                   d (2, i, j) = 0.
                endif
                if (ff_type.eq.3) then
                   ff (i, j) = 14.d0 - 0.08d0 * d (1, i, j)
                   if (ff (i, j).lt.0.1d0) then
                      ff (i, j) = 0.1d0
                   endif
                elseif (ff_type.eq.4) then
                   re = (1.d-6 * v (i, j) * d (1, i, j)) / viscosity
                   if (sed_propn (5, i, j).gt.0.0d0.or.
     &                 sed_propn (6, i, j).gt.0.0d0)
     &                then
                      if (sed_propn (5, i, j).gt.sed_propn (6, i, j))
     &                   then
                         dg = 2.0d0 + 10.0d0 * (sed_propn (5, i, j) /
     &                        (sed_propn (5, i, j) +
     &                         sed_propn (6, i, j)))
                      else
                         dg = 12.0d0 + (20.0d0 * sed_propn (6, i, j))
                      endif
                   else
                      dg = 2.0d0
                   endif
                   if (pave (i, j).ge.0.0d0) then
                      pave_perc = pave (i, j) * 1.d4
                   else
                      pave_perc = 0.0d0
                   endif
                   if (re.gt.0.0d0) then
                      ff (i, j) = 0.0796d0 * 10.0d0 **
     &                            (2.4d-2 * pave_perc) *
     &                            re ** (-0.313d0) * dg ** 0.915d0
                   else
                      ff (i, j) = 0.0796d0 * 10.0d0 **
     &                            (2.4d-2 * pave_perc) * dg ** 0.915d0
                   endif
                   if (ff (i, j).lt.0.1d0) then
                      ff (i, j) = 0.1d0
                   endif
                elseif (ff_type.eq.5) then
                   re = (1.d-6 * v (i, j) * d (1, i, j)) / viscosity
                   if (sed_propn (5, i, j).gt.0.0d0.or.
     &                 sed_propn (6, i, j).gt.0.0d0)
     &                then
                      if (sed_propn (5, i, j).gt.sed_propn (6, i, j))
     &                   then
                         dg = 2.0d0 + 10.0d0 * (sed_propn (5, i, j) /
     &                        (sed_propn (5, i, j) +
     &                         sed_propn (6, i, j)))
                      else
                         dg = 12.0d0 + (20.0d0 * sed_propn (6, i, j))
                      endif
                   else
                      dg = 2.0d0
                   endif
                   if (pave (i, j).ge.0.0d0) then
                      pave_perc = pave (i, j) * 1.d4
                   else
                      pave_perc = 0.0d0
                   endif
                   if (re.gt.0.0d0) then
                      ff (i, j) = 9.143d-6 * (re ** (-0.307d0)) *
     &                            (pave_perc ** 3.470) *
     &                            (dg ** 1.025d0)
                   else
                      ff (i, j) = 9.143d-6 *
     &                            (pave_perc ** 3.470) *
     &                            (dg ** 1.025d0)
                   endif
                   if (ff (i, j).lt.0.1d0) then
                      ff (i, j) = 0.1d0
                   endif
                elseif (ff_type.eq.6) then
                   ff (i, j) = re ** 0.33d0
                   if (ff (i, j).lt.0.1d0) then
                      ff (i, j) = 0.1d0
                   endif
                elseif (ff_type.eq.7) then
c
c  ff = 1.202 Dg^1.383 Q^-0.317
c
                   if (sed_propn (5, i, j).gt.0.0d0.or.
     &                 sed_propn (6, i, j).gt.0.0d0)
     &                then
                      if (sed_propn (5, i, j).gt.sed_propn (6, i, j))
     &                   then
                         dg = 2.0d0 + 10.0d0 * (sed_propn (5, i, j) /
     &                        (sed_propn (5, i, j) +
     &                         sed_propn (6, i, j)))
                      else
                         dg = 12.0d0 + (20.0d0 * sed_propn (6, i, j))
                      endif
                   else
                      dg = 2.0d0
                   endif
                   if (q (2, i, j).gt.0.0d0) then
                      ff (i, j) = 1.202d0 * dg ** 1.383d0 *
     &                            (q (2, i, j) * dx) ** (-0.317d0)
                   else
                    ff (i, j) = 1.202d0 * dg ** 1.383d0
                   endif
                endif

                v (i, j) = sqrt ((gfconst * d (2, i, j) * slope (i, j))
     &                     / ff (i, j))
                courant = dtdx * v (i, j)
                if (courant.gt.1.) then
                   write (6, 9999) courant, i, j, v (i, j), ff (i, j),
     &                             d (2, i, j), excess (i, j),
     &                             slope (i, j)
9999   format (' Courant number ', f7.4, ' at i, j ', 2 (i3, 1x),
     &         ' v= ', e10.4, ' ff= ', f7.4, ' d= ', f7.4, ' xs= ',
     &         f7.4, ' slope= ', f7.4)
                endif
                q (2, i, j) = d (2, i, j) * v (i, j)
             enddo
          enddo

       elseif (iroute.eq.2) then
c
c   Use FTCS method
c
         write (6, *) 'ERROR - the option chosen for water flow routing'
     &      //' (option 2) was previously the'
     &      //' FTCS method which has been removed.'
         write (6, *) 'Choose a different option and run again.'
         write (6, *) '***************************************'
         stop

       elseif (iroute.eq.3) then
c
c   Use Lax's method
c
         write (6, *) 'ERROR - the option chosen for water flow routing'
     &      //' (option 3) was previously the'
     &      //' Lax-Wendroff method which has been removed.'
         write (6, *) 'Choose a different option and run again.'
         write (6, *) '***************************************'
         stop

       elseif (iroute.eq.4) then

         write (6, *) 'ERROR - the option chosen for water flow routing'
     &      //' (option 4) was previously the two-step'
     &      //' Lax-Wendroff method which has been removed.'
         write (6, *) 'Choose a different option and run again.'
         write (6, *) '***************************************'
         stop

       elseif (iroute.eq.5) then
c
c   Use Crank-Nicolson method with bisection method solution
c
          do k = 1, ncell1
             i = order (k, 1)
             j = order (k, 2)
             if (i.ge.2.and.j.ge.2.and.rmask (i, j).ge.0.0d0) then
                qin (2, i, j) = 0.0d0

                do l = 1, 4
                   iin = i + sdirin (l, 1)
                   jin = j + sdirin (l, 2)
                   if (aspect (iin, jin).eq.l.and.
     &                 rmask (iin, jin).ge.0.0d0) then
                        if ( q (2, iin, jin).ge.0.0d0 ) then
                            qin (2, i, j) = qin (2, i, j) +
     &                      q (2, iin, jin)
                        else
                            write(6, 60) iin, jin, q (2, iin, jin)
                            stop
                        endif
                   endif
                enddo
c
c   bisection method for new depth
c
                cnconst = (d (1, i, j) / dt) + ((0.5d0 / dx) *
     &                    qin (2, i, j)) + excess (i, j) -
     &                    ((0.5d0 / dx) * (q (1, i, j) - qin (1, i, j)))
c                write(51,*) '1: cnconst =',cnconst,
c     &                     ' old depth =',d (1, i, j),
c     &                     ' new depth =',d (2, i, j)

                if (cnconst.gt.0.0d0) then
c
c  upper and lower bound estimates for rising limb
c
cJWFeb05   having d (1, i, j) as lower limit causes convergence issues
cJWFeb05     with runon infiltration, so set as zero, even if this will
cJWFeb05     often be less efficient
cJWFeb05              dlow = d (1, i, j)
                   dlow = 0.0d0
                   dhigh = (d (1, i, j) + excess (i, j)) * 100.0d0
                     if (dhigh.eq.0.0d0) then
                         dhigh = 0.5d0
c                         write(51,20) k, i, j, dhigh
                     endif
                     write (52, 20) k, i, j, dhigh
 20     format('cell ',I3,' position (',I3,',',I3,'), dhigh =',D15.8)
                elseif (cnconst.lt.0.0d0) then
c
c  upper and lower bound estimates for falling limb
c
                   dlow = 0.0d0
                   dhigh = d (1, i, j) + excess (i, j)
                     write(6, *) 'Error - RHS<0'
                     stop
                else
c
c  upper and lower bound estimates for steady state
c
                   dlow = 0.0d0
                 dhigh = (d (1, i, j) + excess (i, j)) * 2.0d0
cCJMHJan13 print non-zero values of dhigh to file
                   if (dhigh.ne.0.0d0) then
                         write(51,20) k, i, j, dhigh
                   endif
                endif
cCJMHJan13 initialise dmid before going into loop
                dmid = 0.5d0 * (dlow + dhigh)
              dlow1 = dlow
              dhigh1 = dhigh
cCJMHJan13 changed iteration counter to be initialized to zero instead of iteration = 1
                iteration = 0
                do while ((dhigh - dlow).gt.tol)
cCJMHJan13 moved update of iteration counter to beginning of loop
               iteration = iteration + 1
cCJMHJan13 commented out update of dmid because moved to end of loop
c                     dmid = 0.5d0 * (dlow + dhigh)
cJWFeb05--------
cJWFeb05   add in dynamic ff effects

                   if (ff_type.eq.3) then
                     fflow = 14.d0 - 0.08d0 * dlow
                     ffmid = 14.d0 - 0.08d0 * dmid
                     if (fflow.lt.0.1d0) then
                         fflow = 0.1d0
                     endif
                     if (ffmid.lt.0.1d0) then
                         ffmid = 0.1d0
                     endif
c                      cn1 = 1. / ((1. / dt) + ((0.5d0 * sqrt ((gfconst *
c     &                      dlow * slope (i, j)) / fflow)) / dx))
c                      flow = dlow - cnconst * cn1
c                      cn1 = 1. / ((1. / dt) + ((0.5d0 * sqrt ((gfconst *
c     &                      dmid * slope (i, j)) / ffmid)) / dx))
                   elseif (ff_type.eq.4) then
                      relow = (1.d-6 * v (i, j) * dlow) / viscosity
                      remid = (1.d-6 * v (i, j) * dmid) / viscosity
                      if (sed_propn (5, i, j).gt.0.0d0.or.
     &                    sed_propn (6, i, j).gt.0.0d0)
     &                   then
                         if (sed_propn (5, i, j).gt.sed_propn (6, i, j))
     &                     then
                            dg = 2.0d0 + 10.0d0 * (sed_propn (5, i, j) /
     &                           (sed_propn (5, i, j) +
     &                            sed_propn (6, i, j)))
                         else
                            dg = 12.0d0 + (20.0d0 * sed_propn (6, i, j))
                         endif
                      else
                         dg = 2.0d0
                      endif
                      if (pave (i, j).ge.0.0d0) then
                         pave_perc = pave (i, j) * 1.d4
                      else
                         pave_perc = 0.0d0
                      endif
                      if (relow.gt.0.0d0) then
                         fflow = 0.0796d0 * 10.0d0 **
     &                           (2.4d-2 * pave_perc) *
     &                            relow ** (-0.313d0) * dg ** 0.915d0
                      else
                         fflow = 0.0796d0 * 10.0d0 **
     &                           (2.4d-2 * pave_perc) * dg ** 0.915d0
                      endif
                      if (ffmid.lt.0.1d0) then
                         ffmid = 0.1d0
                      endif
                      if (remid.gt.0.0d0) then
                         ffmid = 0.0796d0 * 10.0d0 **
     &                           (2.4d-2 * pave_perc) *
     &                           remid ** (-0.313d0) * dg ** 0.915d0
                      else
                         ffmid = 0.0796d0 * 10.0d0 **
     &                           (2.4d-2 * pave_perc) * dg ** 0.915d0
                      endif
                      if (ffmid.lt.0.1d0) then
                         ffmid = 0.1d0
                      endif
c                      cn1 = 1. / ((1. / dt) + ((0.5d0 * sqrt ((gfconst *
c     &                      dlow * slope (i, j)) / fflow)) / dx))
c                      flow = dlow - cnconst * cn1
c                      cn1 = 1. / ((1. / dt) + ((0.5d0 * sqrt ((gfconst *
c     &                      dmid * slope (i, j)) / ffmid)) / dx))

                   elseif (ff_type.eq.5) then
                      relow = (1.d-6 * v (i, j) * dlow) / viscosity
                      remid = (1.d-6 * v (i, j) * dmid) / viscosity
                      if (sed_propn (5, i, j).gt.0.0d0.or.
     &                    sed_propn (6, i, j).gt.0.0d0)
     &                   then
                         if (sed_propn (5, i, j).gt.sed_propn (6, i, j))
     &                     then
                            dg = 2.0d0 + 10.0d0 * (sed_propn (5, i, j) /
     &                           (sed_propn (5, i, j) +
     &                            sed_propn (6, i, j)))
                         else
                            dg = 12.0d0 + (20.0d0 * sed_propn (6, i, j))
                         endif
                      else
                         dg = 2.0d0
                      endif
                      if (pave (i, j).ge.0.0d0) then
                         pave_perc = pave (i, j) * 1.d4
                      else
                         pave_perc = 0.0d0
                      endif
                      if (relow.gt.0.0d0) then
                         fflow = 9.143d-6 * (relow ** (-0.307d0)) *
     &                           (pave_perc ** 3.470) *
     &                           (dg ** 1.025d0)
                      else
                         fflow = 9.143d-6 * (pave_perc ** 3.470) *
     &                           (dg ** 1.025d0)
                      endif
                    if (ffmid.lt.0.1d0) then
                         ffmid = 0.1d0
                      endif
                      if (remid.gt.0.0d0) then
                         ffmid = 9.143d-6 * (remid ** (-0.307d0)) *
     &                           (pave_perc ** 3.470) *
     &                           (dg ** 1.025d0)
                      else
                         ffmid = 9.143d-6 * (pave_perc ** 3.470) *
     &                           (dg ** 1.025d0)
                      endif
                      if (ffmid.lt.0.1d0) then
                         ffmid = 0.1d0
                      endif
c                      cn1 = 1. / ((1. / dt) + ((0.5d0 * sqrt ((gfconst *
c     &                      dlow * slope (i, j)) / fflow)) / dx))
c                      flow = dlow - cnconst * cn1
c                      cn1 = 1. / ((1. / dt) + ((0.5d0 * sqrt ((gfconst *
c     &                      dmid * slope (i, j)) / ffmid)) / dx))
                   elseif (ff_type.eq.6) then
                      relow = (1.d-6 * v (i, j) * dlow) / viscosity
                      remid = (1.d-6 * v (i, j) * dmid) / viscosity
                      if (sed_propn (5, i, j).gt.0.0d0.or.
     &                    sed_propn (6, i, j).gt.0.0d0)
     &                   then
                         if (sed_propn (5, i, j).gt.sed_propn (6, i, j))
     &                     then
                            dg = 2.0d0 + 10.0d0 * (sed_propn (5, i, j) /
     &                           (sed_propn (5, i, j) +
     &                            sed_propn (6, i, j)))
                         else
                            dg = 12.0d0 + (20.0d0 * sed_propn (6, i, j))
                         endif
                      else
                         dg = 2.0d0
                      endif
                      if (pave (i, j).ge.0.0d0) then
                         pave_perc = pave (i, j) * 1.d4
                      else
                         pave_perc = 0.0d0
                      endif
                      if (relow.gt.0.0d0) then
                         fflow = relow ** 0.33d0
                      else
                         fflow = 16.17d0
                      endif
                    if (ffmid.lt.0.1d0) then
                         ffmid = 0.1d0
                      endif
                      if (remid.gt.0.0d0) then
                         ffmid = remid ** 0.33d0
                      else
                         ffmid = 16.17
                      endif
                      if (ffmid.lt.0.1d0) then
                         ffmid = 0.1d0
                      endif
c                      cn1 = 1. / ((1. / dt) + ((0.5d0 * sqrt ((gfconst *
c     &                      dlow * slope (i, j)) / fflow)) / dx))
c                      flow = dlow - cnconst * cn1
c                      cn1 = 1. / ((1. / dt) + ((0.5d0 * sqrt ((gfconst *
c     &                      dmid * slope (i, j)) / ffmid)) / dx))
                   elseif (ff_type.eq.7) then
c
c  ff = 1.202 Dg^1.383 Q^-0.317
c
                      if (sed_propn (5, i, j).gt.0.0d0.or.
     &                    sed_propn (6, i, j).gt.0.0d0) then
                         if (sed_propn (5, i, j).gt.sed_propn (6, i, j))
     &                 then
                            dg = 2.0d0 + 10.0d0 * (sed_propn (5, i, j) /
     &                           (sed_propn (5, i, j) +
     &                            sed_propn (6, i, j)))
                         else
                            dg = 12.0d0 + (20.0d0 * sed_propn (6, i, j))
                         endif
                      else
                         dg = 2.0d0
                      endif
                      fflow = 1.202d0 * dg ** 1.383d0 *
     &                            (dlow * v (i, j) * dx) ** (-0.317d0)
                      ffmid = 1.202d0 * dg ** 1.383d0 *
     &                            (dmid * v (i, j) * dx) ** (-0.317d0)
c                      cn1 = 1. / ((1. / dt) + ((0.5d0 * sqrt ((gfconst *
c     &                      dlow * slope (i, j)) / fflow)) / dx))
c                      flow = dlow - cnconst * cn1
c                      cn1 = 1. / ((1. / dt) + ((0.5d0 * sqrt ((gfconst *
c     &                      dmid * slope (i, j)) / ffmid)) / dx))
                   else
cCJMHJan13 moved calculation of flow and cn1 out of if statement so not repeated
cCJMHJan13 added fflow = ffmid = ff (i, j) here:
                         fflow = ff (i, j)
                         ffmid = ff (i, j)

c                      cn1 = 1. / ((1. / dt) + ((0.5d0 * sqrt ((gfconst *
c     &                      dlow * slope (i, j)) / ff (i, j))) / dx))
c                      flow = dlow - cnconst * cn1
                      dmid = 0.5d0 * (dlow + dhigh)
c                      cn1 = 1. / ((1. / dt) + ((0.5d0 * sqrt ((gfconst *
c     &                      dmid * slope (i, j)) / ff (i, j))) / dx))

cJWFeb05--------
                   endif
cCJMHJan13 Moved calculation of flow (and cn1) out of if statement so not repeated
                   cn1 = 1. / ((1. / dt) + ((0.5d0 * sqrt ((gfconst *
     &                      dlow * slope (i, j)) / fflow)) / dx))
                   flow = dlow - cnconst * cn1
                   cn1 = 1. / ((1. / dt) + ((0.5d0 * sqrt ((gfconst *
     &                      dmid * slope (i, j)) / ffmid)) / dx))
                   fmid = dmid - cnconst * cn1
                   if (fmid.eq.0.0d0) then
                      exit
                   endif
                   if ((fmid.lt.0.0d0.and.flow.lt.0.0d0).or.
     &              (fmid.gt.0.0d0.and.flow.gt.0.0d0)) then
                      dlow = dmid
                   else
                      dhigh = dmid
                   endif
cCJMHJan13 update of dmid moved to end of loop so as to include final iteration
                     dmid = 0.5d0 * (dlow + dhigh)
cCJMH Jan13 moved update of iteration counter to beginning of do while loop
c                iteration = iteration + 1
                   if (iteration.gt.10001) then
                     write (6, *) ' CN failed to converge after 10001',
     &                      ' iterations at time ', dt * iter, ' cell ',
     &                      i, j, ' dlow=', dlow, ' dhigh=', dhigh,
     &                      ' initial values: ', dlow1, dhigh1,
     &                      ' difference ', dhigh - dlow, ' tol=', tol
                     write (51, *) ' CN failed to converge after 10001',
     &                      ' iterations at time ', dt * iter, ' cell ',
     &                      i, j, ' dlow=', dlow, ' dhigh=', dhigh,
     &                      ' initial values: ', dlow1, dhigh1,
     &                      ' difference ', dhigh - dlow, ' tol=', tol
                      stop
                   endif
c               end of do while loop (bisection)
                enddo
                d (2, i, j) = dmid

                if (cnconst.eq.0.0d0) then
cCJMH added error trap - d(2,i,j) should be zero if cnconst = 0
                    if (d(2,i,j).ne.0.0d0) then
                        write(51, *) 'Error: cnconst =',cnconst,
     &                     ' old depth =',d (1, i, j)
                        write(51, *) '  iteration no =', iterations,
     &                     ' new depth =',d (2, i, j)
                        write(51, *) '  time =',dt*iter
                    endif

c                  write(51, *) '2: cnconst =',cnconst,
c     &                     ' old depth =',d (1, i, j),
c     &                     ' new depth =',d (2, i, j),' after ',
c     &                      iteration,' iterations'
                else
                  write(52, *) '2: cnconst =',cnconst,
     &                     ' dold =',d (1, i, j),
     &                     ' dnew =',d (2, i, j),' after ',
     &                      'iteration ',iteration
                endif

                if (ff_type.gt.2) then
                   ff (i, j) = fmid
                endif
                if (ff (i, j).lt.0.1d0) then
                   ff (i, j) = 0.1d0
                endif
                v (i, j) = sqrt ((gfconst * dmid * slope (i, j)) /
     &                     ff (i, j))
                q (2, i, j) = d (2, i, j) * v (i, j)
             endif
       enddo
cJWFeb05---------------------------------------------->>>>
       elseif (iroute.eq.6) then
c
c   Use Crank-Nicolson method with automatic infilling/overtopping of pits
c
          do k = 1, ncell1
             i = order (k, 1)
             j = order (k, 2)
             if (i.gt.1.and.j.gt.1.and.rmask (i, j).ge.0.0d0) then
                qin (2, i, j) = 0.0d0
                do l = 1, 4
                   iin = i + sdirin (l, 1)
                   jin = j + sdirin (l, 2)
                   if (aspect (iin, jin).eq.l.and.
     &                rmask (iin, jin).ge.0.0d0) then
                      qin (2, i, j) = qin (2, i, j) + q (2, iin, jin)
                   endif
                enddo
c
c   bisection method for new depth
c
cJWFeb05  use effective depth rather than total depth to account for flow into and
cJWFeb05  out of hollows
cJWFeb05
                effect_depth = d (1, i, j) - d_thresh (i, j)
                if (effect_depth.lt.0.0d0) then
                   effect_depth = 0.0d0
                endif
                cnconst = (effect_depth / dt) + ((0.5d0 / dx) *
     &                    qin (2, i, j)) + excess (i, j) -
     &                 ((0.5d0 / dx) * (q (1, i, j) - qin (1, i, j)))
                write(6,*) 'RHS =',cnconst
cJWFeb05 ---<<<
                if (cnconst.gt.0.0d0) then
c
c  upper and lower bound estimates for rising limb
c
cJWFeb05
                   dlow = 0.0d0
                   dhigh = (effect_depth + excess (i, j)) * 10.0d0
                elseif (cnconst.lt.0.0d0) then
c
c  upper and lower bound estimates for falling limb
c
                   dlow = 0.0d0
cJWFeb05
                   dhigh = effect_depth + excess (i, j)
                else
c
c  upper and lower bound estimates for steady state
c
                   dlow = 0.0d0
cJWFeb05
                   dhigh = (effect_depth + excess (i, j)) * 2.0d0
                endif
                dlow1 = dlow
                dhigh1 = dhigh
                iteration = 1
                do while ((dhigh - dlow).gt.tol)
cJWFeb05--------
cJWFeb05   add in dynamic ff effects
                   dmid = 0.5d0 * (dlow + dhigh)
                   if (ff_type.eq.3) then
                      fflow = 14.d0 - 0.08d0 * dlow
                      ffmid = 14.d0 - 0.08d0 * dmid
                      if (fflow.lt.0.1d0) then
                         fflow = 0.1d0
                      endif
                      if (ffmid.lt.0.1d0) then
                         ffmid = 0.1d0
                      endif
                      cn1 = 1. / ((1. / dt) + ((0.5d0 * sqrt ((gfconst *
     &                      dlow * slope (i, j)) / fflow)) / dx))
                      flow = dlow - cnconst * cn1
                      cn1 = 1. / ((1. / dt) + ((0.5d0 * sqrt ((gfconst *
     &                      dmid * slope (i, j)) / ffmid)) / dx))
                   elseif (ff_type.eq.4) then
                      relow = (1.d-6 * v (i, j) * dlow) / viscosity
                      remid = (1.d-6 * v (i, j) * dmid) / viscosity
                      if (sed_propn (5, i, j).gt.0.0d0.or.
     &                   sed_propn (6, i, j).gt.0.0d0)
     &                   then
                         if (sed_propn (5, i, j).gt.sed_propn (6, i, j))
     &                      then
                            dg = 2.0d0 + 10.0d0 * (sed_propn (5, i, j) /
     &                           (sed_propn (5, i, j) +
     &                           sed_propn (6, i, j)))
                         else
                            dg = 12.0d0 + (20.0d0 * sed_propn (6, i, j))
                         endif
                      else
                         dg = 2.0d0
                      endif
                      if (pave (i, j).ge.0.0d0) then
                         pave_perc = pave (i, j) * 1.d4
                      else
                         pave_perc = 0.0d0
                      endif
                      if (relow.gt.0.0d0) then
                         fflow = 0.0796d0 * 10.0d0 **
     &                           (2.4d-2 * pave_perc) *
     &                           relow ** (-0.313d0) * dg ** 0.915d0
                      else
                         fflow = 0.0796d0 * 10.0d0 **
     &                           (2.4d-2 * pave_perc) * dg ** 0.915d0
                      endif
                    if (ffmid.lt.0.1d0) then
                         ffmid = 0.1d0
                      endif
                      if (remid.gt.0.0d0) then
                         ffmid = 0.0796d0 * 10.0d0 **
     &                           (2.4d-2 * pave_perc) *
     &                           remid ** (-0.313d0) * dg ** 0.915d0
                      else
                         ffmid = 0.0796d0 * 10.0d0 **
     &                           (2.4d-2 * pave_perc) * dg ** 0.915d0
                      endif
                    if (ffmid.lt.0.1d0) then
                         ffmid = 0.1d0
                      endif
                      cn1 = 1. / ((1. / dt) + ((0.5d0 * sqrt ((gfconst *
     &                      dlow * slope (i, j)) / fflow)) / dx))
                      flow = dlow - cnconst * cn1
                      cn1 = 1. / ((1. / dt) + ((0.5d0 * sqrt ((gfconst *
     &                      dmid * slope (i, j)) / ffmid)) / dx))
                   elseif (ff_type.eq.5) then
                      relow = (1.d-6 * v (i, j) * dlow) / viscosity
                      remid = (1.d-6 * v (i, j) * dmid) / viscosity
                      if (sed_propn (5, i, j).gt.0.0d0.or.
     &                   sed_propn (6, i, j).gt.0.0d0)
     &                   then
                         if (sed_propn (5, i, j).gt.sed_propn (6, i, j))
     &                      then
                            dg = 2.0d0 + 10.0d0 * (sed_propn (5, i, j) /
     &                           (sed_propn (5, i, j) +
     &                           sed_propn (6, i, j)))
                         else
                            dg = 12.0d0 + (20.0d0 * sed_propn (6, i, j))
                         endif
                      else
                         dg = 2.0d0
                      endif
                      if (pave (i, j).ge.0.0d0) then
                         pave_perc = pave (i, j) * 1.d4
                      else
                         pave_perc = 0.0d0
                      endif
                      if (relow.gt.0.0d0) then
                         fflow = 9.143d-6 * (relow ** (-0.307d0)) *
     &                            (pave_perc ** 3.470) *
     &                            (dg ** 1.025d0)
                      else
                         fflow = 9.143d-6 * (pave_perc ** 3.470) *
     &                            (dg ** 1.025d0)
                      endif
                    if (ffmid.lt.0.1d0) then
                         ffmid = 0.1d0
                      endif
                      if (remid.gt.0.0d0) then
                         ffmid = 9.143d-6 * (remid ** (-0.307d0)) *
     &                            (pave_perc ** 3.470) *
     &                            (dg ** 1.025d0)
                      else
                         ffmid = 9.143d-6 * (pave_perc ** 3.470) *
     &                            (dg ** 1.025d0)
                      endif
                    if (ffmid.lt.0.1d0) then
                         ffmid = 0.1d0
                      endif
                      cn1 = 1. / ((1. / dt) + ((0.5d0 * sqrt ((gfconst *
     &                      dlow * slope (i, j)) / fflow)) / dx))
                      flow = dlow - cnconst * cn1
                      cn1 = 1. / ((1. / dt) + ((0.5d0 * sqrt ((gfconst *
     &                      dmid * slope (i, j)) / ffmid)) / dx))
                   elseif (ff_type.eq.6) then
                      relow = (1.d-6 * v (i, j) * dlow) / viscosity
                      remid = (1.d-6 * v (i, j) * dmid) / viscosity
                      if (sed_propn (5, i, j).gt.0.0d0.or.
     &                   sed_propn (6, i, j).gt.0.0d0)
     &                   then
                         if (sed_propn (5, i, j).gt.sed_propn (6, i, j))
     &                      then
                            dg = 2.0d0 + 10.0d0 * (sed_propn (5, i, j) /
     &                           (sed_propn (5, i, j) +
     &                           sed_propn (6, i, j)))
                         else
                            dg = 12.0d0 + (20.0d0 * sed_propn (6, i, j))
                         endif
                      else
                         dg = 2.0d0
                      endif
                      if (pave (i, j).ge.0.0d0) then
                         pave_perc = pave (i, j) * 1.d4
                      else
                         pave_perc = 0.0d0
                      endif
                      if (relow.gt.0.0d0) then
                         fflow = relow ** 0.33d0
                      else
                         fflow = 16.17
                      endif
                    if (ffmid.lt.0.1d0) then
                         ffmid = 0.1d0
                      endif
                      if (remid.gt.0.0d0) then
                         ffmid = remid ** 0.33d0
                      else
                         ffmid = 16.17d0
                      endif
                    if (ffmid.lt.0.1d0) then
                         ffmid = 0.1d0
                      endif
                      cn1 = 1. / ((1. / dt) + ((0.5d0 * sqrt ((gfconst *
     &                      dlow * slope (i, j)) / fflow)) / dx))
                      flow = dlow - cnconst * cn1
                      cn1 = 1. / ((1. / dt) + ((0.5d0 * sqrt ((gfconst *
     &                      dmid * slope (i, j)) / ffmid)) / dx))
                   elseif (ff_type.eq.7) then
c
c  ff = 1.202 Dg^1.383 Q^-0.317
c
                      if (sed_propn (5, i, j).gt.0.0d0.or.
     &                    sed_propn (6, i, j).gt.0.0d0) then
                         if (sed_propn (5, i, j).gt.sed_propn (6, i, j))
     &                 then
                            dg = 2.0d0 + 10.0d0 * (sed_propn (5, i, j) /
     &                           (sed_propn (5, i, j) +
     &                            sed_propn (6, i, j)))
                         else
                            dg = 12.0d0 + (20.0d0 * sed_propn (6, i, j))
                         endif
                      else
                         dg = 2.0d0
                      endif
                      fflow = 1.202d0 * dg ** 1.383d0 *
     &                            (dlow * v (i, j) * dx) ** (-0.317d0)
                      ffmid = 1.202d0 * dg ** 1.383d0 *
     &                            (dmid * v (i, j) * dx) ** (-0.317d0)
                      cn1 = 1. / ((1. / dt) + ((0.5d0 * sqrt ((gfconst *
     &                      dlow * slope (i, j)) / fflow)) / dx))
                      flow = dlow - cnconst * cn1
                      cn1 = 1. / ((1. / dt) + ((0.5d0 * sqrt ((gfconst *
     &                      dmid * slope (i, j)) / ffmid)) / dx))
                   else
                      cn1 = 1. / ((1. / dt) + ((0.5d0 * sqrt ((gfconst *
     &                      dlow * slope (i, j)) / ff (i, j))) / dx))
                      flow = dlow - cnconst * cn1
                      dmid = 0.5d0 * (dlow + dhigh)
                      cn1 = 1. / ((1. / dt) + ((0.5d0 * sqrt ((gfconst *
     &                      dmid * slope (i, j)) / ff (i, j))) / dx))
cJWFeb05--------
                   endif
                   fmid = dmid - cnconst * cn1
                   if (fmid.eq.0.0d0) then
                      exit
                   endif
                   if ((fmid.lt.0.0d0.and.flow.lt.0.0d0).or.
     &              (fmid.gt.0.0d0.and.flow.gt.0.0d0)) then
                      dlow = dmid
                   else
                      dhigh = dmid
                   endif

                 iteration = iteration + 1
                   if (iteration.gt.10001) then
                      write (6, *) ' CN failed to converge after 10001',
     &                     '  iterations at time ', dt * iter, ' cell ',
     &                       i, j, ' dlow=', dlow, ' dhigh=', dhigh,
     &                       ' initial values: ', dlow1, dhigh1,
     &                       ' difference ', dhigh - dlow, ' tol=', tol
                      stop
                   endif
                enddo
cJWFeb05
cJWFeb05   add threshold depth back in
cJWFeb05
                if (ff_type.gt.2) then
                   ff (i, j) = fmid
                endif
                d (2, i, j) = dmid + d_thresh (i, j)
                v (i, j) = sqrt ((gfconst * dmid * slope (i, j)) /
     &                     ff (i, j))
cJWFeb05
cJWFeb05   but only discharge based on overflow amount
cJWFeb05
                q (2, i, j) = dmid * v (i, j)

             endif
        enddo
cJWFeb05----------------------------------------------<<<<<<
       elseif (iroute.eq.7) then
cCJMHJan13
c   Use Crank-Nicolson method with Newton's method solution
c
c          Equation is
c               (1/dt)*dnew + (1/(2dx))* sqrt(8g*slope/ff) * dnew^3/2 = cnrhs
c          where
c               cnrhs = (1/dt)*dold
c                     + (1/(2dx))*(qin(new) - q(old) + qin(old)) + excess
c
          cntol = 1.0D-20
c         cntol = tolerance used for checking right hand side of equation before
c         performing Newton-Raphson solution - should always have cnrhs>0 but in
c         practice extremely small negative values sometimes appear, in which case
c         treat the value as zero (provided absolute value < cntol)

          do k = 1, ncell1
             i = order (k, 1)
             j = order (k, 2)
             if (i.ge.2.and.j.ge.2.and.rmask (i, j).ge.0.0d0) then
                qin (2, i, j) = 0.0d0

                do l = 1, 4
                   iin = i + sdirin (l, 1)
                   jin = j + sdirin (l, 2)

                   if (aspect (iin, jin).eq.l.and.
     &                  rmask (iin, jin).ge.0.0d0) then
                        if ( q (2, iin, jin).ge.0.0d0 ) then
                            qin (2, i, j) = qin (2, i, j) +
     &                      q (2, iin, jin)
                        else
                            write(6, 60) iin, jin, q (2, iin, jin)
                            stop
                        endif
                   endif
                enddo
 60             format('q(2,',I4,',',I4,') =',D16.8)
 65             format('cell no ',I4,': q(2,',I4,',',I4,') =',D16.8)
 70             format('d(1,',I4,',',I4,') =',D16.8,' q(1,' I4,',',I4,
     &                 ') =',D16.8)
 71             format('qin(',I1,',',I4,',',I4,') =',D16.8)
 72             format('excess(',I4,',',I4,') =',D16.8)
c
c   Newton's method for new depth
c
                cnrhs = (d (1, i, j) / dt) + (0.5d0 / dx) *
     &                    (qin (2, i, j) - q (1, i, j) + qin (1, i, j))
     &                    + excess (i, j)

c    initialise dnew
                if (cnrhs.lt.0.0D0) then

                    if ( abs(cnrhs).lt.cntol ) then
                        dnew = 0.0d0
                    else
                        write(6, *) 'Error - RHS < 0 in',
     &                  ' Crank-Nicolson / Newton-Raphson routine'
                        write(6, *) 'Value of RHS =',cnrhs
                        write (6, 70) i,j, d (1, i, j), i, j,
     &                                q (1, i, j)
                        write (6, 71) 1,i,j, qin (1, i, j)
                        write (6, 71) 2,i,j, qin (2, i, j)
                        write (6, 72) i,j, excess (i, j)
                        close(52)
                        stop
                    endif

                elseif (cnrhs.eq.0.0d0) then
                    dnew = 0.0d0
                elseif (cnrhs.gt.0.0d0) then
                    imax = 100
c                    dnew = 2*d(1, i, j) + excess (i, j)
                    dnew = d(1, i, j)
                    if (dnew.eq.0) then
                        dnew = 0.5D0
                    endif
                    coeff1 = 1.0D0/dt
                    coeff2 = (0.5D0/dx) * sqrt( gfconst * slope(i,j) /
     &                    ff (i, j))
c                   Equation to solve is thus
c                       coeff1 * dnew + coeff2 * dnew^3/2 = cnrhs
c
c                    write(6, 40) coeff1, coeff2, cnrhs, dnew

 40                 format('coeff1 = ',D15.8,' coeff2 = ',D15.8,
     &              ' RHS = ',D15.8,' dnew =',D15.8)

                    check = coeff1 * dnew + coeff2 * dnew**1.5d0 - cnrhs
                    gradient = coeff1 + 1.5*coeff2*SQRT(dnew)
                    dnew = dnew - (check / gradient)
                    check = coeff1 * dnew + coeff2 * dnew**1.5d0 - cnrhs
                    icount = 1

                    do while (abs(check).gt.tol)
c   calculate friction factor ff - to be added
c   effectively assumes ff(i, j) fixed for now (i.e. fftype = 1)
                        icount = icount + 1
                        gradient = coeff1 + 1.5*coeff2*SQRT(dnew)
                        dnew = dnew - (check / gradient)
                        check = coeff1 * dnew + coeff2*dnew**1.5d0
     &                       - cnrhs
                        if (icount.eq.imax) then
                            write(6, *) 'failed to converge'
                            stop
                        endif
                    enddo
                    write(52, 80) i, j, dnew, check, icount
c
 80                 format('dnew(,',I4,',',I4,')=',D15.8,' check =',
     &              D15.8,' ',I4,' iterations')
c
                else
                    write(6, *) 'Error in C-N routine : RHS =',cnrhs
                    write(6, *) 'Position: ',i,',',j
                    write(6, *) 'd1 =',d (1, i, j)
                    write(6, *) 'quin1 =',qin (1, i, j)
                    write(6, *) 'quin2 =',qin (2, i, j)
                    write(6, *) 'q1 =',q (1, i, j)
                    write(6, *) 'excess =',excess (i, j)
                    write(6, *) 'dt = ',dt,' dx = ',dx
                    stop
                endif
                d(2, i, j) = dnew
                v (i, j) = sqrt ((gfconst * dnew * slope (i, j)) /
     &                     ff (i, j))
                q (2, i, j) = dnew * v (i, j)
             endif
          enddo
       endif

       return
       end
