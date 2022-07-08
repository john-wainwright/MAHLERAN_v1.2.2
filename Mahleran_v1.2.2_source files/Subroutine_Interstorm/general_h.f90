
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module time_h
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
save
! time of start/end of simulation; assigned in interstorm.dat via read_interstorm_parameters [-]
INTEGER :: tstart, tstop, mstart, mstop
! year; assigned in read_interstorm_parameters [yr]
INTEGER :: t
! month assigned in calcyear [month]
INTEGER :: m
! day (in year)
INTEGER :: d, dprev
! day since simulation start; assigned in read_interstorm_parameters [-]
INTEGER :: dtot
! days in year; assigned in calcyear [-]
INTEGER :: dayyear, daylastyear, dayoutsim
! days in months
INTEGER :: daymon(12)

! number of normal years (no leap year) in calculation period
INTEGER*1 nos
! number of years in calculation period
INTEGER*1 years
INTEGER :: idold,idmon,mnew
INTEGER :: mon_day(12)
!COMMON /maintime/ tstart, tstop, mstart, mstop, t, m, d,  &
!    dprev, dtot, dayyear, daylastyear, idold, idmon, mnew, nos, years
!COMMON /timedat/ daymon,daynr,monnr,mon_day
end module time_h



