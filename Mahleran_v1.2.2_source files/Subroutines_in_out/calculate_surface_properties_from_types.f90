subroutine calculate_surface_properties_from_types (data_array, cover_array, n_cols, n_rows, nodata_value, &
                                                    location, scale, distribution)
!     
! File:   calculate_surface_properties_from_types.f90
! Author: John Wainwright
!
! Created on 29 May 2014, 22:35
!
! Parameters of subroutine:
!   data_array:   array of [n_rows, n_cols] double precision values to be calculated from the parameters supplied
!   cover_array:  corresponding array of [n_rows, n_cols] integer values specifying the surface type at the equivalent point  
!   n_cols:       number of columns in the .asc file read in (equivalent to x direction in the model)
!   n_rows:       number of rows in the .asc file read in (equivalent to y direction in the model)
!   nodata_value: value used to specify missing values in the cover, transfered to the data_array 
!   location:     array of [10] location parameters corresponding to the location parameter (mean etc.) of the surface type i 
!   scale:        array of [10] scale parameters corresponding to the location parameter (std dev etc.) of the surface type i
!   distribution: integer giving distribution function used to distribute parameters. Can be:
!                 0: deterministic (location used everywhere)                                                    
!                 1: uniform distribution (location gives minimum, scale gives maximum)     ZBQLUAB(A,B)
!                 2: exponential distribution (location gives mean)                         ZBQLEXP(MU)
!                 3: normal distribution (location gives mean, scale standard deviation)    ZBQLNOR(MU,SIGMA)
!                 4: lognormal distribution 
!                 5: gamma distribution mean G/H and variance G/(H^2)                       ZBQLGAM(G,H)
!                 6: Weibull distribution with shape parameter A and location parameter B   ZBQLWEI(A,B)    
! random-number generation is via the randgen library of Richard Chandler and Paul Northrop
! values are constrained to be non-negative    
!
!

use parameters_from_xml  ! to get access to n_types
                                                    
implicit none

double precision, allocatable, intent(inout) :: data_array (:,:)
double precision, intent (inout) :: nodata_value
double precision, intent (in) :: location (10)
double precision, intent (in) :: scale (10)
double precision :: log_location (10)
double precision :: log_scale (10)
double precision :: ZBQLUAB 
double precision :: ZBQLEXP
double precision :: ZBQLNOR
double precision :: ZBQLGAM 
double precision :: ZBQLWEI

integer, allocatable, intent(inout) :: cover_array (:,:)
integer, intent (inout) :: n_cols
integer, intent (inout) :: n_rows
integer, intent (in) :: distribution
integer :: i, k
integer :: surface_type

if (.not.allocated (data_array)) then
    write (6, *) ' Allocating new space for data '
    allocate (data_array (n_rows, n_cols))
else
    if (size (data_array, 1).ne.n_rows.and.size (data_array, 2).ne.n_cols) then
        write (6, *) ' Error in size of data_array '
        write (6, *) ' File has ', n_cols, ' columns and ', n_rows, &
                          ' rows,\n compared to already allocated ', &
                          ' variable which has ', size (data_array, 1), &
                          ' columns and ', size (data_array, 2), &
                          ' rows'
        stop
    endif
    write (6, *) ' Space already allocated for data '
endif
if (.not.allocated (cover_array)) then
    write (6, *) 'ERROR - cover array must already exist '
    stop
else
    if (size (data_array, 1).ne.size (cover_array, 1).and.size (data_array, 2).ne.size (cover_array, 2)) then
        write (6, *) ' Error reading in allocating surface parameter: '
        write (6, *) ' Data array has ', size (data_array, 1), ' columns and ', size (data_array, 1), &
                          ' rows,\n compared to surface-types array ', &
                          ' variable which has ', size (cover_array, 1), &
                          ' columns and ', size (cover_array, 2), &
                          ' rows'
        stop
    endif
endif

if (distribution.eq.4) then  ! transform for log normal
   do i = 1, n_types
      log_location (surface_type) = log (location (surface_type))
      log_scale (surface_type) = log (scale(surface_type))
   enddo
endif

do i = 1, n_rows
   do k = 1, n_cols
      surface_type = cover_array (i, k) 
!      write (6, *) i, k, location (surface_type), scale (surface_type), distribution
      data_array (i, k) = 0.0d0
      if (surface_type.eq.int (nodata_value)) then
         data_array (i, k) = nodata_value
      else if (surface_type.lt.1) then
         surface_type = 1
      else if (surface_type.gt.n_types) then
         surface_type = n_types
      endif
      if (data_array (i, k).ne.nodata_value) then
         select case (distribution)
             case (0) ! deterministic from location parameter
                data_array (i, k) = location (surface_type)
             case (1) ! uniform distribution (location gives minimum, scale gives maximum)
                do while (data_array (i, k).le.0.)
                   data_array (i, k) = ZBQLUAB (location (surface_type), scale (surface_type))
                enddo
             case (2) ! exponential distribution (location gives mean)                         
                do while (data_array (i, k).le.0.)
                   data_array (i, k) = ZBQLEXP (location (surface_type))
                enddo
             case (3) ! normal distribution (location gives mean, scale standard deviation)    
                do while (data_array (i, k).le.0.)
                   write (6, *) data_array (i, k)
                   if (scale (surface_type).eq.0.0) then
                      data_array (i, k) = location (surface_type)
                   else
                      data_array (i, k) = ZBQLNOR (location (surface_type), scale (surface_type))
                   endif
                   write (6, *) data_array (i, k)
                enddo
             case (4) ! lognormal distribution (location gives mean, scale standard deviation)    
                do while (data_array (i, k).le.0.)
                   data_array (i, k) = exp (ZBQLNOR (location (surface_type), scale(surface_type)))
                enddo
             case (5) ! normal distribution (location gives mean, scale standard deviation)    
                do while (data_array (i, k).le.0.)
                   data_array (i, k) = ZBQLGAM (location (surface_type), scale (surface_type))
                enddo
             case (6) ! Weibull distribution with shape parameter A and location parameter B
                do while (data_array (i, k).le.0.)
                   data_array (i, k) = ZBQLWEI (scale (surface_type), location (surface_type))
                enddo
             case default
                write (6, 9999) 
                write (6, 9998) distribution 
                stop
         end select
      endif
   enddo
enddo

9999 format ('ERROR -- unknown distribution function in calculate_surface_properties_from_types')
9998 format ('Distribution value is: ', i2, ', but should be between 0 and 6')

end subroutine calculate_surface_properties_from_types 

