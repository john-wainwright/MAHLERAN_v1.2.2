!****************************************************************
!subroutine to read shrub cover
!
!
!***************************************************************
subroutine read_shrub_cov_xml

!
!       ncols [x]
!       nrows [y]
!       xllcorner [xmin]
!       yllcorner [ymin]
!       cellsize [dx]
!       nodata_value [nodata]
!
!  followed by row 1 etc. of the data

use shared_data
use interstorm_shared_data
use parameters_from_xml

implicit none

character (len = 160) :: filename

double precision :: xllcorner, yllcorner, cellsize, nodata_value

integer :: n_cols, n_rows

interface
    subroutine read_spatial_data (filename, data_array, n_cols, n_rows, xllcorner, yllcorner, cellsize, nodata_value)
        implicit none

        double precision, allocatable, intent(inout) :: data_array (:,:)
        double precision, intent (out) :: xllcorner, yllcorner, cellsize
        double precision, intent (out) :: nodata_value
       
        integer, intent (out) :: n_cols
        integer, intent (out) :: n_rows
        integer i, k
       
        logical fexist
       
        character (len=*) :: filename
        character *14 a_col, a_row, a_dx, a_minx, a_miny, a_nodata
        character *80 a_line
    end subroutine read_spatial_data 
end interface

filename = interstorm_input_folder (1: interstorm_input_folder_length) // shrub_cover_map

! type of shrub_cover has been changed in interstorm_shared_data to allow call to standard routine
call read_spatial_data (filename, shrub_cover, n_cols, n_rows, xllcorner, yllcorner, cellsize, nodata_value)

write (6, *)
write (6, *) ' shrub cover data read in '

return
end
