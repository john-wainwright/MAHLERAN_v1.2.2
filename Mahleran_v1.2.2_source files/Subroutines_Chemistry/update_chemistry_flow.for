c****************************************************************
c  subroutine to update chemistry flow routing variables
c****************************************************************
       subroutine update_chemistry_flow

       use shared_data
       implicit double precision (a - h, o - z)
       implicit integer (i - n)

       do i = 2, nr
          do j = 2, nc
             ammonium (1, i, j) = ammonium (2, i, j)
             nitrate (1, i, j) = nitrate (2, i, j)
             phosphorus (1, i, j) = phosphorus (2, i, j)
           ammonium (2, i, j) = 0
             nitrate (2, i, j) = 0
             phosphorus (2, i, j) = 0

        enddo
       enddo

       return
       end
