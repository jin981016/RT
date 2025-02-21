module interpolate
    use data_mod
    implicit none
    public

    ! Public subroutines and functions
    public :: find_y

contains

	function find_y(x,x_i,y_i) result(y_result)
	    use data_mod
	    implicit none 
	    real(8), intent(in) :: x          ! 입력값 r (r0)
	    real(8), allocatable :: x_i(:), y_i(:)          ! 입력할 data_r, data_y 값
	    real(8)  :: y_result          ! interpolate 한 결과
	    integer :: i, imax 
		
		
	    !  Linear Interpolation
	    y_result = 0
	    imax = size(x_i) - 1
	    do i = 1, imax                                                                  
		if (x .ge. x_i(i) .and. x .lt. x_i(i+1) ) then
		    y_result =  (y_i(i+1) - y_i(i)) / (x_i(i+1) - x_i(i)) * (x - x_i(i)) + y_i(i)
		end if
	    end do

	end function find_y
    
end module interpolate

