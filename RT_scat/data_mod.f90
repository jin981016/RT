module data_mod
    implicit none

    ! public 변수 선언
    public :: r_int, e_int, den_int, px_int, p_max, initialize_data, tt

    ! 전역 변수 및 배열
    real(8), allocatable :: r_int(:), e_int(:), den_int(:), px_int(:)
    real(8) :: tt, p_max

contains

    ! 데이터 초기화 서브루틴
    subroutine initialize_data()
        implicit none
        character :: filename*200
        character(len=200) :: line_int
        integer :: i, j, nline_int, line_number
	filename = 'CIV_cloudy.txt'
        ! 파일 열기
        open(31, file=filename)

        ! 파일의 줄 수 계산
        j = 0
        do
            read(31, '(a)', end=200) line_int
            j = j + 1
        end do
200     continue
        close(31)

        nline_int = j - 1
        
        
         if (allocated(r_int)) deallocate(r_int)
    	 if (allocated(e_int)) deallocate(e_int)
   	 if (allocated(den_int)) deallocate(den_int)
   	 if (allocated(px_int)) deallocate(px_int)
	
        ! 배열 할당
        allocate(r_int(nline_int))
        allocate(e_int(nline_int))
        allocate(den_int(nline_int))
        allocate(px_int(nline_int))

        ! 데이터 읽기
        open(31, file=filename)
        do i = 1, nline_int
            read(31, *) r_int(i), e_int(i), den_int(i)
        end do
        close(31)

        ! emit_int 계산 및 p_max 설정
        tt = sum(e_int)
        do i = 1, nline_int
            px_int(i) = e_int(i) / tt
        end do
        p_max = maxval(px_int)

    end subroutine initialize_data

end module data_mod

