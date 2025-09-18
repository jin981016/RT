module dust_mod
use cons
use memory_mod
use mpi
implicit none

public set_dust
public clear_dust

contains

subroutine clear_dust()
        call destroy_mem(dust%cos_scat)
        call destroy_mem( dust%S11)
        call destroy_mem( dust%S12)
        call destroy_mem( dust%S33)
        call destroy_mem( dust%S34)

        call destroy_mem(dust%P_cos)
        call destroy_mem(dust%cthe)
end subroutine clear_dust

subroutine set_dust(fn)
character(len=*), intent(in) :: fn
real(kind=rkd), allocatable :: S11_int(:)
integer :: i, icos, idata

open(newunit=unit, file=fn, status='old',iostat=info)
read(unit,*)
read(unit,*) dust%wl, dust%Cext, dust%albedo, dust%cos_avg, dust%ncos

!if(mpar%rank .eq. master) print*, dust%wl, dust%Cext, dust%albedo, dust%cos_avg, dust%ncos

	call create_shared_mem(dust%cos_scat, [dust%ncos])
	call create_shared_mem(dust%S11, [dust%ncos])
	call create_shared_mem(dust%S12, [dust%ncos])
	call create_shared_mem(dust%S33, [dust%ncos])
	call create_shared_mem(dust%S34, [dust%ncos])

	call create_shared_mem(dust%P_cos, [dust%n_phase])
	call create_shared_mem(dust%cthe, [dust%n_phase])

	if(mpar%h_rank .eq. master) then

		if(.not. allocated(S11_int)) then
		allocate(S11_int(dust%ncos))
		S11_int = 0.d0
		endif

		read(unit,*)
		do idata = 1, dust%ncos
		read(unit,*) dust%cos_scat(idata), dust%S11(idata), dust%S12(idata), dust%S33(idata), dust%S34(idata)
!if(mpar%rank .eq. master) print*, dust%cos_scat(idata), dust%S11(idata), dust%S12(idata), dust%S33(idata), dust%S34(idata)
			if(idata .eq. 1) then
			S11_int(1) = 0.d0
			else
			S11_int(idata) = S11_int(idata - 1) &
			+ 0.5d0 * (dust%S11(idata) + dust%S11(idata-1)) * &
				  (dust%cos_scat(idata) - dust%cos_scat(idata-1))
			endif
		enddo


	
		do icos = 1, dust%n_phase
		dust%P_cos(icos) = (icos - 1.d0)/(dust%n_phase - 1.d0)

			do i =  1, dust%ncos - 1
		       
			if(dust%P_cos(icos) .ge. S11_int(i) .and. dust%P_cos(icos) .le. S11_int(i+1)) then
			dust%cthe(icos) = (dust%cos_scat(i+1) - dust%cos_scat(i)) / &
					  (S11_int(i+1) - S11_int(i)) * &
					  (dust%P_cos(icos) - S11_int(i)) + dust%cos_scat(i)
        		exit
       			endif
			enddo
		enddo

	endif


close(unit)

call MPI_BARRIER(MPI_COMM_WORLD,ierr)



end subroutine set_dust

end module dust_mod 
