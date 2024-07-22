module observer_mod
use cons
use memory_mod
use mpi
implicit none
public

public clear_observer
public set_observer
public reduce_observer
public write_file 

contains

subroutine clear_observer()
integer :: i

do i = 1, par%nobserver

!call destroy_mem(observer(i)%w_I_2D)
!call destroy_mem(observer(i)%w_Q_2D)
!call destroy_mem(observer(i)%w_U_2D)
!call destroy_mem(observer(i)%w_V_2D)
call destroy_mem(observer(i)%spec)
call destroy_mem(observer(i)%spec_halo)
call destroy_mem(observer(i)%spec_scat)

call destroy_mem(observer(i)%sb)
call destroy_mem(observer(i)%vel_2d)
call destroy_mem(observer(i)%width_2d)
!call destroy_mem(observer(i)%IFU)

call destroy_mem(observer(i)%sb_r)
call destroy_mem(observer(i)%sb_K_r)
call destroy_mem(observer(i)%sb_H_r)

call destroy_mem(observer(i)%N_total)
call destroy_mem(observer(i)%N_direct)
call destroy_mem(observer(i)%NS)
call destroy_mem(observer(i)%NS_K)
call destroy_mem(observer(i)%NS_H)
call destroy_mem(observer(i)%path)


call MPI_BARRIER(MPI_COMM_WORLD,ierr)

enddo

end subroutine clear_observer


subroutine set_observer()
use mpi
use cons
real(kind=rkd) :: akz
real(kind=rkd) :: vmax, vmin
integer :: i 
real(kind=rkd) :: phi = pi/3.d0
	
	par%Nobserver = 1

	par%nx = 41
	par%ny = 41
!	par%nRp = 10
!	par%nYp = 3
!	par%nslit = 10
	par%nR = 41
	par%nspec = 4e4
!	par%nspec = 1e4
!	par%nspec = 2e3

!	vmin = -5000.d0	!	km/s 
!	vmax =  5000.d0	!	km/s 
	par%wlmin = atom%wlc_0 - 20.d0
	par%wlmax = atom%wlc_0 + 20.d0
!	par%wlmin = atom%wlc_0 - 7.d0
!	par%wlmax = atom%wlc_0 + 10.d0

	if(.not. allocated(observer)) allocate(observer(par%Nobserver))

	call MPI_BARRIER(MPI_COMM_WORLD,ierr)

	observer(1)%kz = cos(pi/180.d0*1.d0)
!	observer(2)%kz = cos(pi/180.d0*89.d0)


!	observer(1)%kz = cos(pi/180.d0*1.d0)
!	observer(2)%kz = cos(pi/180.d0*30.d0)
!	observer(3)%kz = cos(pi/180.d0*60.d0)
!	observer(4)%kz = cos(pi/180.d0*90.d0)
!	observer(5)%kz = cos(pi/180.d0*120.d0)
!	observer(6)%kz = cos(pi/180.d0*150.d0)
!	observer(7)%kz = cos(pi/180.d0*179.d0)


	do i = 1, par%Nobserver

		if(abs(observer(i)%kz) == 1.d0) then

		observer(i)%kx = 0.d0 
		observer(i)%ky = 0.d0

		observer(i)%kNx = -cos(phi)
		observer(i)%kNy = -sin(phi)
		observer(i)%kNz = 0.d0 

		observer(i)%kEx =  sin(phi)*observer(i)%kz
		observer(i)%kEy = -cos(phi)*observer(i)%kz
		observer(i)%kEz = 0.d0

		else

		akz = sqrt(1.d0 - observer(i)%kz**2)
		observer(i)%kx = akz*cos(phi)
		observer(i)%ky = akz*sin(phi)

		observer(i)%kNx = -observer(i)%kx*observer(i)%kz/akz
		observer(i)%kNy = -observer(i)%ky*observer(i)%kz/akz
		observer(i)%kNz = akz  

		observer(i)%kEx =  observer(i)%ky/akz
		observer(i)%kEy = -observer(i)%kx/akz
		observer(i)%kEz = 0.d0
		endif


!		call create_shared_mem(observer(i)%I_2d, [par%nx,par%ny])
!		call create_shared_mem(observer(i)%Q_2d, [par%nx,par%ny])
!		call create_shared_mem(observer(i)%U_2d, [par%nx,par%ny])
!		call create_shared_mem(observer(i)%V_2d, [par%nx,par%ny])
		call create_shared_mem(observer(i)%spec, [par%nspec])
		call create_shared_mem(observer(i)%spec_halo, [par%nspec])
		call create_shared_mem(observer(i)%spec_scat, [par%nspec])

		call create_shared_mem(observer(i)%sb, [par%nx,par%ny])
		call create_shared_mem(observer(i)%vel_2d, [par%nx,par%ny])
		call create_shared_mem(observer(i)%width_2d, [par%nx,par%ny])
!		call create_shared_mem(observer(i)%IFU, [par%nspec,par%nx,par%ny])

		call create_shared_mem(observer(i)%sb_r, [par%nR])
		call create_shared_mem(observer(i)%sb_K_r, [par%nR])
		call create_shared_mem(observer(i)%sb_H_r, [par%nR])

		call create_shared_mem(observer(i)%N_total, [1])
		call create_shared_mem(observer(i)%N_direct, [1])
		call create_shared_mem(observer(i)%NS, [1])
		call create_shared_mem(observer(i)%NS_K, [1])
		call create_shared_mem(observer(i)%NS_H, [1])
		call create_shared_mem(observer(i)%path, [1])



	enddo


end subroutine set_observer


subroutine reduce_observer()
integer :: i

do i = 1, par%Nobserver


!	call reduce_mem(observer(i)%I_2d)
!	call reduce_mem(observer(i)%Q_2d)
!	call reduce_mem(observer(i)%U_2d)
!	call reduce_mem(observer(i)%V_2d)

	call reduce_mem(observer(i)%spec)
	call reduce_mem(observer(i)%spec_halo)
	call reduce_mem(observer(i)%spec_scat)

	call reduce_mem(observer(i)%sb)
	call reduce_mem(observer(i)%vel_2d)
	call reduce_mem(observer(i)%width_2d)
!	call reduce_mem(observer(i)%IFU)

	call reduce_mem(observer(i)%sb_r)
	call reduce_mem(observer(i)%sb_K_r)
	call reduce_mem(observer(i)%sb_H_r)

	call reduce_mem(observer(i)%N_total)
	call reduce_mem(observer(i)%N_direct)
	call reduce_mem(observer(i)%NS)
	call reduce_mem(observer(i)%NS_K)
	call reduce_mem(observer(i)%NS_H)
	call reduce_mem(observer(i)%path)

	call MPI_BARRIER(MPI_COMM_WORLD,ierr)

enddo

return
end subroutine


subroutine write_file()
use char_mod
integer :: i, j, ix, iy
real(kind=rkd) :: wl, dVel
character(len=100) :: fn
character(len=100) :: fn1, fn2, fn3, fn4
character(len=100) :: fn_sb1, fn_sb2
real(kind=rkd) :: x, y, dX, dY
real(kind=rkd) :: flux, Qp, Up, Vp 
real(kind=rkd) :: sb, vel_2d, width_2d 
real(kind=rkd) :: avg, avg2 
real(kind=rkd) :: sb_r, sb_r_K, sb_r_H, dR, R, area

dR = grid%Ro/kpc/(par%nR - 1.d0)

dX = 2.d0/par%nx
dY = 2.d0/par%nx


if(mpar%rank == 0) then
do i = 1, par%Nobserver

!
!	Statics
!
        write(fn,111) fn_model, '_static',i,'.dat'
        call space_char(fn)
        open(21,file=fn)
        write(21,411)   observer(i)%N_total(1),observer(i)%N_direct(1), &
                        observer(i)%NS(1),observer(i)%NS_K(1),observer(i)%NS_H(1),observer(i)%path(1)
411     format(<6>ES16.7)
        close(21)


!
!	Spectrum
!
	write(fn,111) fn_model, '_spec',i,'.dat'
111	format(A,A,I2,A)
	call space_char(fn)
	open(21,file=fn)

		do j = 1, par%nspec
			wl = par%wlmin + (par%wlmax-par%wlmin)*real(j)/par%nspec
!			dVel = (wl/atom%wl0_c - 1.d0)*c_km
		write(21,211) wl, observer(i)%spec(j), observer(i)%spec_halo(j), observer(i)%spec_scat(j)
211		format(<4>ES16.7)

	enddo
	close(21)
!
!	Radial
!
	write(fn,111) fn_model,'_radi',i,'.dat'
	call space_char(fn)
	open(21,file=fn)

	do j = 1, par%nR

	R = (j - 1.d0)/(par%nR - 1.d0)*grid%Ro/kpc

	if(j .eq. 1) then
	area = pi*(0.5d0*dR)**2
	else if(j .eq. par%nR) then
	area = pi*(2.d0*R+0.5d0*dR)*0.5d0*dR
	else
	area = 2.d0*pi*R*dR
	endif

	sb_r = observer(i)%sb_r(j)/area
	sb_r_H = observer(i)%sb_H_r(j)/area
	sb_r_K = observer(i)%sb_K_r(j)/area

	write(21,211) R, sb_r, sb_r_H, sb_r_K

	enddo

	close(21)
!
!	For 2D image
!
	write(fn,111) fn_model,'_sb',i,'.dat'
	call space_char(fn)
	open(21,file=fn)
!	write(fn,111) fn_model,'_IFU',i,'.dat'
!	call space_char(fn)
!	open(22,file=fn)

	do ix = 1, par%nx
        x = -1 + dX*(ix-1) + dX/2.d0
	do iy = 1, par%ny
        y = -1 + dY*(iy-1) + dY/2.d0

		if(observer(i)%sb(ix,iy) .eq. 0.0) then
		sb = 0.d0
		vel_2d = 0.d0
		width_2d = 0.d0
		else
		sb = observer(i)%sb(ix,iy) 
		avg = observer(i)%vel_2d(ix,iy)/observer(i)%sb(ix,iy)
		avg2 = observer(i)%width_2d(ix,iy)/observer(i)%sb(ix,iy)
		vel_2d   = (avg/atom%wlc_0 - 1.d0)*c_km
		width_2d = sqrt(avg2 - avg**2)/atom%wlc_0*c_km

!		Qp   = observer(i)%Q_2d(ix,iy)/observer(i)%I_2d(ix,iy)
!		Up   = observer(i)%U_2d(ix,iy)/observer(i)%I_2d(ix,iy)
!		Vp   = observer(i)%V_2d(ix,iy)/observer(i)%I_2d(ix,iy)
		endif

	write(21,123) x, y, sb, width_2d, vel_2d 
123	format(<5>ES16.7)
!
!		IFU
!
!	do j = 1, par%nspec
!		wl = par%wlmin + (par%wlmax-par%wlmin)*real(j)/par%nspec

!	write(22,124) x,y,wl, observer(i)%IFU(j,ix,iy)
!124	format(<4>ES16.7)
!	enddo

	enddo
	enddo
	close(21)
	close(22)

enddo	! observer
endif

call MPI_BARRIER(MPI_COMM_WORLD,ierr)


end subroutine write_file
end module observer_mod
