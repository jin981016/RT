module clumps_mod
use cons
use random
use mpi
use memory_mod
use grid_mod
implicit none
public

public set_clumps
public set_clumps_no
public clear_clumps



contains

subroutine clear_clumps()
integer :: ix,iy,iz

call destroy_mem(clumps%x)
call destroy_mem(clumps%y)
call destroy_mem(clumps%z)
call destroy_mem(clumps%vx)
call destroy_mem(clumps%vy)
call destroy_mem(clumps%vz)
call destroy_mem(clumps%N)
call destroy_mem(clumps%init_i)

call MPI_BARRIER(MPI_COMM_WORLD,ierr)

end subroutine clear_clumps

subroutine set_clumps_no()
use cons

call create_shared_mem(clumps%N, [grid%N_X,grid%N_Y,grid%N_Z])
call create_shared_mem(clumps%init_i, [grid%N_X,grid%N_Y,grid%N_Z])
clumps%N = 0.d0

call MPI_BARRIER(mpar%h_comm,ierr)

end subroutine set_clumps_no


subroutine set_clumps(f_c, NH_total, v_exp_cl, ratio_r_cl, v_ran, tau_d, v_cl)
use cons
real(kind=rkd) :: vx,vy,vz
real(kind=rkd) :: Xmin,Xmax,Ymax,Ymin,Zmin,Zmax
real(kind=rkd) :: temp
real(kind=rkd) :: temp1 
real(kind=rkd) :: temp2 
real(kind=rkd) :: temp3 
real(kind=rkd) :: x,y,z 
real(kind=rkd) :: dx,dy,dz 
real(kind=rkd) :: den0 
real(kind=rkd) :: R
real(kind=rkd) :: NH_cl
!real(kind=rkd) :: Tem
real(kind=rkd), intent(in) :: f_c, NH_total, v_exp_cl, ratio_r_cl, v_ran, tau_d, v_cl
integer :: ix,iy,iz
!	For clumps
real(kind=rkd) :: N_cl_total
integer :: N_cl, N_cl_r
integer :: iclump, i_init, i_final
real(kind=rkd) :: r_cl, clumps_ratio, temp_c
real(kind=rkd) :: xc,yc,zc
real(kind=rkd) :: tau_d_cl 

NH_cl = 3.0d0/4.0d0*NH_total/f_c
tau_d_cl = 3.d0/4.d0*tau_d/f_c

r_cl = grid%Ro*ratio_r_cl 
clumps%r   = r_cl
clumps%den   = NH_cl/r_cl
!clumps%den_d = (tau_d/f_c)/(dust%Cext*(1.d0-dust%albedo))/r_cl 
clumps%den_d = tau_d_cl/(dust%Cext*(1.d0-dust%albedo))/r_cl 
!Tem = 1.d4
!clumps%v_th = sqrt(2.d0*k*Tem/atom%mass)
clumps%v_th = v_ran 



dZ = grid%z(2) - grid%z(1)
dY = grid%y(2) - grid%y(1)
dX = grid%x(2) - grid%x(1)

N_cl_total = f_c*4.d0/3.d0/r_cl**2*grid%Ro**2


call MPI_BARRIER(MPI_COMM_WORLD,ierr)

call create_shared_mem(clumps%N, [grid%N_X,grid%N_Y,grid%N_Z])
call create_shared_mem(clumps%init_i, [grid%N_X,grid%N_Y,grid%N_Z])

if(mpar%h_rank .eq. master) then

clumps%N_total = 0
iclump = 0
do ix = 1, grid%N_X
x = (grid%x(ix) + grid%x(ix+1))/2.
do iy = 1, grid%N_Y
y = (grid%y(iy) + grid%y(iy+1))/2.
do iz = 1, grid%N_Z
z = (grid%z(iz) + grid%z(iz+1))/2.

temp1 = sqrt(x**2 + y**2 + z**2)
clumps_ratio = N_cl_total/(4./3.*pi*grid%Ro**3)*(dX*dY*dZ)
N_cl = int(clumps_ratio)
clumps_ratio = clumps_ratio - N_cl

	if(temp1 .le. grid%Ro .and. temp1 .ge. grid%Ri ) then

		temp_c = rand_number()
		if(temp_c .le. clumps_ratio) then
		N_cl_r = N_cl + 1
		else
		N_cl_r = N_cl
		endif

		clumps%init_i(ix,iy,iz) = clumps%N_total + 1
		clumps%N(ix,iy,iz) = N_cl_r
		clumps%N_total = clumps%N_total + N_cl_r




	else


	endif

enddo
enddo
enddo

endif

call MPI_BARRIER(MPI_COMM_WORLD,ierr)


clumps%N_total = 0
do ix = 1, grid%N_X
do iy = 1, grid%N_Y
do iz = 1, grid%N_Z
clumps%N_total = clumps%N_total + clumps%N(ix,iy,iz)
enddo
enddo
enddo
                                                                                                   

call MPI_BARRIER(MPI_COMM_WORLD,ierr)


call create_shared_mem(clumps%x, [clumps%N_total])
call create_shared_mem(clumps%y, [clumps%N_total])
call create_shared_mem(clumps%z, [clumps%N_total])
call create_shared_mem(clumps%vx, [clumps%N_total])
call create_shared_mem(clumps%vy, [clumps%N_total])
call create_shared_mem(clumps%vz, [clumps%N_total])

	

call MPI_BARRIER(MPI_COMM_WORLD,ierr)

!	Set grid in shared memory
if(mpar%h_rank .eq. master) then


        do ix = 1, grid%N_X
        x = (grid%x(ix) + grid%x(ix+1))/2.
        do iy = 1, grid%N_Y
        y = (grid%y(iy) + grid%y(iy+1))/2.
        do iz = 1, grid%N_Z
        z = (grid%z(iz) + grid%z(iz+1))/2.
!        temp1 = sqrt(x**2 + y**2 + z**2)

!	if(temp1 .le. grid%Ro .and. temp1 .ge. grid%Ri) then


	if(clumps%N(ix,iy,iz) .gt. 0) then
		i_init  = clumps%init_i(ix,iy,iz)
		i_final = i_init + clumps%N(ix,iy,iz) - 1

		do iclump = i_init, i_final

                do 101
                xc = x + (2.d0*rand_number() - 1.d0)*(dX/2.d0 - r_cl)
                yc = y + (2.d0*rand_number() - 1.d0)*(dY/2.d0 - r_cl)
                zc = z + (2.d0*rand_number() - 1.d0)*(dZ/2.d0 - r_cl)
                temp_c = sqrt(xc**2 + yc**2 + zc**2)
                if(temp_c .gt. r_cl) exit
                101     continue

                clumps%x(iclump) = xc
                clumps%y(iclump) = yc
                clumps%z(iclump) = zc

                clumps%vx(iclump) = xc/grid%Ro*v_exp_cl + v_cl*rand_gauss()
                clumps%vy(iclump) = yc/grid%Ro*v_exp_cl + v_cl*rand_gauss()
                clumps%vz(iclump) = zc/grid%Ro*v_exp_cl + v_cl*rand_gauss()

		enddo
	endif


!        endif

        enddo
        enddo
        enddo


endif


call MPI_BARRIER(mpar%h_comm,ierr)

end subroutine set_clumps

end module clumps_mod 
