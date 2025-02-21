module grid_mod
use cons
use random
use interpolate
use data_mod
use mpi
use memory_mod
implicit none
public

public clear_grid
public make_grid
public set_grid_sphere
public set_grid_shell
public set_grid_cloudy



contains

subroutine clear_grid()

call destroy_mem(grid%den)
call destroy_mem(grid%den_d)
call destroy_mem(grid%Tem)
call destroy_mem(grid%v_ran)

call destroy_mem(grid%vx)
call destroy_mem(grid%vy)
call destroy_mem(grid%vz)

call destroy_mem(grid%x)
call destroy_mem(grid%y)
call destroy_mem(grid%z)

call MPI_BARRIER(MPI_COMM_WORLD,ierr)

end subroutine clear_grid

subroutine make_grid()

call create_shared_mem(grid%den, [grid%N_X,grid%N_Y,grid%N_Z])
call create_shared_mem(grid%den_d, [grid%N_X,grid%N_Y,grid%N_Z])
call create_shared_mem(grid%Tem, [grid%N_X,grid%N_Y,grid%N_Z])
call create_shared_mem(grid%v_ran, [grid%N_X,grid%N_Y,grid%N_Z])

call create_shared_mem(grid%vx, [grid%N_X,grid%N_Y,grid%N_Z])
call create_shared_mem(grid%vy, [grid%N_X,grid%N_Y,grid%N_Z])
call create_shared_mem(grid%vz, [grid%N_X,grid%N_Y,grid%N_Z])

call create_shared_mem(grid%x, [grid%N_X+1])
call create_shared_mem(grid%y, [grid%N_Y+1])
call create_shared_mem(grid%z, [grid%N_Z+1])

call MPI_BARRIER(MPI_COMM_WORLD,ierr)

end subroutine make_grid

subroutine set_grid_empty()
use cons
real(kind=rkd) :: vx,vy,vz
real(kind=rkd) :: Xmin,Xmax,Ymax,Ymin,Zmin,Zmax
real(kind=rkd) :: temp1 
real(kind=rkd) :: temp2 
real(kind=rkd) :: temp3 
real(kind=rkd) :: x,y,z 
real(kind=rkd) :: dx,dy,dz 
real(kind=rkd) :: den0 
real(kind=rkd) :: R
integer :: ix,iy,iz
real(kind=rkd) :: open_angle
real(kind=rkd) :: v_rot, Ri_disk, Ro_disk, open_angle_disk, NH_disk, H_disk
real(kind=rkd) :: Rd_disk
real(kind=rkd) :: Ri_wind, Ro_wind
real(kind=rkd) :: AH, beta

R = 1.d0


grid%N_XYZ = 300
grid%N_X = 201
grid%N_Y = 201
grid%N_Z = 201

grid%Ro = R             	! Outer radius
grid%Ri = grid%Ro/100.d0      	! Inner radius

Xmax = grid%Ro*grid%N_X/grid%N_Z
Xmin = -grid%Ro*grid%N_X/grid%N_Z
dX = (Xmax - Xmin)/(grid%N_X) 

Ymax = grid%Ro*grid%N_Y/grid%N_Z
Ymin = -grid%Ro*grid%N_Y/grid%N_Z
dY = (Ymax - Ymin)/(grid%N_Y) 

Zmax = grid%Ro
Zmin = -grid%Ro
dZ = (Zmax - Zmin)/(grid%N_Z) 

grid%max_len = sqrt(dX**2 + dY**2 + dZ**2)

call make_grid()

if(mpar%h_rank .eq. master) then

        grid%den = 0.d0
        grid%vx = 0.d0
        grid%vy = 0.d0
        grid%vz = 0.d0
        grid%x = 0.d0
        grid%y = 0.d0
        grid%z = 0.d0

        grid%Tem = 1.d4
        grid%v_ran = sqrt(2.d0*k*grid%Tem/atom%mass)


        do ix = 1,grid%N_X+1
        grid%X(ix) = Xmin + (ix-1)*dX
        enddo
        do iy = 1,grid%N_Y+1
        grid%Y(iy) = Ymin + (iy-1)*dY
        enddo
        do iz = 1,grid%N_Z+1
        grid%Z(iz) = Zmin + (iz-1)*dZ
        enddo

endif


end subroutine set_grid_empty

subroutine set_grid_cloudy(N_HI,v_ran,v_exp,tau_d)
use cons
use data_mod
use interpolate
real(kind=rkd) :: vx,vy,vz
real(kind=rkd) :: Xmin,Xmax,Ymax,Ymin,Zmin,Zmax
real(kind=rkd) :: temp1, radius 
real(kind=rkd) :: temp2 
real(kind=rkd) :: temp3 
real(kind=rkd) :: x,y,z 
real(kind=rkd) :: dx,dy,dz 
real(kind=rkd) :: den0 
real(kind=rkd) :: R , per
real(kind=rkd) :: v_th, tau_d0, dnu_th
real(kind=rkd), intent(in) :: tau_d, N_HI, v_ran, v_exp
integer :: ix,iy,iz
integer :: count




!       initial position
!	read cloudy output

R = 100.d0*kpc


grid%N_XYZ = 300
grid%N_X = 201
grid%N_Y = 201
grid%N_Z = 201

grid%Ro = R
grid%Ri = grid%Ro/100.d0

Xmax = grid%Ro*grid%N_X/grid%N_Z
Xmin = -grid%Ro*grid%N_X/grid%N_Z
dX = (Xmax - Xmin)/(grid%N_X) 

Ymax = grid%Ro*grid%N_Y/grid%N_Z
Ymin = -grid%Ro*grid%N_Y/grid%N_Z
dY = (Ymax - Ymin)/(grid%N_Y) 

Zmax = grid%Ro
Zmin = -grid%Ro
dZ = (Zmax - Zmin)/(grid%N_Z) 

grid%max_len = sqrt(dX**2 + dY**2 + dZ**2)

count = 0

call make_grid()

if(mpar%h_rank .eq. master) then


        grid%den = 0.d0
        grid%vx = 0.d0
        grid%vy = 0.d0
        grid%vz = 0.d0
        grid%x = 0.d0
        grid%y = 0.d0
        grid%z = 0.d0

        grid%Tem = 1.d4
        grid%T = 1.d4
        grid%v_ran = v_ran 
        v_th =  v_ran 
        dnu_th = v_th/c*atom%nuK
        grid%a =  atom%gamma_K/(4.d0*pi*dnu_th )



        do ix = 1,grid%N_X+1
        grid%X(ix) = Xmin + (ix-1)*dX
        enddo
        do iy = 1,grid%N_Y+1
        grid%Y(iy) = Ymin + (iy-1)*dY
        enddo
        do iz = 1,grid%N_Z+1
        grid%Z(iz) = Zmin + (iz-1)*dZ
        enddo

        do ix = 1, grid%N_X
        x = (grid%x(ix) + grid%x(ix+1))/2.
        do iy = 1, grid%N_Y
        y = (grid%y(iy) + grid%y(iy+1))/2.
        do iz = 1, grid%N_Z
        z = (grid%z(iz) + grid%z(iz+1))/2.
!	radius: radius of cell's center
        radius = sqrt(x**2 + y**2 + z**2)
!	linear interpolarization den_cloudy(radius)

!        if(temp1 .ge. grid%Ri .and. temp1 .le. grid%Ro) then
!	print*, 3, radius, grid%Ro
        if(radius .le. grid%Ro) then
!	print*, 4, radius, grid%Ro	

        grid%den_d(ix,iy,iz) = tau_d / (dust%Cext*(1.d0-dust%albedo)) /(grid%Ro-grid%Ri)
        grid%vx(ix,iy,iz) = (x/grid%Ro)*v_exp
        grid%vy(ix,iy,iz) = (y/grid%Ro)*v_exp
        grid%vz(ix,iy,iz) = (z/grid%Ro)*v_exp


        !grid%den(ix,iy,iz) = N_HI/(grid%Ro-grid%Ri)
        temp1 = radius / kpc
        grid%den(ix,iy,iz) = find_y(temp1,r_int,den_int) 
!	grid%den(ix,iy,iz) = 0 ! test_Version
!	count = (count+1)
	!print*, per
        endif


        enddo
        enddo
        enddo

print*, count

endif





end subroutine set_grid_cloudy

subroutine set_grid_sphere(N_HI,v_ran,v_exp,tau_d)
use cons
real(kind=rkd) :: vx,vy,vz
real(kind=rkd) :: Xmin,Xmax,Ymax,Ymin,Zmin,Zmax
real(kind=rkd) :: temp1 
real(kind=rkd) :: temp2 
real(kind=rkd) :: temp3 
real(kind=rkd) :: x,y,z 
real(kind=rkd) :: dx,dy,dz 
real(kind=rkd) :: den0 
real(kind=rkd) :: R 
real(kind=rkd) :: v_th, tau_d0, dnu_th
real(kind=rkd), intent(in) :: tau_d, N_HI, v_ran, v_exp
integer :: ix,iy,iz , count

R = 100.d0 



grid%N_XYZ = 300
grid%N_X = 201
grid%N_Y = 201
grid%N_Z = 201

grid%Ro = R
grid%Ri = grid%Ro/100.d0

Xmax = grid%Ro*grid%N_X/grid%N_Z
Xmin = -grid%Ro*grid%N_X/grid%N_Z
dX = (Xmax - Xmin)/(grid%N_X) 

Ymax = grid%Ro*grid%N_Y/grid%N_Z
Ymin = -grid%Ro*grid%N_Y/grid%N_Z
dY = (Ymax - Ymin)/(grid%N_Y) 

Zmax = grid%Ro
Zmin = -grid%Ro
dZ = (Zmax - Zmin)/(grid%N_Z) 

grid%max_len = sqrt(dX**2 + dY**2 + dZ**2)

count = 0
call make_grid()

if(mpar%h_rank .eq. master) then

        grid%den = 0.d0
        grid%vx = 0.d0
        grid%vy = 0.d0
        grid%vz = 0.d0
        grid%x = 0.d0
        grid%y = 0.d0
        grid%z = 0.d0

        grid%Tem = 1.d4
        grid%T = 1.d4
        grid%v_ran = v_ran 
        v_th =  v_ran 
        dnu_th = v_th/c*atom%nuK
        grid%a =  atom%gamma_K/(4.d0*pi*dnu_th )



        do ix = 1,grid%N_X+1
        grid%X(ix) = Xmin + (ix-1)*dX
        enddo
        do iy = 1,grid%N_Y+1
        grid%Y(iy) = Ymin + (iy-1)*dY
        enddo
        do iz = 1,grid%N_Z+1
        grid%Z(iz) = Zmin + (iz-1)*dZ
        enddo

        do ix = 1, grid%N_X
        x = (grid%x(ix) + grid%x(ix+1))/2.
        do iy = 1, grid%N_Y
        y = (grid%y(iy) + grid%y(iy+1))/2.
        do iz = 1, grid%N_Z
        z = (grid%z(iz) + grid%z(iz+1))/2.
        temp1 = sqrt(x**2 + y**2 + z**2)


        if(temp1 .ge. grid%Ri .and. temp1 .le. grid%Ro) then

        grid%den_d(ix,iy,iz) = tau_d / (dust%Cext*(1.d0-dust%albedo)) /(grid%Ro-grid%Ri)
        grid%vx(ix,iy,iz) = (x/grid%Ro)*v_exp
        grid%vy(ix,iy,iz) = (y/grid%Ro)*v_exp
        grid%vz(ix,iy,iz) = (z/grid%Ro)*v_exp


        grid%den(ix,iy,iz) = N_HI/(grid%Ro-grid%Ri)
        count = count + 1
	!print*, temp1

        endif


        enddo
        enddo
        enddo

print*, count
endif





end subroutine set_grid_sphere


subroutine set_grid_shell(N_HI,v_ran,v_exp,tau_d)
use cons
real(kind=rkd) :: vx,vy,vz
real(kind=rkd) :: Xmin,Xmax,Ymax,Ymin,Zmin,Zmax
real(kind=rkd) :: temp1 
real(kind=rkd) :: temp2 
real(kind=rkd) :: temp3 
real(kind=rkd) :: x,y,z 
real(kind=rkd) :: dx,dy,dz 
real(kind=rkd) :: den0 
real(kind=rkd) :: R
real(kind=rkd) :: v_th, tau_d0, dnu_th
real(kind=rkd), intent(in) :: tau_d, N_HI, v_ran, v_exp
integer :: ix,iy,iz

R = 1.d0



grid%N_XYZ = 300
grid%N_X = 201
grid%N_Y = 201
grid%N_Z = 201

grid%Ro = R
grid%Ri = grid%Ro*0.9d0

Xmax = grid%Ro*grid%N_X/grid%N_Z
Xmin = -grid%Ro*grid%N_X/grid%N_Z
dX = (Xmax - Xmin)/(grid%N_X) 

Ymax = grid%Ro*grid%N_Y/grid%N_Z
Ymin = -grid%Ro*grid%N_Y/grid%N_Z
dY = (Ymax - Ymin)/(grid%N_Y) 

Zmax = grid%Ro
Zmin = -grid%Ro
dZ = (Zmax - Zmin)/(grid%N_Z) 

grid%max_len = sqrt(dX**2 + dY**2 + dZ**2)

call make_grid()

if(mpar%h_rank .eq. master) then

        grid%den = 0.d0
        grid%vx = 0.d0
        grid%vy = 0.d0
        grid%vz = 0.d0
        grid%x = 0.d0
        grid%y = 0.d0
        grid%z = 0.d0

        grid%Tem = 1.d4
        grid%T = 1.d4
        grid%v_ran = v_ran 
        v_th =  v_ran 
        dnu_th = v_th/c*atom%nuK
        grid%a =  atom%gamma_K/(4.d0*pi*dnu_th )



        do ix = 1,grid%N_X+1
        grid%X(ix) = Xmin + (ix-1)*dX
        enddo
        do iy = 1,grid%N_Y+1
        grid%Y(iy) = Ymin + (iy-1)*dY
        enddo
        do iz = 1,grid%N_Z+1
        grid%Z(iz) = Zmin + (iz-1)*dZ
        enddo

        do ix = 1, grid%N_X
        x = (grid%x(ix) + grid%x(ix+1))/2.
        do iy = 1, grid%N_Y
        y = (grid%y(iy) + grid%y(iy+1))/2.
        do iz = 1, grid%N_Z
        z = (grid%z(iz) + grid%z(iz+1))/2.
        temp1 = sqrt(x**2 + y**2 + z**2)


        if(temp1 .ge. grid%Ri .and. temp1 .le. grid%Ro) then

        grid%den_d(ix,iy,iz) = tau_d / (dust%Cext*(1.d0-dust%albedo)) /(grid%Ro-grid%Ri)
        grid%vx(ix,iy,iz) = (x/temp1)*v_exp
        grid%vy(ix,iy,iz) = (y/temp1)*v_exp
        grid%vz(ix,iy,iz) = (z/temp1)*v_exp


        grid%den(ix,iy,iz) = N_HI/(grid%Ro-grid%Ri)

        endif


        enddo
        enddo
        enddo


endif





end subroutine set_grid_shell


end module grid_mod 
