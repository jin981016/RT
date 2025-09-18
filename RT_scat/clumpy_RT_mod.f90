module clumpy_RT_mod 
use cons
use random
use voigt_mod
use mpi
use tau_edge_mod
use scattering_mod
use peeling_off_mod
implicit none

public

public RT_in_clump

contains



subroutine RT_in_clump(photon)
type(photon_type), intent(inout) :: photon
real(kind=rkd) :: x,y,z,dl,tau
real(kind=rkd) :: xp,yp,zp
real(kind=rkd) :: x_pc,y_pc,z_pc
integer :: ix,iy,iz 
real(kind=rkd) :: R, L_cl
real(kind=rkd) :: temp,temp1,temp2,temp3
real(kind=rkd) :: vel
real(kind=rkd) :: D_c
real(kind=rkd) :: sigma_atom 
real(kind=rkd) :: dtau_atom, dtau_dust
real(kind=rkd) :: v_th, dnu_th_K, dnu_th_H 
real(kind=rkd) :: a_K, a_H 
integer :: NS
real(kind=rkd) :: nu_i
integer :: iclump

iclump = photon%iclump
R = clumps%r
photon%nclump = photon%nclump + 1
ix = photon%ix
iy = photon%iy
iz = photon%iz

NS = 0

vel = 	  photon%kx*clumps%vx(iclump) &
	+ photon%ky*clumps%vy(iclump) &
	+ photon%kz*clumps%vz(iclump)

nu_i = photon%nu
photon%nu = photon%nu*(1.d0 - vel/c + photon%vel1/c) 

photon%tau_atom = 0.d0
photon%tau_dust = 0.d0

if(photon%overlap .eqv. .false.) then

xp = photon%x + photon%kx*photon%D_cl
yp = photon%y + photon%ky*photon%D_cl
zp = photon%z + photon%kz*photon%D_cl

x = xp - clumps%x(iclump)
y = yp - clumps%y(iclump)
z = zp - clumps%z(iclump)

else if(photon%overlap .eqv. .true.) then
!	Move Photon -\hat k direction

x = photon%x - clumps%x(iclump)
y = photon%y - clumps%y(iclump)
z = photon%z - clumps%z(iclump)

                x_pc = x
                y_pc = y
                z_pc = z

                D_c   = sqrt(    (-photon%ky*z_pc + photon%kz*y_pc)**2 &
                               + (-photon%kz*x_pc + photon%kx*z_pc)**2 &
                               + (-photon%kx*y_pc + photon%ky*x_pc)**2 )

		temp = sqrt(x_pc**2 + y_pc**2 + z_pc**2)
		temp1 = sqrt(R**2 - D_c**2)
		temp2 = sqrt(temp**2 - D_c**2)
!               if k hat and r_pc hat are really similar
		if(isnan(temp2) .eqv. .true.) temp2 = 0.d0
		temp3 = - photon%kx*x - photon%ky*y - photon%kz*z 
!		value of temp2 with sign of temp3
		dl = temp1 + sign(temp2,-temp3)
		x = x - photon%kx*dl
		y = y - photon%ky*dl
		z = z - photon%kz*dl


endif

do 101


	v_th = clumps%v_th 
	dnu_th_K = v_th/c*atom%nuK
	dnu_th_H = v_th/c*atom%nuH
	a_K = atom%gamma_K/(4.d0*pi*dnu_th_K)
	a_H = atom%gamma_H/(4.d0*pi*dnu_th_H)
	photon%xnuK = (photon%nu-atom%nuK)/dnu_th_K
	photon%xnuH = (photon%nu-atom%nuH)/dnu_th_H
	photon%sigmaK = voigt(photon%xnuK,a_K)/dnu_th_K
	photon%sigmaH = voigt(photon%xnuH,a_H)/dnu_th_H
	sigma_atom = atom%sigma_0*(atom%f12_K*photon%sigmaK + atom%f12_H*photon%sigmaH)/atom%f12
	dtau_atom = sigma_atom*clumps%den
	dtau_dust = dust%Cext*clumps%den_d

	tau = -dlog(rand_number())
	dl = tau/(dtau_atom + dtau_dust)

	xp = x + photon%kx*dl
	yp = y + photon%ky*dl
	zp = z + photon%kz*dl
	temp = sqrt(xp**2 + yp**2 + zp**2)


	if(temp .lt. R) then
!	IN CLUMP
		photon%x = xp + clumps%x(iclump)
		photon%y = yp + clumps%y(iclump)
		photon%z = zp + clumps%z(iclump)
		photon%tau_atom = dl*dtau_atom
		photon%tau_dust = dl*dtau_dust
		call scattering(photon)
		NS = NS + 1

		x = xp
		y = yp
		z = zp
		photon%path = photon%path + dl

	else
!	OUTSIDE
                x_pc = x
                y_pc = y
                z_pc = z

                D_c   = sqrt(    (photon%ky*z_pc - photon%kz*y_pc)**2 &
                               + (photon%kz*x_pc - photon%kx*z_pc)**2 &
                               + (photon%kx*y_pc - photon%ky*x_pc)**2 )

		temp = sqrt(x_pc**2 + y_pc**2 + z_pc**2)
		temp1 = sqrt(R**2 - D_c**2)

		temp2 = sqrt(temp**2 - D_c**2)
!		if k hat and r_pc hat are really similar
		if(isnan(temp2) .eqv. .true.) temp2 = 0.d0
		temp3 = photon%kx*x_pc + photon%ky*y_pc + photon%kz*z_pc
!		value of temp2 with sign of temp3
		dl = temp1 + sign(temp2,-temp3)


		xp = x + photon%kx*dl
		yp = y + photon%ky*dl
		zp = z + photon%kz*dl
		photon%path = photon%path + dl
		exit

	endif

101 continue

		photon%x = xp + clumps%x(iclump)
		photon%y = yp + clumps%y(iclump)
		photon%z = zp + clumps%z(iclump)
		photon%overlap = .false.
		photon%clump = .false.

	if(NS .gt. 0) then

	vel = 	  photon%kx*clumps%vx(iclump) &
		+ photon%ky*clumps%vy(iclump) &
		+ photon%kz*clumps%vz(iclump)

	photon%vel1 = photon%kx*grid%vx(ix,iy,iz) &
		    + photon%ky*grid%vy(ix,iy,iz) &
		    + photon%kz*grid%vz(ix,iy,iz)

	photon%nu = photon%nu*(1.d0 + vel/c - photon%vel1/c) 

	else

	photon%nu = nu_i

	endif


end subroutine RT_in_clump



end module clumpy_RT_mod
