module gen_photon_mod
use cons
use random
use voigt_mod
use mpi
implicit none
public

public gen_photon
public gen_photon_flat
public gen_photon_AGN_con
public gen_photon_delta
public gen_photon_Gaussian
public initial_photon

contains

subroutine initial_photon(photon)
!	isotropic emission
type(photon_type) :: photon
real(kind=rkd) cost,phi
real(kind=rkd) :: cosp, sinp, sint

!       initial wavevector
        cost = 2.d0*rand_number() - 1.d0
        phi = 2.d0*pi*rand_number()
	sint = sqrt(1.d0 - cost**2)
	cosp = cos(phi)
	sinp = sin(phi)
        photon%kx = sint*cosp
        photon%ky = sint*sinp
        photon%kz = cost
        photon%esc = .false.

!
!	For Polarization
!
!       initial basis vector for polarization, n, m
!	cost = -e1
	photon%mx = cost*cosp
	photon%my = cost*sinp
	photon%mz = -sint

	photon%nx = -sinp
	photon%ny = cosp
	photon%nz = 0.d0

!       initial Stokes Parameter
        photon%I = 1.d0
        photon%Q = 0.d0
        photon%U = 0.d0
        photon%V = 0.d0

	
        photon%E3 = 1.0d0
	photon%vel1 = 0.d0
	photon%vel2 = 0.d0


	photon%weight = 1.d0
	photon%NS = 0
	photon%NS_K = 0
	photon%NS_H = 0
	photon%NS_D = 0
	photon%path = 0.d0
	photon%nclump = 0
	photon%clump = .false.
	photon%tau_atom = 0.d0
	photon%tau_dust = 0.d0

end subroutine initial_photon

subroutine gen_photon_Gaussian(photon, v_emit)
!use random
implicit none
type(photon_type) :: photon
real(kind=rkd), intent(in) :: v_emit
real(kind=rkd) vx,vy,vz
real(kind=rkd) vel 
real(kind=rkd) temp
real(kind=rkd) :: v_th



!       initial position
        photon%x = 0.d0
        photon%y = 0.d0
        photon%z = 0.d0
        photon%ix = grid%N_X/2 + 1
        photon%iy = grid%N_Y/2 + 1
        photon%iz = grid%N_Z/2 + 1

        photon%x_s = photon%x
        photon%y_s = photon%y
        photon%z_s = photon%z


!	random velocity in emission region
!	v_emit is sigma of Gaussian distribution
	vel = rand_gauss()*v_emit
!	initial wavelength (frequency)
	temp = rand_number()
	if(temp .le. atom%f12_K/atom%f12 ) then
	photon%nu = atom%nuK/(1.d0-vel/c)
	photon%line = 1
	else
	photon%nu = atom%nuH/(1.d0-vel/c)
	photon%line = 2
	endif

call initial_photon(photon)


end subroutine gen_photon_Gaussian



subroutine gen_photon_delta(photon)
!use random
implicit none
type(photon_type) :: photon
real(kind=rkd) cost,phi
real(kind=rkd) vx,vy,vz
real(kind=rkd) dnuH, dnuK
real(kind=rkd) vel 
real(kind=rkd) radius 
real(kind=rkd) temp,temp1, temp2 
real(kind=rkd) :: cosp, sinp, sint
real(kind=rkd) :: v_th, T 
integer :: ip

ip = photon%ip
T = 1.e4
v_th = sqrt(2.d0*k*T/atom%mass)


!       initial position
        photon%x = 0.d0
        photon%y = 0.d0
        photon%z = 0.d0
        photon%ix = grid%N_X/2 + 1
        photon%iy = grid%N_Y/2 + 1
        photon%iz = grid%N_Z/2 + 1

        photon%x_s = photon%x
        photon%y_s = photon%y
        photon%z_s = photon%z

!       initial wavevector
        cost = 2.d0*rand_number() - 1.d0
        phi = 2.d0*pi*rand_number()
	sint = sqrt(1.d0 - cost**2)
	cosp = cos(phi)
	sinp = sin(phi)
        photon%kx = sint*cosp
        photon%ky = sint*sinp
        photon%kz = cost
        photon%esc = .false.

!
!	For Polarization
!
!       initial basis vector for polarization, n, m
!	cost = -e1
	photon%mx = cost*cosp
	photon%my = cost*sinp
	photon%mz = -sint

	photon%nx = -sinp
	photon%ny = cosp
	photon%nz = 0.d0

!       initial Stokes Parameter
        photon%I = 1.d0
        photon%Q = 0.d0
        photon%U = 0.d0
        photon%V = 0.d0


!	random velocity in emission region
!	v_th : thermal,	v_cir : emission width
	vel = rand_gauss()*v_th
!	vel = vel + rand_gauss()*v_cir

	temp = rand_number()
	if(temp .le. atom%f12_K/atom%f12 ) then
	photon%nu = atom%nuK/(1.d0-vel/c)
	else
	photon%nu = atom%nuH/(1.d0-vel/c)
	endif
	
        photon%E3 = 1.0d0
	photon%vel1 = 0.d0
	photon%vel2 = 0.d0


	photon%weight = 1.d0
	photon%NS = 0
	photon%NS_K = 0
	photon%NS_H = 0
	photon%NS_D = 0
	photon%path = 0.d0
	photon%clump = .false.

end subroutine gen_photon_delta




subroutine gen_photon_AGN_con(photon)
use cons
implicit none
type(photon_type) :: photon
real(kind=rkd) cost,phi
real(kind=rkd) vx,vy,vz
real(kind=rkd) dnuH, dnuK
real(kind=rkd) vel 
real(kind=rkd) radius 
real(kind=rkd) temp1, temp2 
real(kind=rkd) :: cosp, sinp, sint
real(kind=rkd) :: wl
real(kind=rkd) :: R_emit, x,y,z
integer :: ip
real(kind=rkd) :: con_obs, sigma_obs, peak_obs, width_obs

con_obs = 8e3
peak_obs = 6e3
width_obs = 4.03	! km/s
sigma_obs = width_obs/c_km*atom%wlc_0


R_emit = 0.05d0*kpc
ip = photon%ip

!       initial position
	photon%x = 0.d0
	photon%y = 0.d0
	photon%z = 0.d0

	photon%ix = grid%N_X/2 + 1
	photon%iy = grid%N_Y/2 + 1
	photon%iz = grid%N_Z/2 + 1

        photon%x_s = photon%x
        photon%y_s = photon%y
        photon%z_s = photon%z

!       initial wavevector
        cost = 2.d0*rand_number() - 1.d0
        phi = 2.d0*pi*rand_number()
	sint = sqrt(1.d0 - cost**2)
	cosp = cos(phi)
	sinp = sin(phi)
        photon%kx = sint*cosp
        photon%ky = sint*sinp
        photon%kz = cost
        photon%esc = .false.

!
!	For Polarization
!
!       initial basis vector for polarization, n, m
!	cost = -e1
	photon%mx = cost*cosp
	photon%my = cost*sinp
	photon%mz = -sint

	photon%nx = -sinp
	photon%ny = cosp
	photon%nz = 0.d0

!       initial Stokes Parameter
        photon%I = 1.d0
        photon%Q = 0.d0
        photon%U = 0.d0
        photon%V = 0.d0


!	random velocity in emission region
!	v_th : thermal,	v_cir : emission width

	wl = par%wlmin + (par%wlmax - par%wlmin)*rand_number()
	photon%nu = c/(wl*1.d-8)

        photon%E3 = 1.0d0
        dnuH  = atom%nuH - photon%nu
        dnuK  = atom%nuK - photon%nu
        photon%E1 = (dnuH**2 + 2.d0*dnuK*dnuH)/(2.d0*dnuH**2 + dnuK**2)
	photon%vel1 = 0.d0
	photon%vel2 = 0.d0

	photon%weight = 1.d0 + peak_obs/con_obs*exp(-wl**2/2.d0/sigma_obs)
	photon%NS = 0
	photon%path = 0.d0
	photon%clump = .false.

end subroutine gen_photon_AGN_con


subroutine gen_photon_flat(photon)
use cons
implicit none
type(photon_type) :: photon
real(kind=rkd) cost,phi
real(kind=rkd) vx,vy,vz
real(kind=rkd) dnuH, dnuK
real(kind=rkd) vel 
real(kind=rkd) radius 
real(kind=rkd) temp1, temp2 
real(kind=rkd) :: cosp, sinp, sint
real(kind=rkd) :: wl
real(kind=rkd) :: R_emit, x,y,z
integer :: ip

R_emit = 0.05d0*kpc
ip = photon%ip
!v_cir = 200.d5  ! cm/s

!       initial position
       photon%x = 0.d0
       photon%y = 0.d0
       photon%z = 0.d0
!	do 111
!        x = R_emit*(2.d0*rand_number() - 1.d0)
!        y = R_emit*(2.d0*rand_number() - 1.d0)
!        z = R_emit*(2.d0*rand_number() - 1.d0)
!	temp1 = sqrt(x**2 + y**2 + z**2)
!	if(temp1 .le. R_emit) exit
!111	continue

!        photon%x = x
!        photon%y = y
!        photon%z = z

	photon%ix = grid%N_X/2 + 1
	photon%iy = grid%N_Y/2 + 1
	photon%iz = grid%N_Z/2 + 1
!        photon%ix = int((photon%x - grid%X(1))/(grid%X(grid%N_X+1) - grid%X(1))*grid%N_X) + 1
!        photon%iy = int((photon%y - grid%Y(1))/(grid%Y(grid%N_Y+1) - grid%Y(1))*grid%N_Y) + 1
!        photon%iz = int((photon%z - grid%Z(1))/(grid%Z(grid%N_Z+1) - grid%Z(1))*grid%N_Z) + 1


        photon%x_s = photon%x
        photon%y_s = photon%y
        photon%z_s = photon%z

!       initial wavevector
        cost = 2.d0*rand_number() - 1.d0
        phi = 2.d0*pi*rand_number()
	sint = sqrt(1.d0 - cost**2)
	cosp = cos(phi)
	sinp = sin(phi)
        photon%kx = sint*cosp
        photon%ky = sint*sinp
        photon%kz = cost
        photon%esc = .false.

!
!	For Polarization
!
!       initial basis vector for polarization, n, m
!	cost = -e1
	photon%mx = cost*cosp
	photon%my = cost*sinp
	photon%mz = -sint

	photon%nx = -sinp
	photon%ny = cosp
	photon%nz = 0.d0

!       initial Stokes Parameter
        photon%I = 1.d0
        photon%Q = 0.d0
        photon%U = 0.d0
        photon%V = 0.d0


!	random velocity in emission region
!	v_th : thermal,	v_cir : emission width

!       K or H line
!        if(rand_number() .lt. 1.d0/3.d0) then
!        photon%nu = nuK/(1.d0-vel/c)
!        photon%E1 = 0.d0
!        else
!        photon%nu = nuH/(1.d0-vel/c)
!        photon%E1 = 0.5d0
!        endif

	wl = par%wlmin + (par%wlmax - par%wlmin)*rand_number()
	photon%nu = c/(wl*1.d-8)

        photon%E3 = 1.0d0
        dnuH  = atom%nuH - photon%nu
        dnuK  = atom%nuK - photon%nu
        photon%E1 = (dnuH**2 + 2.d0*dnuK*dnuH)/(2.d0*dnuH**2 + dnuK**2)
	photon%vel1 = 0.d0
	photon%vel2 = 0.d0

!	print*,wl,photon%nu/nu_0,photon%E1

!	photon%vel2 = 0.d0
!        photon%vel1 =    photon%kx*grid%vx_grid(photon%ix,photon%iy,photon%iz) &
!                        +photon%ky*grid%vy_grid(photon%ix,photon%iy,photon%iz) &
!                        +photon%kz*grid%vz_grid(photon%ix,photon%iy,photon%iz)

!
!	Initial information
!
!	photon%k_i(1) = photon%kx
!	photon%k_i(2) = photon%ky
!	photon%k_i(3) = photon%kz
!	photon%nu_i =  photon%nu
!	photon%r_i(1) = photon%x
!	photon%r_i(2) = photon%y
!	photon%r_i(3) = photon%z

	photon%weight = 1.d0
	photon%NS = 0
	photon%path = 0.d0
	photon%clump = .false.

end subroutine gen_photon_flat

subroutine gen_photon(photon,v_cir)
!use random
implicit none
type(photon_type) :: photon
real(kind=rkd) cost,phi
real(kind=rkd) vx,vy,vz
real(kind=rkd) dnuH, dnuK
real(kind=rkd) vel 
real(kind=rkd) radius 
real(kind=rkd) temp1, temp2 
real(kind=rkd) :: cosp, sinp, sint
real(kind=rkd), intent(in) :: v_cir
integer :: ip

ip = photon%ip
!v_cir = 200.d5  ! cm/s

!       initial position
        photon%x = 0.d0
        photon%y = 0.d0
        photon%z = 0.d0
        photon%ix = grid%N_X/2 + 1
        photon%iy = grid%N_Y/2 + 1
        photon%iz = grid%N_Z/2 + 1

        photon%x_s = photon%x
        photon%y_s = photon%y
        photon%z_s = photon%z

!       initial wavevector
        cost = 2.d0*rand_number() - 1.d0
        phi = 2.d0*pi*rand_number()
	sint = sqrt(1.d0 - cost**2)
	cosp = cos(phi)
	sinp = sin(phi)
        photon%kx = sint*cosp
        photon%ky = sint*sinp
        photon%kz = cost
        photon%esc = .false.

!
!	For Polarization
!
!       initial basis vector for polarization, n, m
!	cost = -e1
	photon%mx = cost*cosp
	photon%my = cost*sinp
	photon%mz = -sint

	photon%nx = -sinp
	photon%ny = cosp
	photon%nz = 0.d0

!       initial Stokes Parameter
        photon%I = 1.d0
        photon%Q = 0.d0
        photon%U = 0.d0
        photon%V = 0.d0


!	random velocity in emission region
!	v_th : thermal,	v_cir : emission width
	vel = rand_gauss()*grid%v_th
	vel = vel + rand_gauss()*v_cir

!       K or H line
!        if(rand_number() .lt. 1.d0/3.d0) then
!        photon%nu = nuK/(1.d0-vel/c)
!        photon%E1 = 0.d0
!        else
!        photon%nu = nuH/(1.d0-vel/c)
!        photon%E1 = 0.5d0
!        endif

        photon%nu = nu_0/(1.d0-vel/c)

        photon%E3 = 1.0d0
        dnuH  = nuH - photon%nu
        dnuK  = nuK - photon%nu
        photon%E1 = (dnuH**2 + 2.d0*dnuK*dnuH)/(2.d0*dnuH**2 + dnuK**2)
	photon%vel1 = 0.d0
	photon%vel2 = 0.d0

!	photon%vel2 = 0.d0
!        photon%vel1 =    photon%kx*grid%vx_grid(photon%ix,photon%iy,photon%iz) &
!                        +photon%ky*grid%vy_grid(photon%ix,photon%iy,photon%iz) &
!                        +photon%kz*grid%vz_grid(photon%ix,photon%iy,photon%iz)

!
!	Initial information
!
!	photon%k_i(1) = photon%kx
!	photon%k_i(2) = photon%ky
!	photon%k_i(3) = photon%kz
!	photon%nu_i =  photon%nu
!	photon%r_i(1) = photon%x
!	photon%r_i(2) = photon%y
!	photon%r_i(3) = photon%z

	photon%weight = 1.d0
	photon%NS = 0
	photon%path = 0.d0
	photon%clump = .false.

end subroutine gen_photon



end module gen_photon_mod
