module scattering_mod
use cons
use random
use voigt_mod
use mpi
use peeling_off_mod
implicit none
public

public scattering
public metal_scattering 
public Lya_scattering 
public dust_scattering 

contains

subroutine scattering(photon)
type(photon_type) :: photon
real(kind=rkd) :: temp, ratio_d
type(photon_type) :: photon_i

temp = rand_number()
ratio_d = photon%tau_dust/(photon%tau_dust + photon%tau_atom)

photon_i = photon

	if(temp .le. ratio_d) then

	call dust_scattering(photon)
!	call peeling_off_dust(photon,photon_i)

	else

	call metal_scattering(photon)
!	call peeling_off_metal(photon,photon_i)

	endif

photon%tau_atom = 0.d0
photon%tau_dust = 0.d0


end subroutine scattering

subroutine metal_scattering(photon)
type(photon_type) :: photon
real(kind=rkd) :: v_th
real(kind=rkd) :: a_K, dnu_th_K
real(kind=rkd) :: a_H, dnu_th_H
real(kind=rkd) :: cost, phi
real(kind=rkd) :: cost2, sint, cosp, sinp
real(kind=rkd) :: r1, r2, temp, norm
real(kind=rkd) :: kxp,kyp,kzp 
real(kind=rkd) :: px, py, pz 
real(kind=rkd) :: vz, nu_r, vel
real(kind=rkd) :: cos2p, sin2p 
real(kind=rkd) :: E1, DoP
real(kind=rkd) :: Ip, Qp, Up, Vp 
real(kind=rkd) :: Q0, U0 
real(kind=rkd) :: S12overS11 
real(kind=rkd) :: dnuH, dnuK 
real(kind=rkd) :: S11, S22, S12, S33, S44


	if(photon%clump .eqv. .false.) then
	v_th = grid%v_ran(photon%ix,photon%iy,photon%iz) 
	else if(photon%clump .eqv. .true.) then
	v_th = clumps%v_th
	endif

	dnu_th_K = v_th/c*atom%nuK
	dnu_th_H = v_th/c*atom%nuH
	a_K = atom%gamma_K/(4.d0*pi*dnu_th_K)
	a_H = atom%gamma_H/(4.d0*pi*dnu_th_H)
!	Update position and number
	photon%x_s = photon%x
	photon%y_s = photon%y
	photon%z_s = photon%z

!
!       K or H line
!
        photon%NS = photon%NS + 1
        temp = atom%f12_K*photon%sigmaK/(atom%f12_K*photon%sigmaK + atom%f12_H*photon%sigmaH)
        if(rand_number() .lt. temp) then
        vz = rand_resonance_vz(photon%xnuK,a_K)
	photon%NS_K = photon%NS_K + 1
        else
        vz = rand_resonance_vz(photon%xnuH,a_H)
	photon%NS_H = photon%NS_H + 1
        endif
!
!       nu_r : Wavelength in Rest Frame of Atom
!
        nu_r = photon%nu/(1.d0 + vz*v_th/c)
        dnuH  = atom%nuH - nu_r
        dnuK  = atom%nuK - nu_r
        E1 = (dnuH**2 + 2.d0*dnuK*dnuH)/(2.d0*dnuH**2 + dnuK**2)

	photon%nu_atom = nu_r
	photon%vz = vz

        vel = v_th*(vz*cost+rand_gauss()*sqrt(1.d0-cost**2)/sqrt(2.d0))
        photon%nu = nu_r/(1.d0 - vel/c)

!
!	cost : cos(theta), theta : angle between k and kp
!
        cost = rand_resonance(E1)
	sint = sqrt(1.d0 - cost**2)	
	cost2 = cost**2
!
!       Calculate Stokes Parameter
!
	S22 = 3.d0/4.d0*E1*(cost2 + 1.d0)
	S11 = S22 + (1.d0 - E1)
	S12 = 3.d0/4.d0*E1*(cost2 - 1.d0)
	S33 = 3.d0/2.d0*E1*cost
	S44 = (E1 + 2.d0)/2.d0*cost
!
!	For Phi!
!
	S12overS11 = S12/S11 
	DoP = sqrt(photon%Q**2 + photon%U**2)
        do 301
        r1 = rand_number()*(1.d0 + DoP*abs(S12overS11))
        r2 = 2.d0*pi*rand_number()
        temp = 1.d0 + S12overS11*(photon%Q*cos(2.d0*r2)+photon%U*sin(2.d0*r2))
        if(r1 .le. temp) then
        phi = r2
        exit
        endif
301     continue

	cosp = cos(phi)
	sinp = sin(phi)
        cos2p = cos(2.d0*phi)
        sin2p = sin(2.d0*phi)
!
!	Update Stokes Vector
!
	Q0 =  cos2p*photon%Q + sin2p*photon%U
	U0 = -sin2p*photon%Q + cos2p*photon%U

	Ip = S11 + S12*Q0 	! S11*I + S12*Q_r
	Qp = S12 + S22*Q0 	! S12*I + S22*U_r
	Up = S33*U0		! S33*U_r
	Vp = S44*photon%V	! S44*V

        photon%I = 1.d0
        photon%Q = Qp/Ip
        photon%U = Up/Ip
        photon%V = Vp/Ip
!
!	Calculate Basis Vector
!
	px = cosp*photon%mx + sinp*photon%nx
	py = cosp*photon%my + sinp*photon%ny
	pz = cosp*photon%mz + sinp*photon%nz

	photon%nx = cosp*photon%nx - sinp*photon%mx
	photon%ny = cosp*photon%ny - sinp*photon%my
	photon%nz = cosp*photon%nz - sinp*photon%mz

	photon%mx = cost*px - sint*photon%kx
	photon%my = cost*py - sint*photon%ky
	photon%mz = cost*pz - sint*photon%kz

	photon%kx = sint*px + cost*photon%kx
	photon%ky = sint*py + cost*photon%ky
	photon%kz = sint*pz + cost*photon%kz

        norm = sqrt(photon%kx**2 + photon%ky**2 + photon%kz**2)
        photon%kx = photon%kx/norm
        photon%ky = photon%ky/norm
        photon%kz = photon%kz/norm


return
end subroutine metal_scattering



subroutine Lya_scattering(photon)
type(photon_type) :: photon
real(kind=rkd) :: Tem
real(kind=rkd) :: v_th, a, dnu_th
real(kind=rkd) :: cost, phi
real(kind=rkd) :: cost2, sint, cosp, sinp
real(kind=rkd) :: r1, r2, temp, norm
real(kind=rkd) :: kxp,kyp,kzp 
real(kind=rkd) :: px, py, pz 
real(kind=rkd) :: vz, nu_r, vel
real(kind=rkd) :: cos2p, sin2p 
real(kind=rkd) :: E1, DoP
real(kind=rkd) :: Ip, Qp, Up, Vp 
real(kind=rkd) :: Q0, U0 
real(kind=rkd) :: S12overS11 
real(kind=rkd) :: dnuH, dnuK 
real(kind=rkd) :: S11, S22, S12, S33, S44

        Tem = grid%Tem(photon%ix,photon%iy,photon%iz)
        v_th = sqrt(2.d0*k*Tem/m_H)
        dnu_th = v_th/c*((nuK+2.d0*nuH)/3.d0)
        a = gamma/(4.d0*pi*dnu_th)

!	Update position and number
	photon%x_s = photon%x
	photon%y_s = photon%y
	photon%z_s = photon%z

        photon%NS = photon%NS + 1
!
!       K or H line
!
        temp = photon%sigmaK/(photon%sigmaK + 2.d0*photon%sigmaH)
        if(rand_number() .lt. temp) then
        vz = rand_resonance_vz(photon%xnuK,a)
        else
        vz = rand_resonance_vz(photon%xnuH,a)
        endif
!
!       nu_r : Wavelength in Rest Frame of Atom
!
        nu_r = photon%nu/(1.d0 + vz*v_th/c)
        dnuH  = nuH - nu_r
        dnuK  = nuK - nu_r
        E1 = (dnuH**2 + 2.d0*dnuK*dnuH)/(2.d0*dnuH**2 + dnuK**2)

	photon%nu_atom = nu_r
	photon%vz = vz

        vel = v_th*(vz*cost+rand_gauss()*sqrt(1.d0-cost**2)/sqrt(2.d0))
        photon%nu = nu_r/(1.d0 - vel/c)

!
!	cost : cos(theta), theta : angle between k and kp
!
        cost = rand_resonance(E1)
	sint = sqrt(1.d0 - cost**2)	
	cost2 = cost**2
!
!       Calculate Stokes Parameter
!
	S22 = 3.d0/4.d0*E1*(cost2 + 1.d0)
	S11 = S22 + (1.d0 - E1)
	S12 = 3.d0/4.d0*E1*(cost2 - 1.d0)
	S33 = 3.d0/2.d0*E1*cost
	S44 = (E1 + 2.d0)/2.d0*cost
!
!	For Phi!
!
	S12overS11 = S12/S11 
	DoP = sqrt(photon%Q**2 + photon%U**2)
        do 301
        r1 = rand_number()*(1.d0 + DoP*abs(S12overS11))
        r2 = 2.d0*pi*rand_number()
        temp = 1.d0 + S12overS11*(photon%Q*cos(2.d0*r2)+photon%U*sin(2.d0*r2))
        if(r1 .le. temp) then
        phi = r2
        exit
        endif
301     continue

	cosp = cos(phi)
	sinp = sin(phi)
        cos2p = cos(2.d0*phi)
        sin2p = sin(2.d0*phi)
!
!	Update Stokes Vector
!
	Q0 =  cos2p*photon%Q + sin2p*photon%U
	U0 = -sin2p*photon%Q + cos2p*photon%U

	Ip = S11 + S12*Q0 	! S11*I + S12*Q_r
	Qp = S12 + S22*Q0 	! S12*I + S22*U_r
	Up = S33*U0		! S33*U_r
	Vp = S44*photon%V	! S44*V

        photon%I = 1.d0
        photon%Q = Qp/Ip
        photon%U = Up/Ip
        photon%V = Vp/Ip
!
!	Calculate Basis Vector
!
	px = cosp*photon%mx + sinp*photon%nx
	py = cosp*photon%my + sinp*photon%ny
	pz = cosp*photon%mz + sinp*photon%nz

	photon%nx = cosp*photon%nx - sinp*photon%mx
	photon%ny = cosp*photon%ny - sinp*photon%my
	photon%nz = cosp*photon%nz - sinp*photon%mz

	photon%mx = cost*px - sint*photon%kx
	photon%my = cost*py - sint*photon%ky
	photon%mz = cost*pz - sint*photon%kz

	photon%kx = sint*px + cost*photon%kx
	photon%ky = sint*py + cost*photon%ky
	photon%kz = sint*pz + cost*photon%kz

return
end subroutine Lya_scattering




subroutine dust_scattering(photon)
type(photon_type) :: photon
real(kind=rkd) :: cost, phi
real(kind=rkd) :: px,py,pz
real(kind=rkd) :: norm
real(kind=rkd) :: Ip, Qp, Up, Vp, Q0, U0
real(kind=rkd) :: S12overS11, two_phi
real(kind=rkd) :: cosp, sinp, sint
real(kind=rkd) :: cos2p, sin2p
real(kind=rkd) :: S11, S12, S33, S34
real(kind=rkd) :: r1, temp
integer :: icos

!	counting number of scattering
	photon%NS = photon%NS + 1
	photon%NS_D = photon%NS_D + 1
!
!	Scattered Wavevector
!	
!	cost = cos(theta) = k_i \cdot k_s
	r1 = rand_number()
	icos = int(r1*(dust%n_phase - 1)) + 1
	if(icos .eq. dust%n_phase) icos = dust%n_phase - 1
	cost = (dust%cthe(icos + 1) - dust%cthe(icos) ) / &
	     (dust%P_cos(icos + 1) - dust%P_cos(icos) ) * &
	     (r1 - dust%P_cos(icos)) + dust%cthe(icos)
	sint = sqrt(1.d0 - cost**2)

	icos = (cost + 1.d0)/(2.d0) * (dust%ncos - 1.d0) + 1

!	if(icos .gt. dust%ncos .or. icos .lt. 0) then
!	print*,cost
!	stop
!	endif

	if(icos .eq. dust%ncos) icos = dust%ncos - 1 
        S11 = ( dust%S11(icos + 1) - dust%S11(icos) )/( dust%cos_scat(icos + 1) - dust%cos_scat(icos) ) &
              *(cost - dust%cos_scat(icos)) + dust%S11(icos)
        S12 = ( dust%S12(icos + 1) - dust%S12(icos) )/( dust%cos_scat(icos + 1) - dust%cos_scat(icos) ) &
              *(cost - dust%cos_scat(icos)) + dust%S12(icos)
        S33 = ( dust%S33(icos + 1) - dust%S33(icos) )/( dust%cos_scat(icos + 1) - dust%cos_scat(icos) ) &
              *(cost - dust%cos_scat(icos)) + dust%S33(icos)
        S34 = ( dust%S34(icos + 1) - dust%S34(icos) )/( dust%cos_scat(icos + 1) - dust%cos_scat(icos) ) &
              *(cost - dust%cos_scat(icos)) + dust%S34(icos)


        S12overS11 = S12/S11 
        do 301
        phi = 2.d0*pi*rand_number()
	two_phi = 2.d0*phi

        r1 = (1.d0 + sqrt(photon%Q**2 + photon%U**2) * abs(S12overS11)) * rand_number()
        temp = 1.d0 + S12overS11 * (photon%Q*cos(two_phi) + photon%U*sin(two_phi))
        if(r1 .le. temp) then
        exit
        endif
301     continue

!
!	Polarization
!       Calculating Stokes Parameter
	cosp  = cos(phi)
	sinp  = cos(phi)
        cos2p = cos(two_phi)
        sin2p = sin(two_phi)

	Q0 =  cos2p*photon%Q + sin2p*photon%U
	U0 = -sin2p*photon%Q + cos2p*photon%U
!	Muller Matrix for dust 
        Ip = S11 + S12*Q0
        Qp = S12 + S11*Q0
        Up = S33*U0 + S34*photon%V
        Vp = S33*photon%V - S34*U0

	photon%I = 1.d0
	photon%Q = Qp/Ip
	photon%U = Up/Ip
	photon%V = Vp/Ip

	photon%weight = photon%weight*dust%albedo
!	if(photon%weight .lt. 1.d-50) photon%esc = .true.
!
!	Calculate Basis Vector
!
	px = cosp*photon%mx + sinp*photon%nx
	py = cosp*photon%my + sinp*photon%ny
	pz = cosp*photon%mz + sinp*photon%nz

	photon%nx = cosp*photon%nx - sinp*photon%mx
	photon%ny = cosp*photon%ny - sinp*photon%my
	photon%nz = cosp*photon%nz - sinp*photon%mz

	photon%mx = cost*px - sint*photon%kx
	photon%my = cost*py - sint*photon%ky
	photon%mz = cost*pz - sint*photon%kz

	photon%kx = sint*px + cost*photon%kx
	photon%ky = sint*py + cost*photon%ky
	photon%kz = sint*pz + cost*photon%kz

	norm = sqrt(photon%kx**2 + photon%ky**2 + photon%kz**2)
!	if(norm .gt. 1.d0) then
!	print*,'normAA'
!	print*,icos
!	print*, photon%NS, photon%NS_d
!	print*,norm, Ip, cost
!	print*,'normBB'
!	endif
	photon%kx = photon%kx/norm
	photon%ky = photon%ky/norm
	photon%kz = photon%kz/norm

end subroutine dust_scattering


end module scattering_mod
