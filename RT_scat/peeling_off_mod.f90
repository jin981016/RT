module peeling_off_mod
use cons
use random
use voigt_mod
use mpi
use tau_edge_mod
implicit none

public

public peeling_off_direct_hydrogen
public peeling_off_metal
public peeling_off_direct_metal
public collecting_photon
!public peeling_off_dust

contains

subroutine collecting_photon(photon)
use cons
type(photon_type),intent(in) :: photon
real(kind=rkd) :: kNx, kNy, kNz
real(kind=rkd) :: kEx, kEy, kEz
real(kind=rkd) :: cos_obs, sin_obs, cos_2obs, sin_2obs
real(kind=rkd) :: wl
real(kind=rkd) :: Ip, Qp, Up, Vp
real(kind=rkd) :: xp,yp,Rp
real(kind=rkd) :: nu
real(kind=rkd) :: dwl 
real(kind=rkd) :: temp, temp1, temp2 
integer :: ix,iy,iR
integer :: ispec, iRp, islit
integer :: i

	i = par%iobs

	observer(i)%N_total = observer(i)%N_total + photon%weight                                                                        
	observer(i)%path = observer(i)%path + photon%path/grid%Ro*photon%weight
	if(photon%NS .eq. 0) then
	observer(i)%N_direct = observer(i)%N_direct + photon%weight
	else
	observer(i)%NS = observer(i)%NS + photon%NS*photon%weight
	observer(i)%NS_K = observer(i)%NS_K + photon%NS_K*photon%weight
	observer(i)%NS_H = observer(i)%NS_H + photon%NS_H*photon%weight
	endif


        kNx = observer(i)%kNx 
        kNy = observer(i)%kNy 
        kNz = observer(i)%kNz 

        kEx = observer(i)%kEx 
        kEy = observer(i)%kEy 
        kEz = observer(i)%kEz 

!	if(photon%NS .eq. 0) then
!	Ip = 1.d0
!	Qp = 0.d0 
!	Up = 0.d0
!	Vp = 0.d0
!	xp = 0.d0
!	yp = 0.d0
!	else
        cos_obs = photon%mx*kEx + photon%my*kEy + photon%mz*kEz
        sin_obs = photon%kx*(photon%my*kEz - photon%mz*kEy) &
                + photon%ky*(photon%mz*kEx - photon%mx*kEz) & 
                + photon%kz*(photon%mx*kEy - photon%my*kEx)
!	m = -e1
	cos_obs = - cos_obs
	sin_obs = - sin_obs
        cos_2obs = cos_obs**2 - sin_obs**2
        sin_2obs = 2.d0*sin_obs*cos_obs 

        Ip = 1.d0
        Qp = ( cos_2obs*photon%Q + sin_2obs*photon%U)/photon%I
        Up = (-sin_2obs*photon%Q + cos_2obs*photon%U)/photon%I
        Vp = photon%V/photon%I

	xp = kEx*photon%x + kEy*photon%y
	yp = kNx*photon%x + kNy*photon%y + kNz*photon%z
	xp = xp/grid%Ro
	yp = yp/grid%Ro
!	endif


	ix = int(par%nx/2.d0*(xp + 1) + 1)                                      
	iy = int(par%ny/2.d0*(yp + 1) + 1)

	nu = photon%nu*(1.d0 + photon%vel2/c)
        wl = c/nu*1.d8
        ispec = (wl - par%wlmin)/(par%wlmax - par%wlmin)*par%nspec

!
!	For 2D image
!
	if(ix .ge. 1 .and. ix .le. par%nx) then
	if(iy .ge. 1 .and. iy .le. par%ny) then
!		observer(i)%I_2d(ix,iy) = observer(i)%I_2d(ix,iy) +    photon%weight
!		observer(i)%Q_2d(ix,iy) = observer(i)%Q_2d(ix,iy) + Qp*photon%weight
!		observer(i)%U_2d(ix,iy) = observer(i)%U_2d(ix,iy) + Up*photon%weight
!		observer(i)%V_2d(ix,iy) = observer(i)%V_2d(ix,iy) + Vp*photon%weight
		observer(i)%sb(ix,iy) = observer(i)%sb(ix,iy) + photon%weight
		observer(i)%vel_2d(ix,iy) = observer(i)%vel_2d(ix,iy) + wl*photon%weight
		observer(i)%width_2d(ix,iy) = observer(i)%width_2d(ix,iy) + wl**2*photon%weight

		Rp  = sqrt(xp**2 + yp**2)
	        iR = nint(Rp*(par%nR-1)) + 1
	        observer(i)%sb_r(iR) = observer(i)%sb_r(iR) + photon%weight
		if(wl .lt. wlc_0) then
	        observer(i)%sb_K_r(iR) = observer(i)%sb_K_r(iR) + photon%weight
		else
	        observer(i)%sb_H_r(iR) = observer(i)%sb_H_r(iR) + photon%weight
		endif


!		if(ispec .ge. 1 .and. ispec .le. par%nspec) then
!		observer(i)%IFU(ispec,ix,iy) =observer(i)%IFU(ispec,ix,iy) + photon%weight
!		endif
	endif
	endif


!
!	For Spectrum
!
	if(ispec .ge. 1 .and. ispec .le. par%nspec) then

		observer(i)%spec(ispec) = observer(i)%spec(ispec) + photon%weight
		if(photon%NS .gt. 0) then
		observer(i)%spec_scat(ispec) = observer(i)%spec_scat(ispec) + photon%weight
		endif

		Rp  = sqrt(xp**2 + yp**2)
		if(Rp*grid%Ro .ge. grid%Ro/10.d0) then
		observer(i)%spec_halo(ispec) = observer(i)%spec_halo(ispec) + photon%weight
			if(photon%NS .eq. 0) then
			print*,'error??',photon%clump
			print*,Rp,xp,yp
			print*,photon%x/grid%Ro,photon%y/grid%Ro,photon%z/grid%Ro
			print*,photon%x_s/grid%Ro,photon%y_s/grid%Ro,photon%z_s/grid%Ro
			print*,photon%weight,photon%I
			stop
			endif
		endif

	endif

return
end subroutine collecting_photon



subroutine peeling_off_direct_hydrogen(photon)
use cons
type(photon_type), intent(in) :: photon
type(photon_type) :: photon_p
real(kind=rkd) :: tau_edge
integer :: i 

!	print*,'direct'

	do i = 1, par%Nobserver

	par%iobs = i
	photon_p = photon

	photon_p%kx = observer(i)%kx
	photon_p%ky = observer(i)%ky
	photon_p%kz = observer(i)%kz

        call tau_to_edge_hydrogen(photon_p, tau_edge)
!
!       exp(-tau_edge)  : optical depth                                         
!       photon%weigt    : weight of incident photon
!       1/(4.*pi)      : isotropic emission 
	if(tau_edge .le. tau_max) then
        photon_p%weight = exp(-tau_edge)/(4.d0*pi)*photon%weight
	call collecting_photon(photon_p)
	endif

	enddo

return
end subroutine peeling_off_direct_hydrogen

subroutine peeling_off_direct_metal(photon)
use cons
type(photon_type), intent(in) :: photon
type(photon_type) :: photon_p
real(kind=rkd) :: tau_edge
integer :: i 


        do i = 1, par%Nobserver                                                                        

        par%iobs = i
        photon_p = photon

        photon_p%kx = observer(i)%kx
        photon_p%ky = observer(i)%ky
        photon_p%kz = observer(i)%kz

	call tau_to_edge_metal_clumpy_medium(photon_p,tau_edge)

!       exp(-tau_edge)  : optical depth                                         
!       photon%weigt    : weight of incident photon
!       1/(4.*pi)      : isotropic emission 
	        if(tau_edge .le. tau_max) then
		photon_p%weight = exp(-tau_edge)/(4.d0*pi)*photon%weight
		call collecting_photon(photon_p)
		endif

        enddo 

return
end subroutine peeling_off_direct_metal


subroutine peeling_off_metal(photon_s,photon_i)
use cons
type(photon_type), intent(in) :: photon_s, photon_i
type(photon_type) :: photon_p
real(kind=rkd) :: cost, cost2, sint
real(kind=rkd) :: cosp, sinp, cos2p, sin2p 
real(kind=rkd) :: v_th
real(kind=rkd) :: dnu_th_K, a_K
real(kind=rkd) :: dnu_th_H, a_H
real(kind=rkd) :: dnuH, dnuK, E1, nu_r, vel
real(kind=rkd) :: tau_edge
real(kind=rkd) :: S11,S12,S22,S33,S44
real(kind=rkd) :: Ip, Qp, Up, Vp, Q0, U0
real(kind=rkd) :: px, py, pz
real(kind=rkd) :: akz 
integer :: i


        if(photon_s%clump .eqv. .false.) then
        v_th = grid%v_ran(photon_s%ix,photon_s%iy,photon_s%iz) 
        else if(photon_s%clump .eqv. .true.) then
        v_th = clumps%v_th
        endif


	do i = 1, par%Nobserver 
	par%iobs = i

	photon_p = photon_i

	photon_p%NS = photon_s%NS
	photon_p%NS_K = photon_s%NS_K
	photon_p%NS_H = photon_s%NS_H

	photon_p%kx = observer(i)%kx
	photon_p%ky = observer(i)%ky
	photon_p%kz = observer(i)%kz

	cost = photon_p%kx * photon_i%kx + photon_p%ky * photon_i%ky + photon_p%kz * photon_i%kz
	cost2 = cost**2
	sint = sqrt(1.d0 - cost2)

        nu_r = photon_s%nu_atom
        vel = v_th*(photon_s%vz*cost+rand_gauss()*sqrt(1.d0-cost2)/sqrt(2.d0))
        photon_p%nu = nu_r/(1.d0 - vel/c)

	call tau_to_edge_metal_clumpy_medium(photon_p,tau_edge)

	if(tau_edge .le. tau_max) then

		if(cost .eq. 1.d0) then
		cosp = 1.d0
		sinp = 1.d0
		cos2p = 1.d0
		sin2p = 1.d0
		else
	        cosp  = (photon_p%kx * photon_i%mx + photon_p%ky * photon_i%my + photon_p%kz * photon_i%mz)/sint
	        sinp  = (photon_p%kx * photon_i%nx + photon_p%ky * photon_i%ny + photon_p%kz * photon_i%nz)/sint
	        cos2p = 2.0d0*cosp*cosp - 1.0d0
	        sin2p = 2.0d0*cosp*sinp
		endif

	        px = cosp*photon_i%mx + sinp*photon_i%nx
	        py = cosp*photon_i%my + sinp*photon_i%ny
	        pz = cosp*photon_i%mz + sinp*photon_i%nz

	        photon_p%mx = cost*px - sint*photon_i%kx
	        photon_p%my = cost*py - sint*photon_i%ky
	        photon_p%mz = cost*pz - sint*photon_i%kz

	        dnuH  = atom%nuH - nu_r
	        dnuK  = atom%nuK - nu_r
	        E1 = (dnuH**2 + 2.d0*dnuK*dnuH)/(2.d0*dnuH**2 + dnuK**2)
!
!       	Calculate Stokes Parameter
!
	        S22 = 3.d0/4.d0*E1*(cost2 + 1.d0)
	        S11 = S22 + (1.d0 - E1)
	        S12 = 3.d0/4.d0*E1*(cost2 - 1.d0)
	        S33 = 3.d0/2.d0*E1*cost
	        S44 = (E1 + 2.d0)/2.d0*cost

	        Q0 =  cos2p*photon_p%Q + sin2p*photon_p%U
	        U0 = -sin2p*photon_p%Q + cos2p*photon_p%U

	        Ip = S11 + S12*Q0       ! S11*I + S12*Q_r
	        Qp = S12 + S22*Q0       ! S12*I + S22*U_r
	        Up = S33*U0             ! S33*U_r
	        Vp = S44*photon_p%V     ! S44*V
	
		photon_p%I = Ip
		photon_p%Q = Qp
		photon_p%U = Up
		photon_p%V = Vp

!	exp(-tau_edge) 	: optical depth
!	photon%weigt   	: weight of incident photon
!	Ip/(4.*pi)	: Weight for scattering phase function
	photon_p%weight = exp(-tau_edge)*(Ip/(4.d0*pi))*photon_i%weight

        if( abs(cost) .gt. 1.d0) then
        print*,'atom peeling'
        print*,photon_p%weight, photon_i%weight
        print*, sqrt(photon_p%kx**2 + photon_p%ky**2 +  photon_p%kz**2)
        print*, sqrt(photon_i%kx**2 + photon_i%ky**2 +  photon_i%kz**2)
        print*,'BBBB'
        endif


	call collecting_photon(photon_p)


	endif

	enddo

return
end subroutine peeling_off_metal



subroutine peeling_off_dust(photon_s,photon_i)
use cons
type(photon_type), intent(in) :: photon_s, photon_i
type(photon_type) :: photon_p
real(kind=rkd) :: cost, sint
real(kind=rkd) :: cosp, sinp, cos2p, sin2p 
real(kind=rkd) :: tau_edge
real(kind=rkd) :: S11,S12,S33,S34
real(kind=rkd) :: Ip, Qp, Up, Vp, Q0, U0
real(kind=rkd) :: px, py, pz
real(kind=rkd) :: akz 
integer :: i, icos



	do i = 1, par%Nobserver 
	par%iobs = i

	photon_p = photon_i

	photon_p%NS = photon_s%NS
	photon_p%NS_D = photon_s%NS_D

	photon_p%kx = observer(i)%kx
	photon_p%ky = observer(i)%ky
	photon_p%kz = observer(i)%kz


!        nu_r = photon_s%nu_atom
!        vel = v_th*(photon_s%vz*cost+rand_gauss()*sqrt(1.d0-cost2)/sqrt(2.d0))
!        photon_p%nu = nu_r/(1.d0 - vel/c)

	call tau_to_edge_metal_clumpy_medium(photon_p,tau_edge)

	if(tau_edge .le. tau_max) then

	cost = photon_p%kx * photon_i%kx + photon_p%ky * photon_i%ky + photon_p%kz * photon_i%kz
	sint = sqrt(1.d0 - cost**2)

		if(cost .eq. 1.d0) then
		cosp = 1.d0
		sinp = 1.d0
		cos2p = 1.d0
		sin2p = 1.d0
		else
	        cosp  = (photon_p%kx * photon_i%mx + photon_p%ky * photon_i%my + photon_p%kz * photon_i%mz)/sint
	        sinp  = (photon_p%kx * photon_i%nx + photon_p%ky * photon_i%ny + photon_p%kz * photon_i%nz)/sint
	        cos2p = 2.0d0*cosp*cosp - 1.0d0
	        sin2p = 2.0d0*cosp*sinp
		endif

        icos = (cost + 1.d0)/(2.d0) * (dust%ncos - 1.d0) + 1
        if(icos .eq. dust%ncos) icos = dust%ncos - 1 
        S11 = ( dust%S11(icos + 1) - dust%S11(icos) )/( dust%cos_scat(icos + 1) - dust%cos_scat(icos) ) &
              *(cost - dust%cos_scat(icos)) + dust%S11(icos)
        S12 = ( dust%S12(icos + 1) - dust%S12(icos) )/( dust%cos_scat(icos + 1) - dust%cos_scat(icos) ) &
              *(cost - dust%cos_scat(icos)) + dust%S12(icos)
        S33 = ( dust%S33(icos + 1) - dust%S33(icos) )/( dust%cos_scat(icos + 1) - dust%cos_scat(icos) ) &
              *(cost - dust%cos_scat(icos)) + dust%S33(icos)
        S34 = ( dust%S34(icos + 1) - dust%S34(icos) )/( dust%cos_scat(icos + 1) - dust%cos_scat(icos) ) &
              *(cost - dust%cos_scat(icos)) + dust%S34(icos)

!
!       Polarization
!       Calculating Stokes Parameter

        Q0 =  cos2p*photon_p%Q + sin2p*photon_p%U
        U0 = -sin2p*photon_p%Q + cos2p*photon_p%U
!       Muller Matrix for dust 
        Ip = S11 + S12*Q0
        Qp = S12 + S11*Q0
        Up = S33*U0 + S34*photon_p%V
        Vp = S33*photon_p%V - S34*U0

        photon_p%I = 1.d0
        photon_p%Q = Qp/Ip
        photon_p%U = Up/Ip
        photon_p%V = Vp/Ip


!	dust%albedo	: dust albedo
!	exp(-tau_edge) 	: optical depth
!	photon%weigt   	: weight of incident photon
!	Ip/(4.*pi)	: Weight for scattering phase function
	photon_p%weight = dust%albedo*exp(-tau_edge)*(Ip/(4.d0*pi))*photon_i%weight
!
	if( abs(cost) .gt. 1.d0) then
	print*,'dust peeling'
	print*,photon_p%weight, photon_i%weight
	print*,Ip, S11, S12
	print*,Q0, U0
	print*, dust%ncos, icos, cost 
	print*,photon_p%NS, photon_p%NS_d
	print*, sqrt(photon_p%kx**2 + photon_p%ky**2 +  photon_p%kz**2)
	print*, sqrt(photon_i%kx**2 + photon_i%ky**2 +  photon_i%kz**2)
	print*,'BBBB'
	endif

	call collecting_photon(photon_p)


	endif

	enddo

return
end subroutine peeling_off_dust





end module peeling_off_mod 
