module tracing_tau_mod
use cons
use random
use voigt_mod
use mpi
use clumpy_RT_mod
implicit none
public

public tracing_tau
public tracing_tau_metal
public tracing_tau_clumpy
public tracing_tau_w_dust

contains

subroutine tracing_tau_clumpy(photon,tau)
use cons
type(photon_type) :: photon
real(kind=rkd), intent(in) :: tau 
real(kind=rkd) dx,dy,dz,dl
real(kind=rkd) xp,yp,zp 
real(kind=rkd) sigma_sum, sigma_atom
real(kind=rkd) dtau_atom, dtau_dust 
real(kind=rkd) tauD,tauDf,dtau
real(kind=rkd) v_th
real(kind=rkd) a_K, dnu_th_K
real(kind=rkd) a_H, dnu_th_H
integer :: ix,iy,iz
!	for clumps
integer :: iclump, iclump_b, i_init, i_final
real(kind=rkd) :: x_pc, y_pc, z_pc
real(kind=rkd) :: temp, temp1, temp2, temp3 
real(kind=rkd) :: dis_min, D_min 
real(kind=rkd) :: DD

        tauD = 0.d0
        tauDf = 0.d0
	xp = photon%x
	yp = photon%y
	zp = photon%z
	ix = photon%ix
	iy = photon%iy
	iz = photon%iz
        photon%vel1 =    photon%kx*grid%vx(ix,iy,iz) &
			+photon%ky*grid%vy(ix,iy,iz) &
			+photon%kz*grid%vz(ix,iy,iz)

	iclump_b = photon%iclump
	photon%iclump = 0
	DD = grid%max_len/1000.

	do 202


                if(photon%kx .gt. 0.d0) then
                dx = (grid%x(ix+1)-xp)/photon%kx
                else if(photon%kx .lt. 0.d0) then
                dx = (grid%x(ix)-xp)/photon%kx
		else if(photon%kx .eq. 0.d0) then
		dx = grid%max_len 
                endif
        
                if(photon%ky .gt. 0.d0) then
                dy = (grid%y(iy+1)-yp)/photon%ky
                else if(photon%ky .lt. 0.d0) then
                dy = (grid%y(iy)-yp)/photon%ky
		else if(photon%ky .eq. 0.d0) then
		dy = grid%max_len 
                endif

                if(photon%kz .gt. 0.d0) then
                dz = (grid%z(iz+1)-zp)/photon%kz
                else if(photon%kz .lt. 0.d0) then
                dz = (grid%z(iz)-zp)/photon%kz
		else if(photon%kz .eq. 0.d0) then
		dz = grid%max_len 
                endif

                dl = min(dx,dy,dz)


if(grid%den(ix,iy,iz) .eq. 0.d0 .and. clumps%N(ix,iy,iz) .eq. 0) then	! Empty Grid

                if(dx .eq. dl) then
                if(photon%kx .gt. 0.d0) then
                ix = ix + 1
                else if(photon%kx .lt. 0.d0) then
                ix = ix - 1
                endif
                endif

                if(dy .eq. dl) then
                if(photon%ky .gt. 0.d0) then
                iy = iy + 1
                else if(photon%ky .lt. 0.d0) then
                iy = iy - 1
                endif
                endif

                if(dz .eq. dl) then
                if(photon%kz .gt. 0.d0) then
                iz = iz + 1
                else if(photon%kz .lt. 0.d0) then
                iz = iz - 1
                endif
                endif

                if(ix .gt. grid%N_x .or. ix .lt. 1) then
                photon%esc = .true.
                exit
                endif

                if(iy .gt. grid%N_y .or. iy .lt. 1) then
                photon%esc = .true.
                exit
                endif

                if(iz .gt. grid%N_z .or. iz .lt. 1) then
                photon%esc = .true.
                exit
                endif


                xp = xp + photon%kx*dl
                yp = yp + photon%ky*dl
                zp = zp + photon%kz*dl
		iclump_b = 0

else if(grid%den(ix,iy,iz) .gt. 0.d0 .and. clumps%N(ix,iy,iz) .eq. 0) then  ! Grid w/o clumps 

        photon%vel2 =    photon%kx*grid%vx(ix,iy,iz) &
			+photon%ky*grid%vy(ix,iy,iz) &
			+photon%kz*grid%vz(ix,iy,iz)
        photon%nu = photon%nu*(1.d0 + photon%vel1/c - photon%vel2/c)

        v_th = grid%v_ran(ix,iy,iz) 
	dnu_th_K = v_th/c*atom%nuK
	dnu_th_H = v_th/c*atom%nuH
	a_K = atom%gamma_K/(4.d0*pi*dnu_th_K)
	a_H = atom%gamma_H/(4.d0*pi*dnu_th_H)

        photon%xnuK = (photon%nu-atom%nuK)/dnu_th_K
        photon%xnuH = (photon%nu-atom%nuH)/dnu_th_H
        photon%sigmaK = voigt(photon%xnuK,a_K)/dnu_th_K
        photon%sigmaH = voigt(photon%xnuH,a_H)/dnu_th_H
        sigma_atom = atom%sigma_0*(atom%f12_K*photon%sigmaK + atom%f12_H*photon%sigmaH)/atom%f12
        photon%vel1 = photon%vel2

	dtau_atom = dl*sigma_atom*grid%den(ix,iy,iz)
	dtau_dust = dl*dust%Cext*grid%den_d(ix,iy,iz)

	dtau =  dtau_dust + dtau_atom 

                tauDf = tauD + dtau

        if(tauDf .gt. tau) then

	dl = (tau-tauD)/dtau*dl
	xp = xp + photon%kx*dl
	yp = yp + photon%ky*dl
	zp = zp + photon%kz*dl
	photon%path = photon%path + dl 

        photon%tau_atom = photon%tau_atom + (tau-tauD)*dtau_atom/dtau
        photon%tau_dust = photon%tau_dust + (tau-tauD)*dtau_dust/dtau

        exit

        else


                if(dx .eq. dl) then
                if(photon%kx .gt. 0.d0) then
                ix = ix + 1
                else if(photon%kx .lt. 0.d0) then
                ix = ix - 1
                endif
                endif

                if(dy .eq. dl) then
                if(photon%ky .gt. 0.d0) then
                iy = iy + 1
                else if(photon%ky .lt. 0.d0) then
                iy = iy - 1
                endif
                endif

                if(dz .eq. dl) then
                if(photon%kz .gt. 0.d0) then
                iz = iz + 1
                else if(photon%kz .lt. 0.d0) then
                iz = iz - 1
                endif
                endif


                if(ix .gt. grid%N_x .or. ix .lt. 1) then
                photon%esc = .true.
                exit
                endif

                if(iy .gt. grid%N_y .or. iy .lt. 1) then
                photon%esc = .true.
                exit
                endif

                if(iz .gt. grid%N_z .or. iz .lt. 1) then
                photon%esc = .true.
                exit
                endif


                xp = xp + photon%kx*dl
                yp = yp + photon%ky*dl
                zp = zp + photon%kz*dl

                photon%tau_atom = photon%tau_atom + dtau_atom 
                photon%tau_dust = photon%tau_dust + dtau_dust 

		photon%path = photon%path + dl 
                tauD = tauDf
		iclump_b = 0
        endif

else if(clumps%N(ix,iy,iz) .gt. 0) then	 !  Grid with clumps


                dis_min = grid%max_len 
                i_init  = clumps%init_i(ix,iy,iz)
                i_final = i_init + clumps%N(ix,iy,iz) - 1
		photon%iclump = 0
		photon%overlap = .false.

		time_f = MPI_WTIME() 


                do iclump = i_init, i_final

                x_pc = xp - clumps%x(iclump)
                y_pc = yp - clumps%y(iclump)
                z_pc = zp - clumps%z(iclump)

                temp1 = photon%kx*x_pc +photon%ky*y_pc +photon%kz*z_pc
                temp2 = sqrt(   (photon%ky*z_pc - photon%kz*y_pc)**2 + &
                                (photon%kz*x_pc - photon%kx*z_pc)**2 + &
                                (photon%kx*y_pc - photon%ky*x_pc)**2 )
                temp3 = sqrt(x_pc**2 + y_pc**2 + z_pc**2)

                        if(temp3 .lt. clumps%r .and. iclump .ne. iclump_b) then
                        photon%overlap = .true.
                        photon%iclump = iclump
                        D_min = temp2
                        exit
                        endif

                temp = sqrt(temp3**2 - temp2**2) - sqrt(clumps%r**2 - temp2**2)

	                if(temp1 .lt. 0.d0 .and. temp2 .lt. clumps%r) then
	                        if(temp .lt. dis_min .and. iclump .ne. iclump_b) then
	                        dis_min = temp
	                        photon%iclump = iclump
	                        endif
	                endif

                enddo


		if(photon%overlap .eqv. .true.) then
		photon%clump = .true.
		exit
		endif



       		if(photon%iclump .ne. 0) dl = min(dl,dis_min)


		if(grid%den(ix,iy,iz) .gt. 0.d0) then

		        photon%vel2 =    photon%kx*grid%vx(ix,iy,iz) &
		                        +photon%ky*grid%vy(ix,iy,iz) &
		                        +photon%kz*grid%vz(ix,iy,iz)
		        photon%nu = photon%nu*(1.d0 + photon%vel1/c - photon%vel2/c)
	
		        v_th = grid%v_ran(ix,iy,iz) 
		        dnu_th_K = v_th/c*atom%nuK
		        dnu_th_H = v_th/c*atom%nuH
		        a_K = atom%gamma_K/(4.d0*pi*dnu_th_K)
		        a_H = atom%gamma_H/(4.d0*pi*dnu_th_H)
		        photon%xnuK = (photon%nu-atom%nuK)/dnu_th_K
		        photon%xnuH = (photon%nu-atom%nuH)/dnu_th_H
		        photon%sigmaK = voigt(photon%xnuK,a_K)/dnu_th_K
		        photon%sigmaH = voigt(photon%xnuH,a_H)/dnu_th_H
		        sigma_atom = atom%sigma_0*(atom%f12_K*photon%sigmaK + atom%f12_H*photon%sigmaH)/atom%f12
		        photon%vel1 = photon%vel2

		        dtau_atom = dl*sigma_atom*grid%den(ix,iy,iz)
		        dtau_dust = dl*dust%Cext*grid%den_d(ix,iy,iz)

		        dtau =  dtau_dust + dtau_atom 

	                tauDf = tauD + dtau

		        if(tauDf .gt. tau) then
	
			dl = (tau-tauD)/dtau*dl
		        xp = xp + photon%kx*dl
		        yp = yp + photon%ky*dl
		        zp = zp + photon%kz*dl
        		photon%tau_atom = photon%tau_atom + (tau-tauD)*dtau_atom/dtau
		        photon%tau_dust = photon%tau_dust + (tau-tauD)*dtau_dust/dtau

		        exit
		        endif
		endif



		if(photon%iclump .ne. 0) then
! 		MEET CLUMP
			photon%D_cl = dis_min
			photon%clump = .true.
			exit

		else if(photon%iclump .eq. 0) then

		dl = min(dx,dy,dz)

			if(dx .eq. dl) then
			if(photon%kx .gt. 0.d0) then
			ix = ix + 1
			else if(photon%kx .lt. 0.d0) then
			ix = ix - 1
			endif
			endif

			if(dy .eq. dl) then
			if(photon%ky .gt. 0.d0) then
			iy = iy + 1
			else if(photon%ky .lt. 0.d0) then
			iy = iy - 1
			endif
			endif

			if(dz .eq. dl) then
			if(photon%kz .gt. 0.d0) then
			iz = iz + 1
			else if(photon%kz .lt. 0.d0) then
			iz = iz - 1
			endif
			endif


			if(ix .gt. grid%N_x .or. ix .lt. 1) then
			photon%esc = .true.
			exit
			endif

			if(iy .gt. grid%N_y .or. iy .lt. 1) then
			photon%esc = .true.
			exit
			endif

			if(iz .gt. grid%N_z .or. iz .lt. 1) then
			photon%esc = .true.
			exit
			endif

		endif

                xp = xp + photon%kx*dl
                yp = yp + photon%ky*dl
                zp = zp + photon%kz*dl
                photon%tau_atom = photon%tau_atom + dtau_atom 
                photon%tau_dust = photon%tau_dust + dtau_dust 
                tauD = tauDf
                iclump_b = 0



endif

202     continue

		photon%ix = ix
		photon%iy = iy
		photon%iz = iz
		photon%x = xp
		photon%y = yp
		photon%z = zp

end subroutine tracing_tau_clumpy

subroutine tracing_tau_metal(photon,tau)
use cons
type(photon_type) :: photon
real(kind=rkd), intent(in) :: tau 
real(kind=rkd) dx,dy,dz,dl
real(kind=rkd) xp,yp,zp 
real(kind=rkd) sigma
real(kind=rkd) tauD,tauDf,dtau
real(kind=rkd) v_th
real(kind=rkd) a_K, dnu_th_K
real(kind=rkd) a_H, dnu_th_H
integer :: ix,iy,iz

        tauD = 0.d0
        tauDf = 0.d0
	xp = photon%x
	yp = photon%y
	zp = photon%z
	ix = photon%ix
	iy = photon%iy
	iz = photon%iz

        photon%vel1 =    photon%kx*grid%vx(ix,iy,iz) &
			+photon%ky*grid%vy(ix,iy,iz) &
			+photon%kz*grid%vz(ix,iy,iz)

                do 202

                if(photon%kx .gt. 0.d0) then
                dx = (grid%x(ix+1)-xp)/photon%kx
                else if(photon%kx .lt. 0.d0) then
                dx = (grid%x(ix)-xp)/photon%kx
		else if(photon%kx .eq. 0.d0) then
		dx = grid%max_len 
                endif
        
                if(photon%ky .gt. 0.d0) then
                dy = (grid%y(iy+1)-yp)/photon%ky
                else if(photon%ky .lt. 0.d0) then
                dy = (grid%y(iy)-yp)/photon%ky
		else if(photon%ky .eq. 0.d0) then
		dy = grid%max_len 
                endif

                if(photon%kz .gt. 0.d0) then
                dz = (grid%z(iz+1)-zp)/photon%kz
                else if(photon%kz .lt. 0.d0) then
                dz = (grid%z(iz)-zp)/photon%kz
		else if(photon%kz .eq. 0.d0) then
		dz = grid%max_len 
                endif


                dl = min(dx,dy,dz)

if(grid%den(ix,iy,iz) .eq. 0.d0) then

                if(dx .eq. dl) then
                if(photon%kx .gt. 0.d0) then
                ix = ix + 1
                else if(photon%kx .lt. 0.d0) then
                ix = ix - 1
                endif
                endif

                if(dy .eq. dl) then
                if(photon%ky .gt. 0.d0) then
                iy = iy + 1
                else if(photon%ky .lt. 0.d0) then
                iy = iy - 1
                endif
                endif

                if(dz .eq. dl) then
                if(photon%kz .gt. 0.d0) then
                iz = iz + 1
                else if(photon%kz .lt. 0.d0) then
                iz = iz - 1
                endif
                endif

                if(ix .gt. grid%N_x .or. ix .lt. 1) then
                photon%esc = .true.
                exit
                endif

                if(iy .gt. grid%N_y .or. iy .lt. 1) then
                photon%esc = .true.
                exit
                endif

                if(iz .gt. grid%N_z .or. iz .lt. 1) then
                photon%esc = .true.
                exit
                endif


                xp = xp + photon%kx*dl
                yp = yp + photon%ky*dl
                zp = zp + photon%kz*dl

else    ! den .ne. 0

        photon%vel2 =    photon%kx*grid%vx(ix,iy,iz) &
			+photon%ky*grid%vy(ix,iy,iz) &
			+photon%kz*grid%vz(ix,iy,iz)
        photon%nu = photon%nu*(1.d0 + photon%vel1/c - photon%vel2/c)

        v_th = grid%v_ran(ix,iy,iz) 
	dnu_th_K = v_th/c*atom%nuK
	dnu_th_H = v_th/c*atom%nuH
	a_K = atom%gamma_K/(4.d0*pi*dnu_th_K)
	a_H = atom%gamma_H/(4.d0*pi*dnu_th_H)
!	(이거 Grid 로??)
        photon%xnuK = (photon%nu-atom%nuK)/dnu_th_K
        photon%xnuH = (photon%nu-atom%nuH)/dnu_th_H
        photon%sigmaK = voigt(photon%xnuK,a_K)/dnu_th_K
        photon%sigmaH = voigt(photon%xnuH,a_H)/dnu_th_H
        sigma = atom%sigma_0*(atom%f12_K*photon%sigmaK + atom%f12_H*photon%sigmaH)/atom%f12
        photon%vel1 = photon%vel2
	dtau =  dl*grid%den(ix,iy,iz)*sigma

                tauDf = tauD + dtau

        if(tauDf .gt. tau) then

	xp = xp + photon%kx*(tau-tauD)/grid%den(ix,iy,iz)/sigma
	yp = yp + photon%ky*(tau-tauD)/grid%den(ix,iy,iz)/sigma
	zp = zp + photon%kz*(tau-tauD)/grid%den(ix,iy,iz)/sigma

        exit

        else


                if(dx .eq. dl) then
                if(photon%kx .gt. 0.d0) then
                ix = ix + 1
                else if(photon%kx .lt. 0.d0) then
                ix = ix - 1
                endif
                endif

                if(dy .eq. dl) then
                if(photon%ky .gt. 0.d0) then
                iy = iy + 1
                else if(photon%ky .lt. 0.d0) then
                iy = iy - 1
                endif
                endif

                if(dz .eq. dl) then
                if(photon%kz .gt. 0.d0) then
                iz = iz + 1
                else if(photon%kz .lt. 0.d0) then
                iz = iz - 1
                endif
                endif


                if(ix .gt. grid%N_x .or. ix .lt. 1) then
                photon%esc = .true.
                exit
                endif

                if(iy .gt. grid%N_y .or. iy .lt. 1) then
                photon%esc = .true.
                exit
                endif

                if(iz .gt. grid%N_z .or. iz .lt. 1) then
                photon%esc = .true.
                exit
                endif


                xp = xp + photon%kx*dl
                yp = yp + photon%ky*dl
                zp = zp + photon%kz*dl
                tauD = tauDf
endif
        endif

202     continue

		photon%ix = ix
		photon%iy = iy
		photon%iz = iz
		photon%x = xp
		photon%y = yp
		photon%z = zp

end subroutine tracing_tau_metal

subroutine tracing_tau(photon,tau)
use cons
type(photon_type) :: photon
real(kind=rkd), intent(in) :: tau 
real(kind=rkd) dx,dy,dz,dl
real(kind=rkd) xp,yp,zp 
real(kind=rkd) sigma
real(kind=rkd) tauD,tauDf,dtau
real(kind=rkd) v_th, a, dnu_th
integer :: ix,iy,iz

        tauD = 0.d0
        tauDf = 0.d0
	xp = photon%x
	yp = photon%y
	zp = photon%z
	ix = photon%ix
	iy = photon%iy
	iz = photon%iz
        photon%vel1 =    photon%kx*grid%vx(ix,iy,iz) &
			+photon%ky*grid%vy(ix,iy,iz) &
			+photon%kz*grid%vz(ix,iy,iz)

                do 202

                if(photon%kx .gt. 0.d0) then
                dx = (grid%x(ix+1)-xp)/photon%kx
                else if(photon%kx .lt. 0.d0) then
                dx = (grid%x(ix)-xp)/photon%kx
		else if(photon%kx .eq. 0.d0) then
		dx = grid%max_len 
                endif
        
                if(photon%ky .gt. 0.d0) then
                dy = (grid%y(iy+1)-yp)/photon%ky
                else if(photon%ky .lt. 0.d0) then
                dy = (grid%y(iy)-yp)/photon%ky
		else if(photon%ky .eq. 0.d0) then
		dy = grid%max_len 
                endif

                if(photon%kz .gt. 0.d0) then
                dz = (grid%z(iz+1)-zp)/photon%kz
                else if(photon%kz .lt. 0.d0) then
                dz = (grid%z(iz)-zp)/photon%kz
		else if(photon%kz .eq. 0.d0) then
		dz = grid%max_len 
                endif


                dl = min(dx,dy,dz)

if(grid%den(ix,iy,iz) .eq. 0.d0) then

                if(dx .eq. dl) then
                if(photon%kx .gt. 0.d0) then
                ix = ix + 1
                else if(photon%kx .lt. 0.d0) then
                ix = ix - 1
                endif
                endif

                if(dy .eq. dl) then
                if(photon%ky .gt. 0.d0) then
                iy = iy + 1
                else if(photon%ky .lt. 0.d0) then
                iy = iy - 1
                endif
                endif

                if(dz .eq. dl) then
                if(photon%kz .gt. 0.d0) then
                iz = iz + 1
                else if(photon%kz .lt. 0.d0) then
                iz = iz - 1
                endif
                endif

                if(ix .gt. grid%N_x .or. ix .lt. 1) then
                photon%esc = .true.
                exit
                endif

                if(iy .gt. grid%N_y .or. iy .lt. 1) then
                photon%esc = .true.
                exit
                endif

                if(iz .gt. grid%N_z .or. iz .lt. 1) then
                photon%esc = .true.
                exit
                endif


                xp = xp + photon%kx*dl
                yp = yp + photon%ky*dl
                zp = zp + photon%kz*dl

else    ! den .ne. 0

        photon%vel2 =    photon%kx*grid%vx(ix,iy,iz) &
			+photon%ky*grid%vy(ix,iy,iz) &
			+photon%kz*grid%vz(ix,iy,iz)
        photon%nu = photon%nu*(1.d0 + photon%vel1/c - photon%vel2/c)

        v_th = grid%v_ran(ix,iy,iz) 
        dnu_th = v_th/c*((nuK+2.d0*nuH)/3.d0)
        a = gamma/(4.d0*pi*dnu_th)
!	(이거 Grid 로??)
        photon%xnuK = (photon%nu-nuK)/dnu_th
        photon%xnuH = (photon%nu-nuH)/dnu_th
        photon%sigmaK = voigt(photon%xnuK,a)
        photon%sigmaH = voigt(photon%xnuH,a)
        sigma = atom%sigma_0/dnu_th*(photon%sigmaK/3.d0 + 2.d0*photon%sigmaH/3.d0)
        photon%vel1 = photon%vel2
	dtau =  dl*grid%den(ix,iy,iz)*sigma

                tauDf = tauD + dtau

        if(tauDf .gt. tau) then

	xp = xp + photon%kx*(tau-tauD)/grid%den(ix,iy,iz)/sigma
	yp = yp + photon%ky*(tau-tauD)/grid%den(ix,iy,iz)/sigma
	zp = zp + photon%kz*(tau-tauD)/grid%den(ix,iy,iz)/sigma

        exit

        else


                if(dx .eq. dl) then
                if(photon%kx .gt. 0.d0) then
                ix = ix + 1
                else if(photon%kx .lt. 0.d0) then
                ix = ix - 1
                endif
                endif

                if(dy .eq. dl) then
                if(photon%ky .gt. 0.d0) then
                iy = iy + 1
                else if(photon%ky .lt. 0.d0) then
                iy = iy - 1
                endif
                endif

                if(dz .eq. dl) then
                if(photon%kz .gt. 0.d0) then
                iz = iz + 1
                else if(photon%kz .lt. 0.d0) then
                iz = iz - 1
                endif
                endif


                if(ix .gt. grid%N_x .or. ix .lt. 1) then
                photon%esc = .true.
                exit
                endif

                if(iy .gt. grid%N_y .or. iy .lt. 1) then
                photon%esc = .true.
                exit
                endif

                if(iz .gt. grid%N_z .or. iz .lt. 1) then
                photon%esc = .true.
                exit
                endif


                xp = xp + photon%kx*dl
                yp = yp + photon%ky*dl
                zp = zp + photon%kz*dl
                tauD = tauDf
endif
        endif

202     continue

		photon%ix = ix
		photon%iy = iy
		photon%iz = iz
		photon%x = xp
		photon%y = yp
		photon%z = zp

end subroutine tracing_tau

subroutine tracing_tau_w_dust(photon,dust,tau)
type(photon_type) :: photon
type(dust_type), intent(in) :: dust
real(kind=rkd), intent(in) :: tau 
real(kind=rkd) dx,dy,dz,dl
real(kind=rkd) xp,yp,zp 
real(kind=rkd) sigma_sum, sigma_atom, sigma_dust
real(kind=rkd) tauD,tauDf 
real(kind=rkd) v_th, a, dnu_th
integer :: ix, iy, iz


        tauD = 0.d0
	photon%tau_atom = 0.d0
	photon%tau_dust = 0.d0
        photon%vel1 =    photon%kx*grid%vx(ix,iy,iz) &
			+photon%ky*grid%vy(ix,iy,iz) &
			+photon%kz*grid%vz(ix,iy,iz)
	xp = photon%x
	yp = photon%y
	zp = photon%z
	ix = photon%ix
	iy = photon%iy
	iz = photon%iz

                do 202

        
                if(photon%kx .gt. 0.d0) then
                dx = (grid%x(ix+1)-xp)/photon%kx
                else if(photon%kx .lt. 0.d0) then
                dx = (grid%x(ix)-xp)/photon%kx
		else if(photon%kx .eq. 0.d0) then
		dx = grid%x(ix+1) - grid%x(ix)
                endif
        
                if(photon%ky .gt. 0.d0) then
                dy = (grid%y(iy+1)-yp)/photon%ky
                else if(photon%ky .lt. 0.d0) then
                dy = (grid%y(iy)-yp)/photon%ky
		else if(photon%ky .eq. 0.d0) then
		dy = grid%y(iy+1) - grid%y(iy)
                endif

                if(photon%kz .gt. 0.d0) then
                dz = (grid%z(iz+1)-zp)/photon%kz
                else if(photon%kx .gt. 0.d0) then
                dz = (grid%z(iz)-zp)/photon%kz
		else if(photon%kz .eq. 0.d0) then
		dz = grid%z(iz+1) - grid%z(iz)
                endif



                dl = min(dx,dy,dz)

if(grid%den(ix,iy,iz) .eq. 0.d0) then

                if(dx .eq. dl) then
                if(photon%kx .gt. 0.d0) then
                ix = ix + 1
                else if(photon%kx .lt. 0.d0) then
                ix = ix - 1
                endif
                endif

                if(dy .eq. dl) then
                if(photon%ky .gt. 0.d0) then
                iy = iy + 1
                else if(photon%ky .lt. 0.d0) then
                iy = iy - 1
                endif
                endif

                if(dz .eq. dl) then
                if(photon%kz .gt. 0.d0) then
                iz = iz + 1
                else if(photon%kz .lt. 0.d0) then
                iz = iz - 1
                endif
                endif

                if(ix .gt. grid%N_x .or. ix .lt. 1) then
                photon%esc = .true.
                exit
                endif

                if(iy .gt. grid%N_y .or. iy .lt. 1) then
                photon%esc = .true.
                exit
                endif

                if(iz .gt. grid%N_z .or. iz .lt. 1) then
                photon%esc = .true.
                exit
                endif


                xp = xp + photon%kx*dl
                yp = yp + photon%ky*dl
                zp = zp + photon%kz*dl

else    ! den .ne. 0

        photon%vel2 =    photon%kx*grid%vx(ix,iy,iz) &
			+photon%ky*grid%vy(ix,iy,iz) &
			+photon%kz*grid%vz(ix,iy,iz)
        photon%nu = photon%nu*(1.d0 + photon%vel1/c - photon%vel2/c)
        photon%vel1 = photon%vel2

        v_th = grid%v_ran(ix,iy,iz) 
        dnu_th = v_th/c*((nuK+2.d0*nuH)/3.d0)
        a = gamma/(4.d0*pi*dnu_th)
        photon%xnuK = (photon%nu-nuK)/grid%dnu_th
        photon%xnuH = (photon%nu-nuH)/grid%dnu_th
        photon%sigmaK = voigt(photon%xnuK,grid%a)
        photon%sigmaH = voigt(photon%xnuH,grid%a)
!	with Atomic Hydrogen
        sigma_atom = atom%sigma_0/grid%dnu_th*(photon%sigmaK/3.d0 + 2.d0*photon%sigmaH/3.d0)
	sigma_atom = grid%den(ix,iy,iz)*sigma_atom
!	With dust
        sigma_dust = grid%den_d(ix,iy,iz)*dust%Cext 
	sigma_sum = sigma_atom + sigma_dust


                tauDf = tauD + dl*sigma_sum

        if(tauDf .gt. tau) then

	xp = xp + photon%kx*(tau-tauD)/grid%den(ix,iy,iz)/sigma_sum
	yp = yp + photon%ky*(tau-tauD)/grid%den(ix,iy,iz)/sigma_sum
	zp = zp + photon%kz*(tau-tauD)/grid%den(ix,iy,iz)/sigma_sum
	photon%tau_atom = photon%tau_atom + sigma_atom*(tau-tauD)/sigma_sum
	photon%tau_dust = photon%tau_dust + sigma_dust*(tau-tauD)/sigma_sum

        exit

        else


                if(dx .eq. dl) then
                if(photon%kx .gt. 0.d0) then
                ix = ix + 1
                else if(photon%kx .lt. 0.d0) then
                ix = ix - 1
                endif
                endif

                if(dy .eq. dl) then
                if(photon%ky .gt. 0.d0) then
                iy = iy + 1
                else if(photon%ky .lt. 0.d0) then
                iy = iy - 1
                endif
                endif

                if(dz .eq. dl) then
                if(photon%kz .gt. 0.d0) then
                iz = iz + 1
                else if(photon%kz .lt. 0.d0) then
                iz = iz - 1
                endif
                endif


                if(ix .gt. grid%N_x .or. ix .lt. 1) then
                photon%esc = .true.
                exit
                endif

                if(iy .gt. grid%N_y .or. iy .lt. 1) then
                photon%esc = .true.
                exit
                endif

                if(iz .gt. grid%N_z .or. iz .lt. 1) then
                photon%esc = .true.
                exit
                endif


                xp = xp + photon%kx*dl
                yp = yp + photon%ky*dl
                zp = zp + photon%kz*dl
		photon%tau_atom = photon%tau_atom + sigma_atom*dl
		photon%tau_dust = photon%tau_dust + sigma_dust*dl
                tauD = tauDf
endif
        endif

202     continue

		photon%ix = ix
		photon%iy = iy
		photon%iz = iz
		photon%x = xp
		photon%y = yp
		photon%z = zp

end subroutine tracing_tau_w_dust


end module tracing_tau_mod 
