module tau_edge_mod
use cons
use random
use voigt_mod
use mpi
implicit none

public

public tau_to_edge_hydrogen
public tau_to_edge_metal
public tau_to_edge_metal_clumpy_medium
public tau_to_edge_clump

contains

subroutine  tau_to_edge_clump(photon,x_in,y_in,z_in,iclump, tau_clump)
use cons
type(photon_type) :: photon
integer, intent(in) :: iclump
real(kind=rkd) :: x_in,y_in,z_in 
real(kind=rkd), intent(out) :: tau_clump
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
real(kind=rkd) :: dl_overlap
real(kind=rkd) :: nu
integer :: i_init, i_final



dl = 0.d0
R = clumps%r

vel =     photon%kx*clumps%vx(iclump) &
        + photon%ky*clumps%vy(iclump) &
        + photon%kz*clumps%vz(iclump)

nu = photon%nu*(1.d0 - vel/c + photon%vel1/c) 

if(photon%overlap .eqv. .false.) then

xp = x_in + photon%kx*photon%D_cl
yp = y_in + photon%ky*photon%D_cl
zp = z_in + photon%kz*photon%D_cl

x = xp - clumps%x(iclump)
y = yp - clumps%y(iclump)
z = zp - clumps%z(iclump)

else if(photon%overlap .eqv. .true.) then
!       Move Photon -\hat k direction

x = x_in - clumps%x(iclump)
y = y_in - clumps%y(iclump)
z = z_in - clumps%z(iclump)

                D_c   = sqrt(    (-photon%ky*z + photon%kz*y)**2 &
                               + (-photon%kz*x + photon%kx*z)**2 &
                               + (-photon%kx*y + photon%ky*x)**2 )
                temp = sqrt(x**2 + y**2 + z**2)
                temp1 = sqrt(R**2 - D_c**2)
                temp2 = sqrt(temp**2 - D_c**2)
!               if k hat and r_pc hat are really similar
		if(isnan(temp2) .eqv. .true.) temp2 = 0.d0
                temp3 = - photon%kx*x - photon%ky*y - photon%kz*z 
!               value of temp2 with sign of temp3
                dl_overlap = temp1 + sign(temp2,-temp3)
!		back to -k direction
                x = x - photon%kx*dl_overlap
                y = y - photon%ky*dl_overlap
                z = z - photon%kz*dl_overlap

endif

        x_pc = x
        y_pc = y
        z_pc = z

        D_c   = sqrt(    (photon%ky*z_pc - photon%kz*y_pc)**2 &
                       + (photon%kz*x_pc - photon%kx*z_pc)**2 &
                       + (photon%kx*y_pc - photon%ky*x_pc)**2 )

        temp = sqrt(R**2 - D_c**2)
        dl = 2.d0*temp


        v_th = clumps%v_th
        dnu_th_K = v_th/c*atom%nuK
        dnu_th_H = v_th/c*atom%nuH
        a_K = atom%gamma_K/(4.d0*pi*dnu_th_K)
        a_H = atom%gamma_H/(4.d0*pi*dnu_th_H)
        photon%xnuK = (nu-atom%nuK)/dnu_th_K
        photon%xnuH = (nu-atom%nuH)/dnu_th_H
        photon%sigmaK = voigt(photon%xnuK,a_K)/dnu_th_K
        photon%sigmaH = voigt(photon%xnuH,a_H)/dnu_th_H
        sigma_atom = atom%sigma_0*(atom%f12_K*photon%sigmaK + atom%f12_H*photon%sigmaH)/atom%f12
        dtau_atom = sigma_atom*clumps%den
        dtau_dust = dust%Cext*clumps%den_d


!	tau_clump = dl*sigma*clumps%den
	tau_clump = dl*(dtau_atom + dtau_dust)

	x_in = x_pc + dl*photon%kx + clumps%x(iclump) 
	y_in = y_pc + dl*photon%ky + clumps%y(iclump) 
	z_in = z_pc + dl*photon%kz + clumps%z(iclump) 

return
end subroutine tau_to_edge_clump

subroutine tau_to_edge_metal_clumpy_medium(photon,tau_edge)
use cons
type(photon_type) :: photon
real(kind=rkd), intent(out) :: tau_edge
real(kind=rkd) dx,dy,dz,dl
real(kind=rkd) xp,yp,zp
real(kind=rkd) sigma_sum, sigma_atom
real(kind=rkd) tauD,tauDf
real(kind=rkd) v_th
real(kind=rkd) a_K, dnu_th_K
real(kind=rkd) a_H, dnu_th_H
real(kind=rkd) dtau, dtau_atom, dtau_dust
integer :: ix,iy,iz
!       for clumps
integer :: iclump_p
integer :: iclump, iclump_b, i_init, i_final
real(kind=rkd) :: x_pc, y_pc, z_pc
real(kind=rkd) :: temp, temp1, temp2, temp3
real(kind=rkd) :: dis_min, D_min
real(kind=rkd) tau_clump
real(kind=rkd) :: D_c, vel_c,R
real(kind=rkd) :: N_c,D_cl
real(kind=rkd) :: DD,dis_clump

tauDf = 0.d0

if(photon%clump .eqv. .true.) then
	iclump = photon%iclump
	R = clumps%R

        x_pc = photon%x - clumps%x(iclump)
        y_pc = photon%y - clumps%y(iclump)
        z_pc = photon%z - clumps%z(iclump)

        D_c   = sqrt(    (photon%ky*z_pc - photon%kz*y_pc)**2 &
                       + (photon%kz*x_pc - photon%kx*z_pc)**2 &
                       + (photon%kx*y_pc - photon%ky*x_pc)**2 )

        temp = sqrt(x_pc**2 + y_pc**2 + z_pc**2)
        temp1 = sqrt(R**2 - D_c**2)
        temp2 = sqrt(temp**2 - D_c**2)
!               if k hat and r_pc hat are really similar
	if(isnan(temp2) .eqv. .true.) temp2 = 0.d0
        temp3 = photon%kx*x_pc + photon%ky*y_pc + photon%kz*z_pc
!       value of temp2 with sign of temp3
        dl = temp1 + sign(temp2,-temp3)

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
	dtau = dl*(dtau_atom + dtau_dust)

	tauDf = dtau

endif


if(tauDf .le. tau_max) then


	if(photon%clump .eqv. .true.) then

	tauD  = dtau
	tauDf = dtau
        xp = photon%x + photon%kx*dl
        yp = photon%y + photon%ky*dl
        zp = photon%z + photon%kz*dl
	ix = int((xp - grid%X(1))/(grid%X(grid%N_X+1) - grid%X(1))*grid%N_X) + 1
	iy = int((yp - grid%Y(1))/(grid%Y(grid%N_Y+1) - grid%Y(1))*grid%N_Y) + 1
	iz = int((zp - grid%Z(1))/(grid%Z(grid%N_Z+1) - grid%Z(1))*grid%N_Z) + 1
!	Clumps velocity
        vel_c =     photon%kx*clumps%vx(iclump) &
                +   photon%ky*clumps%vy(iclump) &
                +   photon%kz*clumps%vz(iclump)
!	Grid Velocity	
        photon%vel1 = photon%kx*grid%vx(ix,iy,iz) &
                    + photon%ky*grid%vy(ix,iy,iz) &
                    + photon%kz*grid%vz(ix,iy,iz)
	photon%vel2 = photon%vel1
        photon%nu = photon%nu*(1.d0 + vel_c/c - photon%vel1/c) 

	iclump_b = iclump

	else if(photon%clump .eqv. .false.) then

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

	iclump_b = 0

	endif

	iclump_p = 0
	N_c = 0.d0

	

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
		if(dl .lt. 0.d0) then
		print*,'BBBB',dl,N_c
		print*,dx,dy,dz
		print*,xp/grid%Ro,yp/grid%Ro,zp/grid%Ro
		print*,xp, grid%x(ix), grid%x(ix + 1)
		print*,yp, grid%y(iy), grid%y(iy + 1)
		print*,zp, grid%z(iz), grid%z(iz + 1)
		endif

if(grid%den(ix,iy,iz) .eq. 0.d0 .and. clumps%N(ix,iy,iz) .eq. 0) then	! Empty Grid


		if(dx .eq. dl) then
		        if(photon%kx .gt. 0.d0) then
			ix = ix + 1
			else if(photon%kx .lt. 0.d0) then
			ix = ix - 1
			endif
		else if(dy .eq. dl) then
			if(photon%ky .gt. 0.d0) then
			iy = iy + 1
			else if(photon%ky .lt. 0.d0) then
			iy = iy - 1
			endif
		else if(dz .eq. dl) then
			if(photon%kz .gt. 0.d0) then
			iz = iz + 1
			else if(photon%kz .lt. 0.d0) then
			iz = iz - 1
			endif
		endif

		if(ix .gt. grid%N_x .or. ix .lt. 1) then
		exit
		endif

		if(iy .gt. grid%N_y .or. iy .lt. 1) then
		exit
		endif

		if(iz .gt. grid%N_z .or. iz .lt. 1) then
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

		if(tauDf .gt. tau_max) then
		exit
		endif

                if(dx .eq. dl) then
                        if(photon%kx .gt. 0.d0) then
                        ix = ix + 1
                        else if(photon%kx .lt. 0.d0) then
                        ix = ix - 1
                        endif
                else if(dy .eq. dl) then
                        if(photon%ky .gt. 0.d0) then
                        iy = iy + 1
                        else if(photon%ky .lt. 0.d0) then
                        iy = iy - 1
                        endif
                else if(dz .eq. dl) then
                        if(photon%kz .gt. 0.d0) then
                        iz = iz + 1
                        else if(photon%kz .lt. 0.d0) then
                        iz = iz - 1
                        endif
                endif

                if(ix .gt. grid%N_x .or. ix .lt. 1) then
                exit
                endif

                if(iy .gt. grid%N_y .or. iy .lt. 1) then
                exit
                endif

                if(iz .gt. grid%N_z .or. iz .lt. 1) then
                exit
                endif

                xp = xp + photon%kx*dl
                yp = yp + photon%ky*dl
                zp = zp + photon%kz*dl
		photon%path = photon%path + dl
                tauD = tauDf
		iclump_b = 0

else if(clumps%N(ix,iy,iz) .gt. 0) then  ! Grid with clumps 

                dis_min = grid%max_len
                i_init  = clumps%init_i(ix,iy,iz)
                i_final = i_init + clumps%N(ix,iy,iz) - 1
		iclump_p = 0
		photon%overlap = .false.

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
                        iclump_p = iclump
                        D_min = temp2
!                        dis_min = temp
                        exit
                        endif

                temp = sqrt(temp3**2 - temp2**2) - sqrt(clumps%r**2 - temp2**2)

                if(temp1 .lt. 0.d0 .and. temp2 .lt. clumps%r) then
                        if(temp .lt. dis_min .and. iclump .ne. iclump_b) then
                        dis_min = temp
                        iclump_p = iclump
			D_cl = temp2
			dis_clump = temp3
                        endif
                endif

                enddo


	if(photon%overlap .eqv. .true.) then

	call tau_to_edge_clump(photon,xp,yp,zp,iclump_p,tau_clump)
	iclump_b = iclump_p
	iclump_p = 0
	tauDf = tauD + tau_clump

	if(tauDf .gt. tau_max) then
	exit
	endif

	else if(photon%overlap .eqv. .false.) then

                if(dis_min .gt. 0.d0 .and. dis_min .lt. grid%max_len) dl = min(dl,dis_min)



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

			if(tauDf .gt. tau_max) then
			exit
			endif
		endif


                if(dl .eq. dis_min) then
!		MEET CLUMP
	                photon%D_cl = dis_min
			N_c = N_c + 1.d0
	       		call tau_to_edge_clump(photon,xp,yp,zp,iclump_p,tau_clump)
			iclump_b = iclump_p
	       		iclump_p = 0
	                tauDf = tauD + tau_clump

	                if(tauDf .gt. tau_max) then
	                exit
	                endif

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
	               exit
	               endif

	               if(iy .gt. grid%N_y .or. iy .lt. 1) then
	               exit
	               endif

	               if(iz .gt. grid%N_z .or. iz .lt. 1) then
	               exit
	               endif

	        xp = xp + photon%kx*dl
	        yp = yp + photon%ky*dl
	        zp = zp + photon%kz*dl
	        iclump_b = 0

		endif

	endif
                tauD = tauDf

endif

202     continue

endif	!	for tau_max

		tau_edge = tauDf

end subroutine tau_to_edge_metal_clumpy_medium

subroutine tau_to_edge_metal(photon,tau_edge)
use cons
type(photon_type) :: photon
real(kind=rkd), intent(out) :: tau_edge
real(kind=rkd) dx,dy,dz,dl
real(kind=rkd) xp,yp,zp
real(kind=rkd) sigma
real(kind=rkd) tauD,tauDf
real(kind=rkd) v_th
real(kind=rkd) a_K, dnu_th_K
real(kind=rkd) a_H, dnu_th_H
integer :: ix,iy,iz


	tauD = 0.d0
	tauDf = 0.d0
	ix = photon%ix
	iy = photon%iy
	iz = photon%iz
        xp = photon%x
        yp = photon%y
        zp = photon%z
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
!	print*,'EG',mpar%rank, dx,dy,dz 

if(grid%den(ix,iy,iz) .eq. 0.d0) then                      

                if(dx .eq. dl) then

	                if(photon%kx .gt. 0.d0) then
      	        	ix = ix + 1
                	else if(photon%kx .lt. 0.d0) then
                	ix = ix - 1
                	endif

                else if(dy .eq. dl) then

                	if(photon%ky .gt. 0.d0) then
                	iy = iy + 1
                	else if(photon%ky .lt. 0.d0) then
                	iy = iy - 1
                	endif

                else if(dz .eq. dl) then

                	if(photon%kz .gt. 0.d0) then
                	iz = iz + 1
                	else if(photon%kz .lt. 0.d0) then
                	iz = iz - 1
                	endif

                endif

                if(ix .gt. grid%N_x .or. ix .lt. 1) then
                exit
                endif

                if(iy .gt. grid%N_y .or. iy .lt. 1) then
                exit
                endif

                if(iz .gt. grid%N_z .or. iz .lt. 1) then
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

        photon%xnuK = (photon%nu-atom%nuK)/dnu_th_K
        photon%xnuH = (photon%nu-atom%nuH)/dnu_th_H
        photon%sigmaK = voigt(photon%xnuK,a_K)/dnu_th_K
        photon%sigmaH = voigt(photon%xnuH,a_H)/dnu_th_H
        sigma = atom%sigma_0*(atom%f12_K*photon%sigmaK + atom%f12_H*photon%sigmaH)/atom%f12


        photon%vel1 = photon%vel2

                tauDf = tauD + dl*grid%den(ix,iy,iz)*sigma

		if(tauDf .gt. tau_max) then
		exit
		endif

                if(dx .eq. dl) then

                        if(photon%kx .gt. 0.d0) then
                        ix = ix + 1
                        else if(photon%kx .lt. 0.d0) then
                        ix = ix - 1
                        endif

                else if(dy .eq. dl) then

                        if(photon%ky .gt. 0.d0) then
                        iy = iy + 1
                        else if(photon%ky .lt. 0.d0) then
                        iy = iy - 1
                        endif

                else if(dz .eq. dl) then

                        if(photon%kz .gt. 0.d0) then
                        iz = iz + 1
                        else if(photon%kz .lt. 0.d0) then
                        iz = iz - 1
                        endif

                endif

                if(ix .gt. grid%N_x .or. ix .lt. 1) then
                exit
                endif

                if(iy .gt. grid%N_y .or. iy .lt. 1) then
                exit
                endif

                if(iz .gt. grid%N_z .or. iz .lt. 1) then
                exit
                endif

                xp = xp + photon%kx*dl
                yp = yp + photon%ky*dl
                zp = zp + photon%kz*dl
                tauD = tauDf
endif


202     continue

		tau_edge = tauDf

end subroutine tau_to_edge_metal


subroutine tau_to_edge_hydrogen(photon, tau_edge)
use cons
type(photon_type) :: photon
real(kind=rkd), intent(out) :: tau_edge
real(kind=rkd) dx,dy,dz,dl
real(kind=rkd) xp,yp,zp
real(kind=rkd) sigma
real(kind=rkd) tauD,tauDf
real(kind=rkd) v_th, a, dnu_th
integer :: ix,iy,iz


	tauD = 0.d0
	tauDf = 0.d0
	ix = photon%ix
	iy = photon%iy
	iz = photon%iz
        xp = photon%x
        yp = photon%y
        zp = photon%z
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
!	print*,'EG',mpar%rank, dx,dy,dz 

if(grid%den(ix,iy,iz) .eq. 0.d0) then                      

                if(dx .eq. dl) then

	                if(photon%kx .gt. 0.d0) then
      	        	ix = ix + 1
                	else if(photon%kx .lt. 0.d0) then
                	ix = ix - 1
                	endif

                else if(dy .eq. dl) then

                	if(photon%ky .gt. 0.d0) then
                	iy = iy + 1
                	else if(photon%ky .lt. 0.d0) then
                	iy = iy - 1
                	endif

                else if(dz .eq. dl) then

                	if(photon%kz .gt. 0.d0) then
                	iz = iz + 1
                	else if(photon%kz .lt. 0.d0) then
                	iz = iz - 1
                	endif

                endif

                if(ix .gt. grid%N_x .or. ix .lt. 1) then
                exit
                endif

                if(iy .gt. grid%N_y .or. iy .lt. 1) then
                exit
                endif

                if(iz .gt. grid%N_z .or. iz .lt. 1) then
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

        photon%xnuK = (photon%nu-nuK)/dnu_th
        photon%xnuH = (photon%nu-nuH)/dnu_th
        photon%sigmaK = voigt(photon%xnuK,a)
        photon%sigmaH = voigt(photon%xnuH,a)
        sigma = atom%sigma_0/dnu_th*(photon%sigmaK/3.d0 + 2.d0*photon%sigmaH/3.d0)
        photon%vel1 = photon%vel2

                tauDf = tauD + dl*grid%den(ix,iy,iz)*sigma

		if(tauDf .gt. tau_max) then
		exit
		endif

                if(dx .eq. dl) then

                        if(photon%kx .gt. 0.d0) then
                        ix = ix + 1
                        else if(photon%kx .lt. 0.d0) then
                        ix = ix - 1
                        endif

                else if(dy .eq. dl) then

                        if(photon%ky .gt. 0.d0) then
                        iy = iy + 1
                        else if(photon%ky .lt. 0.d0) then
                        iy = iy - 1
                        endif

                else if(dz .eq. dl) then

                        if(photon%kz .gt. 0.d0) then
                        iz = iz + 1
                        else if(photon%kz .lt. 0.d0) then
                        iz = iz - 1
                        endif

                endif

                if(ix .gt. grid%N_x .or. ix .lt. 1) then
                exit
                endif

                if(iy .gt. grid%N_y .or. iy .lt. 1) then
                exit
                endif

                if(iz .gt. grid%N_z .or. iz .lt. 1) then
                exit
                endif

                xp = xp + photon%kx*dl
                yp = yp + photon%ky*dl
                zp = zp + photon%kz*dl
                tauD = tauDf
endif


202     continue

		tau_edge = tauDf

end subroutine tau_to_edge_hydrogen


end module tau_edge_mod
