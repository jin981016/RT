module escape_obs_mod
use cons
use memory_mod
use mpi
use char_mod
implicit none
public

public set_escape_observer
public reduce_escape_observer
public clear_escape_observer
public collect_escape_photon
public write_escape_observer
contains

subroutine set_escape_observer()

call create_shared_mem( escape%N_esc, [1])
call create_shared_mem( escape%N_dir_atom, [2])
call create_shared_mem( escape%N_esc_atom, [2])
call create_shared_mem( escape%NS_atom, [2])
call create_shared_mem( escape%NS_dust, [2])
call create_shared_mem( escape%path, [2])
call create_shared_mem( escape%Nclump, [2])
escape%nspec_total = 1.5e3 
call create_shared_mem( escape%spec_total, [2,escape%nspec_total])
call create_shared_mem( escape%spec_scat , [2,escape%nspec_total])


escape%wlmin(1) = atom%wlK*1.0d8 - escape%dwl
escape%wlmin(2) = atom%wlH*1.0d8 - escape%dwl
escape%wlmax(1) = atom%wlK*1.0d8 + escape%dwl
escape%wlmax(2) = atom%wlH*1.0d8 + escape%dwl

escape%dwl = 5.d0
escape%nspec = 3e3 
! total, scat, halo
call create_shared_mem( escape%spec   , [2,escape%nspec])
call create_shared_mem( escape%Q_spec , [2,escape%nspec])
call create_shared_mem( escape%U_spec , [2,escape%nspec])
call create_shared_mem( escape%V_spec , [2,escape%nspec])

escape%nR = 70
call create_shared_mem( escape%I_r   , [3,escape%nR])
call create_shared_mem( escape%Q_r   , [3,escape%nR])
call create_shared_mem( escape%U_r   , [3,escape%nR])
call create_shared_mem( escape%V_r   , [3,escape%nR])

escape%wlmin_com = escape%wlmin(1) - escape%dwl
escape%wlmax_com = escape%wlmax(2) + escape%dwl

!escape%NS_min = 0
!escape%NS_max = 12
!escape%nNS = 4e2
!call create_shared_mem( escape%NS, [2,escape%nNS])
!escape%nmfp = 1e3
!escape%mfp_min = -20.d0
!escape%mfp_max = +0.d0
!call create_shared_mem( escape%mfp, [2,escape%nmfp])

call MPI_BARRIER(MPI_COMM_WORLD,ierr)
end subroutine set_escape_observer

subroutine reduce_escape_observer

call reduce_mem( escape%N_esc,		shared_memory = .true.)
call reduce_mem( escape%N_dir_atom,	shared_memory = .true.)
call reduce_mem( escape%N_esc_atom,	shared_memory = .true.)
call reduce_mem( escape%NS_atom,	shared_memory = .true.)
call reduce_mem( escape%NS_dust,	shared_memory = .true.)
call reduce_mem( escape%path,		shared_memory = .true.)
call reduce_mem( escape%Nclump,		shared_memory = .true.)

call reduce_mem( escape%spec_total,	shared_memory = .true.)
call reduce_mem( escape%spec_scat ,	shared_memory = .true.)
call reduce_mem( escape%spec ,		shared_memory = .true.)
call reduce_mem( escape%Q_spec ,	shared_memory = .true.)
call reduce_mem( escape%U_spec ,	shared_memory = .true.)
call reduce_mem( escape%V_spec ,	shared_memory = .true.)

call reduce_mem( escape%I_r ,		shared_memory = .true.)
call reduce_mem( escape%Q_r ,		shared_memory = .true.)
call reduce_mem( escape%U_r ,		shared_memory = .true.)
call reduce_mem( escape%V_r ,		shared_memory = .true.)


!call reduce_mem( escape%NS,		shared_memory = .true.)
!call reduce_mem( escape%mfp,		shared_memory = .true.)

call MPI_BARRIER(MPI_COMM_WORLD,ierr)
end subroutine reduce_escape_observer

subroutine clear_escape_observer()

call destroy_mem( escape%N_esc		)
call destroy_mem( escape%N_dir_atom	)
call destroy_mem( escape%N_esc_atom	)
call destroy_mem( escape%NS_atom	)
call destroy_mem( escape%NS_dust	)
call destroy_mem( escape%path		)

call destroy_mem( escape%spec_total	)
call destroy_mem( escape%spec_scat	)
call destroy_mem( escape%spec	)
call destroy_mem( escape%Q_spec	)
call destroy_mem( escape%U_spec	)
call destroy_mem( escape%V_spec	)

call destroy_mem( escape%I_r	)
call destroy_mem( escape%Q_r	)
call destroy_mem( escape%U_r	)
call destroy_mem( escape%V_r	)

call destroy_mem( escape%NS		)
call destroy_mem( escape%mfp		)

call MPI_BARRIER(MPI_COMM_WORLD,ierr)
end subroutine clear_escape_observer


subroutine collect_escape_photon(photon)
type(photon_type) :: photon
integer :: iline, ispec, imfp, iNS, iR
real(kind=rkd) :: nu, wl, mfp, NS!, avg_mfp(2)
real(kind=rkd) :: kEx, kEy, kEz
real(kind=rkd) :: kNx, kNy, kNz
real(kind=rkd) :: akz, xp, yp, temp
real(kind=rkd) :: cos_obs, sin_obs, c2phi, s2phi
real(kind=rkd) :: cos_2obs, sin_2obs
real(kind=rkd) :: cos_psi, sin_psi, c2psi, s2psi
real(kind=rkd) :: Ip, Qp, Up, Vp
real(kind=rkd) :: Q_rot, U_rot


	iline = photon%line

!       Collecting data
        escape%N_esc(1) = escape%N_esc(1) + photon%weight
        
        if(c/photon%nu*1.d8 .lt. atom%wlc_0) then
                escape%NS_atom(1)    = escape%NS_atom(1)      + photon%NS_K
!                photon%mfp        = avg_mfp(1)/photon%NS_K
        else
                escape%NS_atom(2)    = escape%NS_atom(2)      + photon%NS_H
!                photon%mfp        = avg_mfp(2)/photon%NS_H
        endif

        escape%N_esc_atom(iline)   = escape%N_esc_atom(iline) + photon%weight
        escape%path(iline)         = escape%path(iline)       + photon%path!photon%weight*photon%path
        escape%NS_dust(iline)      = escape%NS_dust(iline)    + photon%NS_D
	escape%Nclump(iline)	   = escape%Nclump(iline)     + photon%nclump 

        if(photon%NS .eq. 0.d0) then 
                escape%N_dir_atom(iline)   = escape%N_dir_atom(iline) + 1.d0
        endif
!
!	Coordinate in projected image
!
	akz = sqrt(1.d0 - photon%kz**2)
	kNx = -photon%kx*photon%kz/akz
	kNy = -photon%ky*photon%kz/akz
	kNz = akz  

	kEx =  photon%ky/akz
	kEy = -photon%kx/akz
	kEz = 0.d0

        xp = kEx*photon%x + kEy*photon%y
        yp = kNx*photon%x + kNy*photon%y + kNz*photon%z
        xp = xp/grid%Ro
        yp = yp/grid%Ro
	temp = sqrt(xp**2 + yp**2)



!       Polarization 
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

!	Polarization as funcsion of r
        cos_psi = xp/sqrt(xp**2 + yp**2)
        sin_psi = -yp/sqrt(xp**2 + yp**2)
        c2psi = cos_psi**2 - sin_psi**2
        s2psi = 2.d0*cos_psi*sin_psi

        if(photon%NS .eq. 0.d0) then
!       Directly Escaping photon
        Q_rot = 0.d0
        U_rot = 0.d0
        else
        Q_rot =  c2psi*Qp + s2psi*Up
        U_rot = -s2psi*Qp + c2psi*Up
        endif




!
!	For spectrum
!
        nu = photon%nu*(1.d0 + photon%vel2/c)
        wl = c/nu*1.d8
        ispec = (wl - escape%wlmin(iline))/(escape%wlmax(iline) - escape%wlmin(iline))*escape%nspec_total

        if(ispec .ge. 1 .and. ispec .le. escape%nspec_total) then
        escape%spec_total(iline,ispec) = escape%spec_total(iline, ispec) + photon%weight

                if(photon%NS .gt. 0.d0) then
                escape%spec_scat(iline,ispec) = escape%spec_scat(iline, ispec) + photon%weight
                endif

        endif

        ispec = (wl - escape%wlmin_com)/(escape%wlmax_com - escape%wlmin_com)*escape%nspec

        if(ispec .ge. 1 .and. ispec .le. escape%nspec) then
        escape%spec(1,ispec)   = escape%spec(1,ispec)   + photon%weight
        escape%Q_spec(1,ispec) = escape%Q_spec(1,ispec) + Qp*photon%weight
        escape%U_spec(1,ispec) = escape%U_spec(1,ispec) + Up*photon%weight
        escape%V_spec(1,ispec) = escape%V_spec(1,ispec) + Vp*photon%weight

!                if(photon%NS .gt. 0.d0) then
!        	escape%spec(2,ispec) = escape%spec(2,ispec) + photon%weight
!                endif
                if(temp .gt. 0.1d0) then
        	escape%spec(2,ispec)   = escape%spec(2,ispec)   + photon%weight
        	escape%Q_spec(2,ispec) = escape%Q_spec(2,ispec) + Qp*photon%weight
        	escape%U_spec(2,ispec) = escape%U_spec(2,ispec) + Up*photon%weight
        	escape%V_spec(2,ispec) = escape%V_spec(2,ispec) + Vp*photon%weight
                endif

        endif

        temp = sqrt(xp**2 + yp**2)
        if(temp .le. 1.d0) then

        iR = nint(temp*(escape%nR-1)) + 1
        escape%I_r(iline,iR)   = escape%I_r(iline,iR) + photon%weight
        escape%Q_r(iline,iR)   = escape%Q_r(iline,iR) + Q_rot*photon%weight
        escape%U_r(iline,iR)   = escape%U_r(iline,iR) + U_rot*photon%weight
        escape%V_r(iline,iR)   = escape%V_r(iline,iR) + Vp*photon%weight

        escape%I_r(3,iR)   = escape%I_r(3,iR) + photon%weight
        escape%Q_r(3,iR)   = escape%Q_r(3,iR) + Q_rot*photon%weight
        escape%U_r(3,iR)   = escape%U_r(3,iR) + U_rot*photon%weight
        escape%V_r(3,iR)   = escape%V_r(3,iR) + Vp*photon%weight

	endif


end subroutine collect_escape_photon

subroutine write_escape_observer() 
character(len=120) :: fn                                                                          
integer :: ispec
real(kind=rkd) :: wl
integer :: nphoton_atom(2)
real(kind=rkd) :: DoP_spec(2)
integer :: iR
real(kind=rkd) :: R, dR, sb_r(3), DoP_r(3)
integer :: i

dR = 1.d0/(escape%nR - 1.d0)
nphoton_atom(1) = nphoton/3.d0*2.d0
nphoton_atom(2) = nphoton/3.d0

if(mpar%rank .eq. master) then

!
!	Polarization
!
	escape%Q_spec = escape%Q_spec/escape%spec
	escape%U_spec = escape%U_spec/escape%spec
	escape%V_spec = escape%V_spec/escape%spec
	
	escape%Q_r = escape%Q_r/escape%I_r
	escape%U_r = escape%U_r/escape%I_r
	escape%V_r = escape%V_r/escape%I_r

!
!
!
        write(fn,101) fn_model, '_f_esc.dat'
        call space_char(fn)
101     format(A,A)
	open(21,file=fn)

        write(fn,101) fn_model, 'spec.dat'
        call space_char(fn)
        open(31,file=fn)

        write(fn,101) fn_model, 'spec_com.dat'
        call space_char(fn)
        open(41,file=fn)

        write(fn,101) fn_model, 'radi.dat'
        call space_char(fn)
        open(51,file=fn)

	escape%N_dir_atom  = escape%N_dir_atom/nphoton_atom
	escape%N_esc_atom  = escape%N_esc_atom/nphoton_atom
	escape%path        = escape%path      /nphoton_atom
	escape%NS_atom     = escape%NS_atom   /nphoton_atom
	escape%NS_dust     = escape%NS_dust   /nphoton_atom
	escape%Nclump      = escape%Nclump    /nphoton_atom
	write(21,112) escape%N_esc(1)/nphoton, escape%N_esc_atom(1)/escape%N_esc_atom(2), &
                             escape%N_esc_atom(1), escape%N_esc_atom(2), &
                             escape%NS_atom, escape%NS_dust, escape%path, &
			     escape%N_dir_atom, escape%Nclump
112 format(<4 + 6 + 4>ES16.7)

        do ispec = 1, escape%nspec_total
                wl = -escape%dwl + 2.d0*escape%dwl*real(ispec)/escape%nspec_total
                write(31,131) wl, escape%spec_total(:,ispec), escape%spec_scat(:,ispec)
        enddo
131 format(<5>ES16.7)

        do ispec = 1, escape%nspec

                wl = escape%wlmin_com + (escape%wlmax_com - escape%wlmin_com)*real(ispec)/escape%nspec 

			do i = 1,2
			if(escape%spec(i,ispec) .ne. 0.d0) then
			DoP_spec(i) = sqrt(escape%Q_spec(i,ispec)**2 + escape%U_spec(i,ispec)**2 + escape%V_spec(i,ispec)**2)
			else
			DoP_spec(i) = 0.d0
			endif
			enddo

                write(41,141) wl, escape%spec(:,ispec), DoP_spec
        enddo
141 format(<1 + 2 + 2>ES16.7)

	do iR = 1, escape%nR

	R = (iR - 1.d0)/(escape%nR - 1.d0)

		        if(iR .eq. 1) then
		        sb_r = escape%I_r(:,iR)/(pi*(0.5d0*dR)**2)
		        else if(iR .eq. escape%nR) then
		        sb_r = escape%I_r(:,iR)/(pi*(2.d0*R+0.5d0*dR)*0.5d0*dR)
		        else
		        sb_r = escape%I_r(:,iR)/(2.d0*pi*R*dR)
		        endif

			do i = 1,3
			if(sb_r(i) .ne. 0.d0) then
			DoP_r(i) = sqrt(escape%Q_r(i,iR)**2 + escape%U_r(i,iR)**2 + escape%V_r(i,iR)**2)
			else
			DoP_r(i) = 0.d0
			endif
			enddo

		write(51,151) R, sb_r, DoP_r
	

	enddo
151 format(<1 + 3 + 3>ES16.7)


!        do imfp = 1, escape%nmfp
!                mfp = escape%mfp_min + (escape%mfp_max - escape%mfp_min)*real(imfp)/escape%nmfp
!                write(32,*) mfp, escape%mfp(:,imfp)                                                  
!        enddo

!        do iNS = 1, escape%nNS
!                NS = escape%NS_min + (escape%NS_max - escape%NS_min)*real(iNS)/escape%nNS
!                write(33,*) NS, escape%NS(:,iNS)
!        enddo



	close(21)
	close(31)
	close(41)
	close(51)
!close(32)
!close(33)
endif

call MPI_BARRIER(MPI_COMM_WORLD,ierr)

end subroutine write_escape_observer



end module escape_obs_mod
