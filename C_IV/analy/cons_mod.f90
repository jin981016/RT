module cons
use, intrinsic :: iso_fortran_env, only: real32, real64, real128, int32, int64
implicit none
public

integer, parameter :: rkd = real64
real(kind=rkd), parameter :: pi = 4.*atan(1.d0)
real(kind=rkd), parameter :: k = 1.38064852d-16 ! Boltzmann Constant : cgs
real(kind=rkd), parameter :: m_H = 1.6737236d-24 ! Hydrogen Mass : g 
real(kind=rkd), parameter :: m_u = 1.66054d-24 	 ! Mass for 1 u : g 
real(kind=rkd), parameter :: m_e = 9.10938356d-28! Electron Mass : g 
real(kind=rkd), parameter :: e = 1.602d-19*3e9  ! Electric charge : emu
real(kind=rkd), parameter :: c = 2.99792458d10  ! Light Speed : cm/s
real(kind=rkd), parameter :: c_km = 2.99792458d5  ! Light Speed : cm/s
!		Atomic data of Lya
!		K : 1s - 2p3/2
!		H : 1s - 2p1/2
real(kind=rkd), parameter :: gamma = 6.265d8	! Einstein Coefficient A : /s
real(kind=rkd), parameter :: f12 = 0.416400     ! Oscillator Strengths
real(kind=rkd), parameter :: nuK = c/1215.673644609d-8
real(kind=rkd), parameter :: nuH = c/1215.668237310d-8
real(kind=rkd), parameter :: nu_0 = (nuK + 2.d0*nuH)/3.d0 
real(kind=rkd), parameter :: wlc_0 = c/nu_0*1.d8  ! center wavelength Angstrom          
real(kind=rkd), parameter :: sigma_0 = f12*sqrt(pi)*e**2/(m_e*c)
real(kind=rkd), parameter :: pc = 3.086d+18	! cm
real(kind=rkd), parameter :: kpc = 3.086d+21	! cm
real(kind=rkd), parameter :: AU  = 1.496d+13	! cm
real(kind=rkd), parameter :: tau_max = 300	! maximum tau
integer,parameter :: master = 0                                                 
integer :: ierr
integer :: unit
integer :: info
character(len=100) :: fn_model
real(kind=rkd) :: time_i, time_f

!
!	For atomic data
!		K : 1s - 2p3/2
!		H : 1s - 2p1/2
type atom_type
real(kind=rkd) :: gamma_K         ! Einstein Coefficient A for K : s^-1
real(kind=rkd) :: gamma_H         ! Einstein Coefficient A for H : s^-1
real(kind=rkd) :: f12		! Oscillator Strength
real(kind=rkd) :: f12_K		! Oscillator Strength
real(kind=rkd) :: f12_H		! Oscillator Strength
real(kind=rkd) :: wlK		! center wavelength of K
real(kind=rkd) :: wlH		! center wavelength of H
real(kind=rkd) :: nuK		! center freqeuncy of K
real(kind=rkd) :: nuH		! center freqeuncy of H
real(kind=rkd) :: nu_0		! Average freqeuncy
real(kind=rkd) :: wlc_0		! Average wavelength
real(kind=rkd) :: sigma_0	! Cross Section Parameter
real(kind=rkd) :: mass		! Mass : g 
end type atom_type

!
!	For MPI
!
type mpi_type
integer :: rank, nproc
integer :: h_rank, h_nproc, h_comm 
integer :: SUB_COMM, sub_nproc
end type mpi_type
!
!	For Photon in simulation
!
type photon_type

real(kind=rkd) :: kx,ky,kz
real(kind=rkd) :: weight                ! weight
real(kind=rkd) :: x,y,z
real(kind=rkd) :: x_s,y_s,z_s
real(kind=rkd) :: nu, nu_atom, vz
integer :: line ! 1 and 2 are K and H line
real(kind=rkd) :: vel1, vel2, v_th
logical :: esc
integer :: ix,iy,iz
integer :: NS, NS_K, NS_H, NS_D

real(kind=rkd) :: I, Q, U, V
real(kind=rkd) :: E1, E3, DoP
real(kind=rkd) :: e1x,e1y,e1z   ! \hat n
real(kind=rkd) :: mx, my, mz ! \hat m
real(kind=rkd) :: nx, ny, nz ! \hat n

real(kind=rkd) :: sigmaK, sigmaH, sigma
real(kind=rkd) :: xnuK, xnuH

!real(kind=rkd) :: tau
real(kind=rkd) :: tau_atom, tau_dust
real(kind=rkd) :: D_cl 
integer :: iclump
logical :: overlap
logical :: clump
integer :: ip
integer :: np
integer :: nclump
real(kind=rkd) :: path
!	initial information
real(kind=rkd) :: nu_i
real(kind=rkd) :: k_i(3) 
real(kind=rkd) :: r_i(3) 

end type photon_type
!
!	For dust 
!
type dust_type
!               Mueller Matrix
        real(kind=rkd), pointer :: cos_scat(:), S11(:), S12(:), S33(:), S34(:)
        integer :: w_cos_scat, w_S11, w_S12, w_S33, w_S34
!               Phase Function
        integer :: n_phase = 300
        real(kind=rkd), pointer :: P_cos(:), cthe(:)
        integer :: w_P_cos, w_cthe
!               Extinction, Albedo, g, ncos
        real(kind=rkd) :: wl, Cext, albedo, cos_avg 
        integer :: ncos
end type dust_type

!
!	For grid-based goemetry
!
type grid_type

real(kind=rkd) :: dnu_th, v_th
real(kind=rkd) :: a,T
integer :: N_XYZ
integer :: N_X, N_Y, N_Z
real(kind=rkd), pointer :: den(:,:,:) => null()
real(kind=rkd), pointer :: den_d(:,:,:) => null()
real(kind=rkd), pointer :: Tem(:,:,:) => null()
real(kind=rkd), pointer :: v_ran(:,:,:) => null()
real(kind=rkd), pointer :: vx(:,:,:) => null()
real(kind=rkd), pointer :: vy(:,:,:) => null()
real(kind=rkd), pointer :: vz(:,:,:) => null()
real(kind=rkd), pointer :: X(:) => null()
real(kind=rkd), pointer :: Y(:) => null()
real(kind=rkd), pointer :: Z(:) => null()
real(kind=rkd) :: dx, dy, dz 
real(kind=rkd) :: Ri, Rxy, Rz, Ro
real(kind=rkd) :: NH
real(kind=rkd) :: max_len

end type grid_type
!
!	Observer, peeling off
!
type observer_type
real(kind=rkd) :: kx, ky, kz
real(kind=rkd) :: kNx, kNy, kNz
real(kind=rkd) :: kEx, kEy, kEz
integer :: nx,ny, nspec, resol
real(kind=rkd) :: wlmin, wlmax
!real(kind=rkd), pointer :: I_2d(:,:) => null()
!real(kind=rkd), pointer :: Q_2d(:,:) => null()
!real(kind=rkd), pointer :: U_2d(:,:) => null()
!real(kind=rkd), pointer :: V_2d(:,:) => null()
!	Spectrum
real(kind=rkd), pointer :: spec(:) => null()
real(kind=rkd), pointer :: spec_halo(:) => null()
real(kind=rkd), pointer :: spec_scat(:) => null()
!	2D image
real(kind=rkd), pointer :: sb(:,:) => null()
real(kind=rkd), pointer :: vel_2d(:,:) => null()
real(kind=rkd), pointer :: width_2d(:,:) => null()
real(kind=rkd), pointer :: IFU(:,:,:) => null()
!	projected radius
real(kind=rkd), pointer :: sb_r(:) => null()
real(kind=rkd), pointer :: sb_K_r(:) => null()
real(kind=rkd), pointer :: sb_H_r(:) => null()
real(kind=rkd), pointer :: pol_r(:) => null()
real(kind=rkd), pointer :: ratio_r(:) => null()
!	Static
real(kind=rkd), pointer :: N_total(:) => null()
real(kind=rkd), pointer :: N_direct(:) => null()
real(kind=rkd), pointer :: NS(:) => null()
real(kind=rkd), pointer :: NS_K(:) => null()
real(kind=rkd), pointer :: NS_H(:) => null()
real(kind=rkd), pointer :: path(:) => null() 
end type observer_type

type escape_observer_type
real(kind=rkd) :: wlmin(2), wlmax(2)
real(kind=rkd) :: wlmin_com, wlmax_com
real(kind=rkd), pointer :: N_esc(:) => null()
real(kind=rkd), pointer :: N_esc_atom(:) => null()
real(kind=rkd), pointer :: N_dir_atom(:) => null()
real(kind=rkd), pointer :: NS_atom(:) => null()
real(kind=rkd), pointer :: NS_dust(:) => null()
real(kind=rkd), pointer :: path(:) => null()
real(kind=rkd), pointer :: Nclump(:) => null()
integer :: nspec_total
integer :: nspec
integer :: dwl
real(kind=rkd), pointer :: spec_total(:,:) => null() 
real(kind=rkd), pointer :: spec_scat(:,:) => null() 
real(kind=rkd), pointer :: spec(:,:) => null() 
real(kind=rkd), pointer :: Q_spec(:,:) => null() 
real(kind=rkd), pointer :: U_spec(:,:) => null() 
real(kind=rkd), pointer :: V_spec(:,:) => null() 
integer :: nR
real(kind=rkd), pointer :: I_r(:,:) => null()
real(kind=rkd), pointer :: Q_r(:,:) => null()
real(kind=rkd), pointer :: U_r(:,:) => null()
real(kind=rkd), pointer :: V_r(:,:) => null()
integer :: nNS, nmfp
real(kind=rkd) :: NS_min, NS_max
real(kind=rkd), pointer :: NS(:,:) => null() 
real(kind=rkd) :: mfp_min, mfp_max 
real(kind=rkd), pointer :: mfp(:,:) => null() 
end type escape_observer_type


type single_clump_type
real(kind=rkd) :: den,r,v_th 
end type single_clump_type

type clumps_type
integer :: N_total
real(kind=rkd) :: den, den_d, r, v_th
integer, pointer :: N(:,:,:) => null()
integer, pointer :: init_i(:,:,:) => null()
real(kind=rkd), pointer :: x(:) => null()
real(kind=rkd), pointer :: y(:) => null()
real(kind=rkd), pointer :: z(:) => null()
real(kind=rkd), pointer :: vx(:) => null()
real(kind=rkd), pointer :: vy(:) => null()
real(kind=rkd), pointer :: vz(:) => null()
end type clumps_type



type clump_grid_type
integer :: n
integer, allocatable :: idx(:)
end type clump_grid_type


type par_type
!	Geometry
!	RT
integer :: ny, nx, nspec
integer :: nR 
integer :: iobs, Nobserver
real(kind=rkd) :: wlmin, wlmax
end type par_type

!
!	Global Parameters
!
type(mpi_type) :: mpar
type(observer_type), allocatable :: observer(:)
type(escape_observer_type) :: escape
type(clumps_type) :: clumps
type(single_clump_type) :: single_clump
type(grid_type) :: grid
type(par_type) :: par
type(atom_type) :: H_I, Mg_II, C_IV, O_VI, N_V, atom
type(dust_type) :: dust
integer :: nphoton



end module cons
