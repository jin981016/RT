module random
!-----
!--- Random Number Generator Module
!--- Use Mersenne Twister (MT) random generator
!
! Author: Kwang-Il Seon
!
! Note:
!    - gfortran random_number uses KISS algorithm while ifort uses Lecuyer algorithm.
!    - intrinsic random_number routine is not thread-safe with openmp.
!    - the intial seed in each thread should be different from thread to thread.
!
! Usage:
!    call init_random_seed()     to initialize random seed
!    call random_number(harvest) to call as a subroutine
!    harvest = rand_number()     to call as a function
!         where harvest is a number (32- or 64-bit) or an 1-, 2-, or 3-dimentional array.
!
! History:
!   v2.29 (2018/06/29) added rand_planck and rand_planck_num.
!   v2.28 (2018/02/08) numerical bugs fixed in rand_resonance_vz_seon, Negative values in sqrt (x1) and Division by zero (h1).
!   v2.27 (2018/01/23) added rand_r2exp and rand_pick. rand_rexp is now faster than the algorithm given by Baes (2003).
!   v2.26 (2017/12/19) added rand_resonance_vz_seon, hoping to be faster.
!   v2.25 (2017/09/05) added rand_resonance, rand_resonance_rybicki, and "new" rand_voigt.
!         changed the name of "old" rand_voigt into rand_resonance_vz
!         Now, rand_voigt generates x (frequency), which follows the normalized voigt function.
!   v2.24 (2017/09/03) added random_3Dsphere
!   v2.23 (2017/08/18) minor bug-fixed for gfortran
!         real(rand_number()) in rand_expdev32
!         function rand_bactrian -> function rand_bactrian()
!   v2.22 (2017/06/25)
!         bug-fixed in random_mt_seed (seed values have been assigned to seed for KISS generator)
!         bug-fixed in init_random_mt_seed.
!                When an input non-zero "iseed" is provided by user, "seed"s for each threads were the same.
!         modified to use "random_kiss_seed", instead of "random_seed", "in init_random_kiss_seed."
!                But, we are not using KISS generator.
!   v2.21 (2016/10/26)
!         bug-fixed in u0 calculation in rand_resonance_vz
!   v2.2  (2016/10/26)
!         added rand_cauchy (= rand_lorentz)
!   v2.1  (2016/10/23)
!         added rand_resonance_vz and rand_rayleigh
!   v2b(2016/02/20)
!        added random_mvnorm and make_mvcovmat2
!   v2a(2016/02/16)
!        added rand_t, rand_gamma, rand_scaled_inv_chi2
!   v2 (2016/01/15)
!        modified to use the same calling method as the intrinsic procedure defined in fortran.
!   2015/12/11,
!        To make "save" variables thread safe, they should be declared in THREADPRIVATE directive.
!        http://sc.tamu.edu/IBM.Tutorial/docs/Compilers/xlf_8.1/html/lr225.HTM
!   2012/05/04, Initial version
!-----
  use, intrinsic :: iso_fortran_env, only: real32, real64, int32, int64
  implicit none
  private
  integer, parameter  :: sp = real32
  integer, parameter  :: dp = real64
  integer, parameter  :: wp = dp
  real(wp), parameter :: PI = 3.141592653589793238462643383279502884197_wp
  real(wp), parameter :: TWOPI  = PI*2.0_wp
  real(wp), parameter :: HALFPI = PI/2.0_wp
  real(wp), parameter :: FOURPI = PI*4.0_wp

  public init_random_seed
  public random_seed, random_number, random_gauss, random_3Dsphere, random_sphere, random_t
  public rand_number, rand_gauss, rand_exp, rand_rexp, rand_r2exp, rand_zexp, rand_sech2
  public rand_permutation, rand_pick, rand_cyclic_permutation, rand_index, rand_binomial, rand_multinomial
  public rand_t, rand_gamma, rand_scaled_inv_chi2
  public rand_bactrian
  public rand_stretch, rand_traverse
  !public rand_walk
  public make_mvcovmat2, random_mvnorm, make_mvcovmat
  public rand_resonance, rand_resonance_rybicki, rand_voigt
  public rand_resonance_vz, rand_rayleigh, rand_cauchy, rand_lorentz, rand_henyey_greenstein
  public rand_planck, rand_planck_num

  !======= parameter for Mersenne Twister Random Number Generator (MT19937)
  ! Period parameters
  INTEGER, PARAMETER :: mt_n = 624, mt_n1 = mt_n+1, mt_m = 397, mt_mata = -1727483681 ! constant vector a
  INTEGER, PARAMETER :: mt_seed0 = 4357
  INTEGER, SAVE      :: mt(0:mt_n-1)     ! the array for the state vector
  INTEGER, SAVE      :: mti = mt_n1      ! mti == N+1 means mt(:) is not initialized
  !$OMP THREADPRIVATE(mt, mti)

  !====== parameters for KISS Random Number Generator
  integer(int64), save :: seed_kiss(4)   = [ 1234567890987654321_int64, 362436362436362436_int64,&
                                                1066149217761810_int64, 123456123456123456_int64 ]
  integer(int32), save :: seed_kiss32(4) = [ 123456789, 362436069, 521288629, 916191069 ]
  !$OMP THREADPRIVATE(seed_kiss,seed_kiss32)

  !=================================================================================
  !--- If you want to use Marsaglis KISS (Keep It Simple and Stupid) RNGs, then uncomment the following lines.
  !interface random_seed
  !   module procedure random_kiss_seed
  !end interface random_seed

  !interface random_number
  !   module procedure random_kiss32,random_kiss64,random_kiss_v32,random_kiss_v64,&
  !                    random_kiss_vv32,random_kiss_vv64,random_kiss_vvv32,random_kiss_vvv64
  !end interface random_number

  !interface rand_number
  !   module procedure kiss64
  !end interface rand_number
  !=================================================================================

  !=================================================================================
  !--- If you want to use Marsaglis KISS (Keep It Simple and Stupid) RNGs, then comment out the following lines.
  interface init_random_seed
     module procedure init_random_mt_seed
  end interface init_random_seed

  interface random_seed
     module procedure random_mt_seed
  end interface random_seed

  interface random_number
     module procedure random_mt32,random_mt64,random_mt_v32,random_mt_v64,&
                      random_mt_vv32,random_mt_vv64,random_mt_vvv32,random_mt_vvv64
  end interface random_number
  !=================================================================================

  interface rand_resonance_vz
#ifdef ZHENG
     module procedure rand_resonance_vz_zheng
#else
     module procedure rand_resonance_vz_seon
#endif
  end interface
  interface rand_lorentz
     module procedure rand_cauchy
  end interface

  interface random_t
     module procedure random_t0,random_t1
  end interface

  interface random_gauss
     module procedure random_gauss1,random_gauss2,random_gauss3
  end interface random_gauss

  interface rand_gauss
     module procedure rand_gauss1
  end interface rand_gauss

  interface random_3Dsphere
     module procedure random_3Dsphere1, random_3Dsphere2
  end interface random_3Dsphere

  interface rand_gamma
     module procedure gamdev, gamdev_r
  end interface rand_gamma

  interface rand_exp
     module procedure rand_exp32,rand_exp64
  end interface rand_exp

  interface rand_index
     module procedure rand_index1, rand_indexn
  end interface rand_index

  interface rand_planck
     module procedure rand_planck32,rand_planck64
  end interface rand_planck
  interface rand_planck_num
     module procedure rand_planck_num32,rand_planck_num64
  end interface rand_planck_num
contains
!==================================================================
  subroutine random_kiss32(harvest)
     implicit none
     real(sp), intent(out) :: harvest
     harvest = kiss32()
  end subroutine random_kiss32
  subroutine random_kiss64(harvest)
     implicit none
     real(dp), intent(out) :: harvest
     harvest = kiss64()
  end subroutine random_kiss64
  subroutine random_kiss_v32(harvest)
     implicit none
     real(sp), intent(out) :: harvest(:)
     integer :: i
     do i=1, size(harvest)
        harvest(i) = kiss32()
     enddo
  end subroutine random_kiss_v32
  subroutine random_kiss_v64(harvest)
     implicit none
     real(dp), intent(out) :: harvest(:)
     integer :: i
     do i=1, size(harvest)
        harvest(i) = kiss64()
     enddo
  end subroutine random_kiss_v64
  subroutine random_kiss_vv32(harvest)
     implicit none
     real(sp), intent(out) :: harvest(:,:)
     real(sp) :: vv(size(harvest))
     call random_kiss_v32(vv)
     harvest = reshape(vv, [size(harvest,1), size(harvest,2)])
  end subroutine random_kiss_vv32
  subroutine random_kiss_vv64(harvest)
     implicit none
     real(dp), intent(out) :: harvest(:,:)
     real(dp) :: vv(size(harvest))
     call random_kiss_v64(vv)
     harvest = reshape(vv, [size(harvest,1), size(harvest,2)])
  end subroutine random_kiss_vv64
  subroutine random_kiss_vvv32(harvest)
     implicit none
     real(sp), intent(out) :: harvest(:,:,:)
     real(sp) :: vv(size(harvest))
     call random_kiss_v32(vv)
     harvest = reshape(vv, [size(harvest,1), size(harvest,2), size(harvest,3)])
  end subroutine random_kiss_vvv32
  subroutine random_kiss_vvv64(harvest)
     implicit none
     real(dp), intent(out) :: harvest(:,:,:)
     real(dp) :: vv(size(harvest))
     call random_kiss_v64(vv)
     harvest = reshape(vv, [size(harvest,1), size(harvest,2), size(harvest,3)])
  end subroutine random_kiss_vvv64
!==================================================================
  subroutine random_mt32(harvest)
     implicit none
     real(sp), intent(out) :: harvest
     harvest = rand_number()
  end subroutine random_mt32
  subroutine random_mt64(harvest)
     implicit none
     real(dp), intent(out) :: harvest
     harvest = rand_number()
  end subroutine random_mt64
  subroutine random_mt_v32(harvest)
     implicit none
     real(sp), intent(out) :: harvest(:)
     integer :: i
     do i=1, size(harvest)
        harvest(i) = rand_number()
     enddo
  end subroutine random_mt_v32
  subroutine random_mt_v64(harvest)
     implicit none
     real(dp), intent(out) :: harvest(:)
     integer :: i
     do i=1, size(harvest)
        harvest(i) = rand_number()
     enddo
  end subroutine random_mt_v64
  subroutine random_mt_vv32(harvest)
     implicit none
     real(sp), intent(out) :: harvest(:,:)
     real(sp) :: vv(size(harvest))
     call random_mt_v32(vv)
     harvest = reshape(vv, [size(harvest,1), size(harvest,2)])
  end subroutine random_mt_vv32
  subroutine random_mt_vv64(harvest)
     implicit none
     real(dp), intent(out) :: harvest(:,:)
     real(dp) :: vv(size(harvest))
     call random_mt_v64(vv)
     harvest = reshape(vv, [size(harvest,1), size(harvest,2)])
  end subroutine random_mt_vv64
  subroutine random_mt_vvv32(harvest)
     implicit none
     real(sp), intent(out) :: harvest(:,:,:)
     real(sp) :: vv(size(harvest))
     call random_mt_v32(vv)
     harvest = reshape(vv, [size(harvest,1), size(harvest,2), size(harvest,3)])
  end subroutine random_mt_vvv32
  subroutine random_mt_vvv64(harvest)
     implicit none
     real(dp), intent(out) :: harvest(:,:,:)
     real(dp) :: vv(size(harvest))
     call random_mt_v64(vv)
     harvest = reshape(vv, [size(harvest,1), size(harvest,2), size(harvest,3)])
  end subroutine random_mt_vvv64
  !----------------------------------
  function kiss32() result(u)
  !------------------------------------------------------------------
  ! Pseudo-random uniform 32-bit integer random number generator
  ! The Marsaglia KISS (Keep It Simple Stupid) random number generator
  ! returns a sequence of pseudo-random 32-bit integers. It combines:
  ! (1) The congruential generator x(n)=69069*x(n-1)+1327217885, period 2^32.
  ! (2) A 3-shift shift-register generator, period 2^32-1,
  ! (3) Two 16-bit multiply-with-carry generators, period 597273182964842497 > 2^59
  !  The overall period is > 2^123. Consider this a mininum standard for
  ! generating random integers.  Integers range from -huge(0) to +huge(0)
  ! (this is the range [-2^31-1, 2^31-1] for 32-bit integers.
  !
  ! http://computer-programming-forum.com/49-fortran/b89977aa62f72ee8.htm
  !-----------------
  ! Kwang-Il Seon, 2013 April 06
  !    Modified to return 0.0 < u < 1.0 (u1 <= u <= u2)
  !------------------------------------------------------------------
    implicit none
    integer :: m,k,n
    integer :: kiss
    real(sp) :: u

    real(sp), parameter :: x1 = 129.0, x2 = 257.0
    real(sp), parameter :: a  = 1.0/(2.0*real(huge(0),sp)+1.0+x1+x2)
    real(sp), parameter :: b  = (real(huge(0),sp)+1.0+x1)/(2.0*real(huge(0),sp)+1.0+x1+x2)

    m(k, n) = ieor(k, ishft(k, n))
    seed_kiss32(1) = 69069 * seed_kiss32(1) + 1327217885
    seed_kiss32(2) = m(m(m(seed_kiss32(2), 13), -17), 5)
    seed_kiss32(3) = 18000 * iand(seed_kiss32(3), 65535) + ishft(seed_kiss32(3), - 16)
    seed_kiss32(4) = 30903 * iand(seed_kiss32(4), 65535) + ishft(seed_kiss32(4), - 16)
    kiss = seed_kiss32(1) + seed_kiss32(2) + ishft(seed_kiss32(3), 16) + seed_kiss32(4)
    u    = b + a*real(kiss,sp)
  end function kiss32
  !----------------------------------
  function kiss64() result(u)
  !--------------------------------------------------------------------
  ! The 64-bit KISS (Keep It Simple Stupid) random number generator.
  ! Components:
  !  (1) Xorshift (XSH), period 2^64-1,
  !  (2) Multiply-with-carry (MWC), period (2^121+2^63-1)
  !  (3) Congruential generator (CNG), period 2^64.
  ! Overall period: (2^250+2^192+2^64-2^186-2^129)/6 ~= 2^(247.42) or 10^(74.48)
  !
  ! https://groups.google.com/forum/#!msg/comp.lang.fortran/qFv18ql_WlU/IK8KGZZFJx4J
  ! or http://fortranwiki.org/fortran/show/kiss64
  !-----------------
  ! Kwang-Il Seon, 2014 October 08, 2015-06-03
  !    Modified to return 0.0 < u < 1.0.
  !--------------------------------------------------------------------
    implicit none
    integer(int64) :: kiss,t
    integer(int64) :: m,s,x,k
    !real(int64)    :: u
    real(dp)       :: u

    real(dp), parameter :: x1 = 1025.0_dp, x2 = 2049.0_dp
    real(dp), parameter :: a  = 1.0_dp/(2.0_dp*real(huge(0_int64),dp)+1.0_dp+x1+x2)
    real(dp), parameter :: b  = (real(huge(0_int64),dp)+1.0_dp+x1)/(2.0_dp*real(huge(0_int64),dp)+1.0_dp+x1+x2)
    ! statement functions
    m(x,k) = ieor(x,ishft(x,k))
    s(x)   = ishft(x,-63)

    t = ishft(seed_kiss(1),58) + seed_kiss(4)
    if (s(seed_kiss(1)) == s(t)) then
      seed_kiss(4) = ishft(seed_kiss(1),-6) + s(seed_kiss(1))
    else
      seed_kiss(4) = ishft(seed_kiss(1),-6) + 1 - s(seed_kiss(1)+t)
    endif
    seed_kiss(1) = t + seed_kiss(1)
    seed_kiss(2) = m(m(m(seed_kiss(2),13_int64), -17_int64), 43_int64)
    seed_kiss(3) = 6906969069_int64 * seed_kiss(3) + 1234567_int64
    kiss         = seed_kiss(1) + seed_kiss(2) + seed_kiss(3)
    u            = b + a*real(kiss,dp)
  end function kiss64
!----------------------------------
!==================================
  !-----------------------------------------------------------------------
  !   genrand() generates one pseudorandom real number (double) which is
  ! uniformly distributed on [0,1]-interval, for each call.
  ! sgenrand(seed) set initial values to the working area of 624 words.
  ! Before genrand(), sgenrand(seed) must be called once.  (seed is any 32-bit
  ! integer except for 0).
  !-----------------------------------------------------------------------
  ! Mersenne Twister Random Number Generator (MT19937)
  !   Matsumoto & Nishimura (1998, ACM Transactions of Modeling and Computer Simulation, 8, 3)
  !   Period = 2^19937 - 1 ~ 10^6001
  ! Fortran translation by Hiroshi Takano.  Jan. 13, 1999.
  !   genrand()      -> double precision function grnd()
  !   sgenrand(seed) -> subroutine sgrnd(seed)
  !                     integer seed
  !
  ! Copyright (C) 1997 Makoto Matsumoto and Takuji Nishimura.
  ! When you use this, send an email to: matumoto@math.keio.ac.jp
  ! with an appropriate reference to your work.
  !-----------------------------------------------------------------------
  SUBROUTINE sgrnd(seed)
  ! This is the original version of the seeding routine.
  ! It was replaced in the Japanese version in C on 26 January 2002
  ! It is recommended that routine init_genrand is used instead.

  INTEGER, INTENT(IN)   :: seed
  !    setting initial seeds to mt[N] using the generator Line 25 of Table 1 in
  !    [KNUTH 1981, The Art of Computer Programming Vol. 2 (2nd Ed.), pp102]

  mt(0) = IAND(seed, -1)
  DO  mti=1,mt_n-1
    mt(mti) = IAND(69069 * mt(mti-1), -1)
  END DO

  RETURN
  END SUBROUTINE sgrnd

  !-----------------------------------------------------------------------
  SUBROUTINE init_mt(seed)
  ! This initialization is based upon the multiplier given on p.106 of the
  ! 3rd edition of Knuth, The Art of Computer Programming Vol. 2.
  ! This version assumes that integer overflow does NOT cause a crash.

  INTEGER, INTENT(IN)  :: seed
  INTEGER  :: latest

  mt(0)  = seed
  latest = seed
  DO mti = 1, mt_n-1
    latest  = IEOR( latest, ISHFT( latest, -30 ) )
    latest  = latest * 1812433253 + mti
    mt(mti) = latest
  END DO

  RETURN
  END SUBROUTINE init_mt
  !-----------------------------------------------------------------------
  FUNCTION rand_number() RESULT(fn_val)
  REAL (dp) :: fn_val

  INTEGER, SAVE :: mag01(0:1) = [0, mt_mata]
  !$OMP THREADPRIVATE(mag01)
  INTEGER       :: kk, y
  ! Period parameters
  INTEGER, PARAMETER :: lmask =  2147483647                            ! least significant r bits
  INTEGER, PARAMETER :: umask = -2147483647 - 1                        ! most significant w-r bits
  ! Tempering parameters
  INTEGER, PARAMETER  :: tmaskb= -1658038656, tmaskc= -272236544
  real(dp), parameter :: a = 1.0_real64/4294967297.0_real64
  real(dp), parameter :: b = 2147483649.0_real64/4294967297.0_real64
  !  ishft(i,n): If n > 0, shifts bits in i by n positions to left.
  !              If n < 0, shifts bits in i by n positions to right.
  !  iand (i,j): Performs logical AND on corresponding bits of i and j.
  !  ior  (i,j): Performs inclusive OR on corresponding bits of i and j.
  !  ieor (i,j): Performs exclusive OR on corresponding bits of i and j.
  !  statement functions
  integer :: tshftu, tshfts, tshftt, tshftl
  tshftu(y) = ISHFT(y,-11)
  tshfts(y) = ISHFT(y,7)
  tshftt(y) = ISHFT(y,15)
  tshftl(y) = ISHFT(y,-18)

  IF(mti >= mt_n) THEN       !  generate N words at one time
    IF(mti == mt_n+1) THEN   !  if sgrnd() has not been called,
      !CALL sgrnd(4357)       !  a default initial seed is used
      CALL init_mt(mt_seed0)  !  a default initial seed is used
!      CALL init_random_seed(mt_seed0)  !  a default initial seed is used
    END IF

    DO  kk = 0, mt_n-mt_m-1
      y      = IOR(IAND(mt(kk),umask), IAND(mt(kk+1),lmask))
      mt(kk) = IEOR(IEOR(mt(kk+mt_m), ISHFT(y,-1)),mag01(IAND(y,1)))
    END DO
    DO  kk = mt_n-mt_m, mt_n-2
      y      = IOR(IAND(mt(kk),umask), IAND(mt(kk+1),lmask))
      mt(kk) = IEOR(IEOR(mt(kk+(mt_m-mt_n)), ISHFT(y,-1)),mag01(IAND(y,1)))
    END DO
    y          = IOR(IAND(mt(mt_n-1),umask), IAND(mt(0),lmask))
    mt(mt_n-1) = IEOR(IEOR(mt(mt_m-1), ISHFT(y,-1)),mag01(IAND(y,1)))
    mti        = 0
  END IF

  y   = mt(mti)
  mti = mti + 1
  y = IEOR(y, tshftu(y))
  y = IEOR(y, IAND(tshfts(y),tmaskb))
  y = IEOR(y, IAND(tshftt(y),tmaskc))
  y = IEOR(y, tshftl(y))

  !IF (y < 0) THEN
  !  fn_val = (DBLE(y) + 2.0D0**32) / (2.0D0**32 - 1.0D0)
  !ELSE
  !  fn_val = DBLE(y) / (2.0D0**32 - 1.0D0)
  !END IF
  fn_val = b + a * dble(y)

  RETURN
  END FUNCTION rand_number
!==================================

!----------------------------------
  subroutine random_kiss_seed_(seed_size,put,get)
    ! size is an intrinsic function so that we need to define an intermediate routine
    ! which use the argument name "seed_size" instead of "size"
    ! random_kiss_seed has the optional argument "size"
    implicit none
    integer, optional, intent(out) :: seed_size
    integer, optional, intent(in)  :: put(:)
    integer, optional, intent(out) :: get(:)
    ! local variable
    integer :: n

    if (present(seed_size)) then
       seed_size = 4
    endif
    if (present(put)) then
       n              = minval([4,size(put)])
       seed_kiss(1:n) = put(1:n)
    endif
    if (present(get)) then
       n        = minval([4,size(get)])
       get(1:n) = seed_kiss(1:n)
    endif
  end subroutine random_kiss_seed_
  subroutine random_kiss_seed(size,put,get)
    implicit none
    integer, optional, intent(out) :: size
    integer, optional, intent(in)  :: put(:)
    integer, optional, intent(out) :: get(:)
    call random_kiss_seed_(seed_size=size,put=put,get=get)
  end subroutine random_kiss_seed
!----------
  subroutine random_mt_seed_(seed_size,put,get)
    ! size is an intrinsic function so that we need to define an intermediate routine
    ! which use the argument name "seed_size" instead of "size"
    ! random_kiss_seed has the optional argument "size"
    ! bug-fixed, 2017-06-25
    implicit none
    integer, optional, intent(out) :: seed_size
    integer, optional, intent(in)  :: put(:)
    integer, optional, intent(out) :: get(:)
    ! local variable
    integer :: n

    if (present(seed_size)) then
       seed_size = mt_n
    endif
    if (present(put)) then
       n              = minval([mt_n,size(put)])
       !seed_kiss(1:n) = put(1:n)
       mt(0:n-1) = put(1:n)
    endif
    if (present(get)) then
       n        = minval([mt_n,size(get)])
       !get(1:n) = seed_kiss(1:n)
       get(1:n) = mt(0:n-1)
    endif
  end subroutine random_mt_seed_
  subroutine random_mt_seed(size,put,get)
    implicit none
    integer, optional, intent(out) :: size
    integer, optional, intent(in)  :: put(:)
    integer, optional, intent(out) :: get(:)
    call random_mt_seed_(seed_size=size,put=put,get=get)
  end subroutine random_mt_seed
!----------
  subroutine init_random_kiss_seed(iseed)
#ifdef _OPENMP
    use omp_lib
#endif
#ifdef MPI
    use mpi
#endif
    implicit none
    integer, optional    :: iseed
    ! local variable
    integer, allocatable :: seed(:)
    integer        :: i, n, un, istat, dt(8), pid
    integer(int64) :: t
    integer        :: getpid
    logical        :: set_iseed
#ifdef MPI
    integer        :: myid, ierr
#endif

    call random_kiss_seed(size = n)
    allocate(seed(n))

    set_iseed = .false.
    if (present(iseed)) then
       if (iseed /= 0 .and. iseed /= -999) set_iseed = .true.
    endif
    if (.not. set_iseed) then
       ! First try if the OS provides a random number generator
       ! return an integer uniform random deviate between [0 and huge(0)] from /dev/urandom if successful.
       open(newunit=un, file="/dev/urandom", access="stream", &
            form="unformatted", action="read", status="old", iostat=istat)
       if (istat == 0) then
          read(un) seed
          close(un)
       else
          ! Fallback to XOR:ing the current time and pid. The PID is
          ! useful in case one launchs multiple instances of the same
          ! program in parallel.
          call system_clock(t)
          if (t == 0) then
             call date_and_time(values=dt)
             t = (dt(1) - 1970) * 365_int64 * 24 * 60 * 60 * 1000 &
                  + dt(2) * 31_int64 * 24 * 60 * 60 * 1000 &
                  + dt(3) * 24_int64 * 60 * 60 * 1000 &
                  + dt(5) * 60 * 60 * 1000 &
                  + dt(6) * 60 * 1000 + dt(7) * 1000 &
                  + dt(8)
          end if
#ifdef _OPENMP
          ! OPENMP does not launch multiple instances.
          pid = getpid() + 9999*omp_get_thread_num()
#else
          pid = getpid()
#endif
          t   = ieor(t, int(pid, kind(t)))
          do i = 1, n
             seed(i) = shr3(t)
          end do
       end if
    else
       t = iseed
#ifdef _OPENMP
       ! OPENMP does not launch multiple instances.
       t = t + 9999*omp_get_thread_num()
#endif
#ifdef MPI
       call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierr)
       t = t + 9999*myid
#endif
       do i = 1, n
          seed(i) = shr3(t)
       end do
    endif
    call random_kiss_seed(put=seed)
    contains
      ! This is sufficient for seeding a better RNG.
      function shr3(iseed) result(iran)
        implicit none
        integer(int64), intent(inout) :: iseed
        integer(int64) :: iran
        integer(int64) :: iseed0
        integer(int64) :: m,k,n
        m(k, n) = ieor(k, ishft(k, n))
        iseed0  = iseed
        iseed   = m(m(m(iseed, 13_int64), -17_int64), 43_int64)
        iran    = iseed0 + iseed
      end function shr3
  end subroutine init_random_kiss_seed

!-------------------------------------
  subroutine init_random_mt_seed(iseed)
#ifdef _OPENMP
    use omp_lib
#endif
#ifdef MPI
    use mpi
#endif
    implicit none
    integer, optional :: iseed
    ! local variable
    integer        :: i, n, un, istat, dt(8), pid
    !integer(int64) :: t
    integer        :: t
    integer        :: getpid
    logical        :: set_iseed
#ifdef MPI
    integer        :: myid, ierr
#endif

    set_iseed = .false.
    if (present(iseed)) then
       if (iseed /= 0) then
          set_iseed = .true.
          t         = iseed
        endif
    endif
    if (.not. set_iseed) then
       ! First try if the OS provides a random number generator
       ! return an integer uniform random deviate between [0 and huge(0)] from /dev/urandom if successful.
       open(newunit=un, file="/dev/urandom", access="stream", &
            form="unformatted", action="read", status="old", iostat=istat)
       if (istat == 0) then
          read(un) t
          close(un)
       else
          ! Fallback to XOR:ing the current time and pid. The PID is
          ! useful in case one launches multiple instances of the same
          ! program in parallel.
          call system_clock(t)
          if (t == 0) then
             call date_and_time(values=dt)
             t = (dt(1) - 1970) * 365_int64 * 24 * 60 * 60 * 1000 &
                  + dt(2) * 31_int64 * 24 * 60 * 60 * 1000 &
                  + dt(3) * 24_int64 * 60 * 60 * 1000 &
                  + dt(5) * 60 * 60 * 1000 &
                  + dt(6) * 60 * 1000 + dt(7) * 1000 &
                  + dt(8)
          end if
#ifdef _OPENMP
          ! OPENMP does not launches multiple instances.
          pid = getpid() + 9999*omp_get_thread_num()
#else
          pid = getpid()
#endif
          t   = ieor(t, int(pid, kind(t)))
       end if
    else
    ! bug-fixed, 2017-06-25. "else" part was missing. I don't know why this part was missing.
       t = iseed
#ifdef _OPENMP
       ! OPENMP does not launch multiple instances.
       t = t + 9999*omp_get_thread_num()
#endif
#ifdef MPI
       call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierr)
       t = t + 9999*myid
#endif
    endif
    call init_mt(t)
  end subroutine init_random_mt_seed
  !-------------------------------------
  !-------------------------------------
  !   Generate a random normal deviate using the polar method.
  !   Reference: Marsaglia,G. & Bray,T.A. 'A convenient method for generating
  !              normal variables', Siam Rev., vol.6, 260-264, 1964.
  !           or Numerical Recipes
  !-------------------------------------
  function rand_gauss1() result(gasdev)
    implicit none
    real(dp) :: gasdev
    real(dp) :: rsq,v1,v2
    real(dp), save :: gset
    logical,  save :: gaus_stored = .false.
    real(dp), parameter :: one = 1.0_dp
    !$OMP THREADPRIVATE(gset,gaus_stored)

    if (gaus_stored) then
       gasdev = gset
       gaus_stored = .false.
    else
       do
          v1  = 2.0_dp*rand_number() - 1.0_dp
          v2  = 2.0_dp*rand_number() - 1.0_dp
          rsq = v1*v1 + v2*v2
          if (rsq > 0.0 .and. rsq < one) exit
       enddo
       rsq    = sqrt(-2.0_dp*log(rsq)/rsq)
       gset   = v1*rsq
       gasdev = v2*rsq
       gaus_stored = .true.
    endif
  end function rand_gauss1
  subroutine random_gauss1(harvest)
    implicit none
    real(wp), intent(out) :: harvest
    real(wp) :: rsq,v1,v2
    real(wp), save :: g
    logical,  save :: gaus_stored=.false.
    !$OMP THREADPRIVATE(g,gaus_stored)
    if (gaus_stored) then
       harvest     = g
       gaus_stored = .false.
    else
       do
          v1  = 2.0_sp*rand_number()-1.0_sp
          v2  = 2.0_sp*rand_number()-1.0_sp
          rsq = v1**2+v2**2
          if (rsq > 0.0 .and. rsq < 1.0) exit
       end do
       rsq         = sqrt(-2.0_sp*log(rsq)/rsq)
       harvest     = v1*rsq
       g           = v2*rsq
       gaus_stored = .true.
    end if
  end subroutine random_gauss1
  subroutine random_gauss2(harvest)
    implicit none
    real(wp), dimension(:), intent(out) :: harvest
    real(wp), dimension(size(harvest))  :: rsq,v1,v2
    real(wp), allocatable, dimension(:), save :: g
    integer :: n,ng,nn,m
    integer, save :: last_allocated=0
    logical, save :: gaus_stored=.false.
    logical, dimension(size(harvest)) :: mask
    !$OMP THREADPRIVATE(g,last_allocated,gaus_stored)

    n = size(harvest)
    if (n /= last_allocated) then
       if (last_allocated /= 0) deallocate(g)
       allocate(g(n))
       last_allocated = n
       gaus_stored    = .false.
    end if
    if (gaus_stored) then
       harvest     = g
       gaus_stored = .false.
    else
       ng=1
       do
          if (ng > n) exit
          call random_number(v1(ng:n))
          call random_number(v2(ng:n))
          v1(ng:n)   = 2.0_sp*v1(ng:n)-1.0_sp
          v2(ng:n)   = 2.0_sp*v2(ng:n)-1.0_sp
          rsq(ng:n)  = v1(ng:n)**2+v2(ng:n)**2
          mask(ng:n) = (rsq(ng:n)>0.0 .and. rsq(ng:n)<1.0)
          call array_copy(pack(v1(ng:n),mask(ng:n)),v1(ng:),nn,m)
          v2(ng:ng+nn-1)  = pack(v2(ng:n),mask(ng:n))
          rsq(ng:ng+nn-1) = pack(rsq(ng:n),mask(ng:n))
          ng              = ng+nn
       end do
       rsq     = sqrt(-2.0_sp*log(rsq)/rsq)
       harvest = v1*rsq
       g       = v2*rsq
       gaus_stored = .true.
    end if
    contains
      subroutine array_copy(src,dest,n_copied,n_not_copied)
        real(WP), intent(in)  :: src(:)
        real(WP), intent(out) :: dest(:)
        integer,  intent(out) :: n_copied, n_not_copied

        n_copied         = min(size(src),size(dest))
        n_not_copied     = size(src)-n_copied
        dest(1:n_copied) = src(1:n_copied)
      end subroutine array_copy
  end subroutine random_gauss2
  subroutine random_gauss3(harvest)
    implicit none
    real(wp), intent(out) :: harvest(:,:)
    real(wp)              :: rand_g(size(harvest))
    integer :: n1,n2

    call random_gauss2(rand_g)
    n1 = size(harvest,1)
    n2 = size(harvest,2)
    harvest = reshape(rand_g,(/n1,n2/))
  end subroutine random_gauss3
  !-----------------------------------------------------
  ! random locations in the sphere with a radius r.
  !-----------------------------------------------------
  subroutine random_3Dsphere1(radius,x,y,z)
    implicit none
    real(kind=wp), intent(in)  :: radius
    real(kind=wp), intent(out) :: x,y,z

    ! local variables
    real(kind=wp) :: r,cost,sint,phi

    if (radius > 0.0_wp) then
      r    = radius*(rand_number())**(1.0_wp/3.0_wp)
      cost = 2.0_wp*rand_number()-1.0_wp
      sint = sqrt(1.0_wp-cost*cost)
      phi  = TWOPI*rand_number()
      x    = r*sint*cos(phi)
      y    = r*sint*sin(phi)
      z    = r*cost
    else
      x    = 0.0_wp
      y    = 0.0_wp
      z    = 0.0_wp
    endif
    return
  end subroutine random_3Dsphere1
  subroutine random_3Dsphere2(radius,vec)
    implicit none
    real(kind=wp), intent(in)  :: radius
    real(kind=wp), intent(out) :: vec(3)

    ! local variables
    real(kind=wp) :: r,cost,sint,phi

    if (radius > 0.0_wp) then
      r      = radius*(rand_number())**(1.0_wp/3.0_wp)
      cost   = 2.0_wp*rand_number()-1.0_wp
      sint   = sqrt(1.0_wp-cost*cost)
      phi    = TWOPI*rand_number()
      vec(1) = r*sint*cos(phi)
      vec(2) = r*sint*sin(phi)
      vec(3) = r*cost
    else
      vec(:) = 0.0_wp
    endif
    return
  end subroutine random_3Dsphere2
  !------------------------------
  ! Bactrian distribution
  !------------------------------
  function rand_bactrian() result(y)
  implicit none
  real(wp), parameter :: m   = 0.95_wp
  real(wp), parameter :: sig = sqrt(1.0_wp - m*m)
  real(wp), parameter :: cen = m
  real(wp) :: y

  if (rand_number() < 0.5_wp) then
     y = sig * rand_gauss() - cen
  else
     y = sig * rand_gauss() + cen
  endif
  end function rand_bactrian
  !------------------------------
  ! Trucated exponential distribution
  !------------------------------
  function rand_exp32(x0,xcut) result(expdev)
    implicit none
    real, intent(in) :: x0,xcut

    real :: expdev
    !local variables
    !real :: ra,rtemp
    !real, parameter :: eps = epsilon(0.0)

    !--- Truncated exponential
    ! the following algorithm gives inf number sometimes.
    !ra = rand_number()-0.5
    !rtemp = 2.0*ra*(1.0-exp(-xcut/x0))
    !if (abs(1.0-rtemp) > eps) then
    !if (rtemp /= 1.0) then
    !   expdev = -x0*sign(1.0,ra)*log(1.0-sign(1.0,ra)*rtemp)
    !else
    expdev = -x0 * log(rand_number())
    do while (expdev > xcut)
       expdev = -x0 * log(rand_number())
    enddo
    expdev = sign(1.0,real(rand_number())-0.5)*expdev
    !endif

    return
  end function rand_exp32
  function rand_exp64(x0,xcut) result(expdev)
    implicit none
    real(dp), intent(in) :: x0,xcut
    real(dp) :: expdev

    !local variables
    !real(dp) :: ra
    !--- Truncated exponential
    !ra = rand_number()-0.5_dp
    !expdev = -x0*sign(1.0_dp,ra)*log(1.0_dp-sign(1.0_dp,ra)*2.0_dp*ra*(1.0_dp-exp(-xcut/x0)))
    expdev = -x0 * log(rand_number())
    do while (expdev > xcut)
       expdev = -x0 * log(rand_number())
    enddo
    expdev = sign(1.0_dp,rand_number()-0.5_dp)*expdev

    return
  end function rand_exp64
  !--------------------
  ! random number generator for sech^2 distribution in z-direction.
  ! pdf = sech^2(z) when abs(z) < zmax
  !     = 0         otherwise
  function rand_sech2(zmax) result(r)
    implicit none
    real(dp), intent(in) :: zmax
    real(dp), save :: tanhzmax, zmax_save=-999.0_dp
    real(dp) :: r
    !$OMP THREADPRIVATE(tanhzmax,zmax_save)
    if (zmax_save /= zmax) then
       tanhzmax = tanh(zmax)
       zmax_save = zmax
    endif
    r = atanh(tanhzmax*(2.0_dp*rand_number()-1.0_dp))
    return
  end function rand_sech2
  !--------------------
  ! random number generator for exponential distribution in z-direction.
  ! pdf = exp(-abs(z)) when abs(z) < zmax
  !     = 0            otherwise
  function rand_zexp(zmax) result(r)
    implicit none
    real(dp), intent(in) :: zmax
    real(dp), save :: zmax_save=-999.0_dp, q_save
    real(dp) :: r
    !$OMP THREADPRIVATE(zmax_save,q_save)
    if (zmax_save /= zmax) then
       q_save = 1.0_dp - exp(-zmax)
       zmax_save = zmax
    endif
    r = rand_number()-0.5_dp
    r = -sign(1.0_dp,r)*log(1.0_dp-sign(1.0_dp,r)*2.0_dp*r*q_save)
    return
  end function rand_zexp
  !--------------------
  ! random number generator for exponential distribution in radial direction (for cylindrical coordinate system).
  ! p(r) dr = r x exp(-r) dr when r <= rmax
  !         = 0              otherwise
  ! This is faster than rand_rexp_baes (2018-01-23).
  function rand_rexp(rmax) result(rand)
    implicit none
    real(kind=wp), intent(in) :: rmax
    real(kind=wp) :: rand
    real(kind=wp) :: p, r1, r2

    p  = 1d0 - exp(-rmax)
    do while(.true.)
       r1 = -log(1d0-rand_number()*p)
       r2 = -log(1d0-rand_number()*p)
       rand = r1 + r2
       if (rand <= rmax) exit
    enddo
  end function rand_rexp
  !-----------------
  ! random number generator for exponential distribution in radial direction (for spherical coordinate system).
  ! p(r) dr = r^2 x exp(-r) dr when r <= rmax
  !         = 0                otherwise
  ! This is the fastest than many other algorithms (2018-01).
  function rand_r2exp(rmax) result(rand)
    implicit none
    real(kind=wp), intent(in) :: rmax
    real(kind=wp) :: rand
    real(kind=wp) :: p, r1, r2, r3

    p  = 1d0 - exp(-rmax)
    do while(.true.)
       r1 = -log(1d0-rand_number()*p)
       r2 = -log(1d0-rand_number()*p)
       r3 = -log(1d0-rand_number()*p)
       rand = r1 + r2 + r3
       if (rand <= rmax) exit
    enddo
  end function rand_r2exp
  !--------------------
  ! random number generator for exponential distribution in radial direction.
  ! p(r) dr = r x exp(-r) dr when r <= rmax
  !         = 0              otherwise
  ! see, Baes et al. (2003, MNRAS, 343, 1081)
  ! History:
  !      2015-12-07, bug-fixed
  function rand_rexp_baes(rmax) result(r)
    implicit none
    real(dp), intent(in) :: rmax
    integer, parameter   :: n=2001
    real(dp), save       :: rarr(n), parr(n), dparr, qscale
    real(dp), save       :: rmax_save = -999.9_dp
    real(dp), parameter  :: exp1 = exp(1.0_dp)
    integer  :: i
    real(dp) :: r, p, z
    !$OMP THREADPRIVATE(rarr,parr,dparr,qscale,rmax_save)
    if (rmax_save /= rmax) then
       dparr   = 1.0_dp/(n-1.0_dp)
       parr(:) = (/ ((i-1.0_dp)*dparr, i=1,n) /)
       qscale  = 1.0_dp - (1.0_dp + rmax)*exp(-rmax)
       do i=2,n-1
          z = (parr(i)*qscale-1.0_dp)/exp1
          rarr(i) = -1.0_dp - halley_iteration(z)
       enddo
       ! do not use halley iteraction for calculation at the boundaries.
       ! at the boundaries, it can give NaN. (2015-12-07)
       rarr(1)   = 0.0_dp
       rarr(n)   = rmax
       rmax_save = rmax
    endif

    p = rand_number()
    i = floor(p/dparr)+1
    if (i < 1) then
       r = rarr(1)
    else if (i >= n) then
       r = rarr(n)
    else if (i==1) then
       r = p/dparr * rarr(i+1)
    else
       !r = (p-parr(i))/dparr * (rarr(i+1)-rarr(i)) + rarr(i)
       r = log(rarr(i+1)/rarr(i))/log(parr(i+1)/parr(i)) * log(p/parr(i)) + log(rarr(i))
       r = exp(r)
    endif
  end function rand_rexp_baes
  !--------------------------
  ! Solver of the Lambert W function with Halley Interation Method.
  ! Input parameter range: -1/exp(1.0) < z < 0.0
  ! The Lambert W function is the inverse function of z = w x exp(w).
  !
  function halley_iteration(z) result(w)
    implicit none
    real(dp), intent(in) :: z
    real(dp), parameter :: huge_w=-huge(0.0_dp)
    real(dp) :: expw, wexpw, del, wplus1, w

    integer, parameter :: nc = 6
    real(dp), parameter :: exp1=exp(1.0_dp), exp2=-1.0_dp/exp1, eps_sp=epsilon(0.0_sp)
    real(dp), save :: c(nc) = (/ -221d0/8505d0, 769d0/17280d0, -43d0/540d0, &
                                      11d0/72d0, -1d0/3d0, 1d0 /)
    real(dp) :: p,L1,L2
    integer :: i
    !$OMP THREADPRIVATE(c)

    if (z == 0.0_dp) then
       w = huge_w
    else if (z == exp2) then
       w = -1.0_dp
    else if (z < -0.333_dp) then
       p = -sqrt(2.0_dp*(exp1*z+1.0_dp))
       w = 0.0_dp
       do i=1,nc
         w = (w + c(i))*p
       enddo
       w = w - 1.0_dp
    else if (z >= -0.333_dp .and. z <= -0.033_dp) then
       w = (-8.0960_dp + 391.0025*z)/(1.0_dp-82.9423_dp*z)
    else if (z >= -0.033_dp .and. z < 0.0_dp) then
       L1 = log(-z)
       L2 = log(-L1)
       w  = L1 - L2 + L2/L1 *(1.0_dp + (-2.0_dp+L2)/(2.0_dp*L1))
    endif

    wplus1 = w + 1.0_dp
    if (wplus1 /= 0.0_dp) then
       expw  = exp(w)
       wexpw = w * expw
       del   = wexpw - z
       do while(abs(del) >= eps_sp*abs(z))
          w = w - del/(expw * wplus1 - (wplus1 + 1.0_dp)*del/(2.0_dp*wplus1))
          wplus1 = w + 1.0_dp
          expw  = exp(w)
          wexpw = w * expw
          del   = wexpw - z
       enddo
    endif
  end function halley_iteration
  !-------
  ! Polar Generation of Random Variates with the t-distribution
  !    Bailey (1994, Mathematics of Computation, 62, 779)
  !    2016/02/06, Kwangil Seon, This algorithm is fater than the Kinderman & Monahan's ratio method.
  !-------
  function rand_t(dof) result(t)
  implicit none
  integer, intent(in) :: dof
  real(wp) :: t
  real(wp) :: u, v, w, r, c, u2

  do while(.true.)
     u  = 2.d0*rand_number() - 1.d0
     v  = 2.d0*rand_number() - 1.d0
     u2 = u*u
     w  = u2 + v*v
     !w  = u*u + v*v
     if (w <= 1d0) exit
  end do

  !c = u/sqrt(w)
  !r = sqrt(dof * (w**(-2d0/dof) - 1d0))
  !t = r*c
  c = u2/w
  r = dof * (w**(-2d0/dof) - 1d0)
  t = sqrt(r*c) * sign(1d0, u)
  end function

  subroutine random_t0(t, dof)
  implicit none
  real(wp), intent(out) :: t
  integer,  intent(in)  :: dof
  t = rand_t(dof)
  end subroutine

  subroutine random_t1(t, dof)
  implicit none
  real(wp), intent(out) :: t(:)
  integer,  intent(in)  :: dof
  integer :: n, i
  n = size(t)
  do i=1,n
     t(i) = rand_t(dof)
  enddo
  end subroutine

  !--------------------------------------------------------------
  ! Random Number for Scaled Inverse Chi-square Distribution
  ! 2016/02/16 Kwangil Seon
  function rand_scaled_inv_chi2(dof,var) result(r)
  implicit none
  integer,  intent(in) :: dof
  real(wp), intent(in) :: var
  real(wp) :: r
  r = (dof*var/2d0)/rand_gamma(dof/2d0)
  end function rand_scaled_inv_chi2
  !--------------------------------------------------------------
  !- Gamma Random Deviates for real alpha
  !- combination of two algorithms:
  !      (1) Numerical Recipes for integer alpha and
  !      (2) Martino & Luengo (2013, arXiv:1304.3800v3) for float alpha
  !- 2016/02/15 Kwangil Seon
  function gamdev_r(a) result(x)
  implicit none
  real(wp), intent(in) :: a
  real(wp) :: x
  real(wp) :: kp, bp, ratio
  integer  :: ap, i

  if (a < 1d0) then
     write(*,*) 'alpha should be larger than or equal to 1'
     stop
  endif

  ap = int(a)
  if (ap == a) then
     x = gamdev(ap)
  else
     if (a >= 1d0 .and. a < 2d0) then
        bp = 1d0/a
        kp = exp(a-1d0)*bp**(a-1d0)
     else
        bp = (ap-1d0)/(a-1d0)
        kp = exp(a-ap)*(1d0/(a-1d0))**(a-ap)
     endif

     do while(.true.)
        x     = gamdev(ap)
        ratio = kp * x**(a-ap)*exp(-(1d0-bp)*x)
        if (ratio >= rand_number()) exit
     enddo
  endif
  return
  end function gamdev_r

  !---------------------------------------------------------
  !- Gamma Random Deviates for integer alpha
  !- from Numerical Recipes
  !- 2016/02/15, Kwangil Seon
  !-      slightly modified to make it faster (ifort -fast)
  function gamdev(ia)
  implicit none
  integer,            intent(in) :: ia
  real(wp) :: gamdev
  real(wp) :: am,e,h,s,x,y,v(2),arr(5)
  integer :: i
  if (ia < 1) then
     write(*,*) 'ia should be larger than or equal to 1.'
     stop
  endif

  !if (ia < 6) then
    !call random_number(arr(1:ia))
    !x = -log(product(arr(1:ia)))
  if (ia <= 10) then
    x = 1d0
    do i=1,ia
       x = x*rand_number()
    enddo
    x  = -log(x)
  else
    do
      call random_number(v)
      v(2) = 2.0d0*v(2)-1.0d0
      if (dot_product(v,v) > 1.0d0) cycle
      y  = v(2)/v(1)
      am = ia-1
      s  = sqrt(2.0d0*am+1.0d0)
      x  = s*y+am
      if (x <= 0.0d0) cycle
      e = (1.0d0+y**2)*exp(am*log(x/am)-s*y)
      call random_number(h)
      if (h <= e) exit
    end do
  end if
  gamdev = x
  end function gamdev
  !==============================
  subroutine random_mvnorm(chole, x, dof_t, use_bactrian)
  ! generates an n multivariate random normal vector using a Cholesky decomposition.
  !    Dagpunar, J. (1988) Principles of random variate generation
  ! input:
  !    n     = number of variates in vector
  !    avg   = vector of means
  !    chole = Cholescky decomposition calculated with make_mvcovmat2
  ! output:
  !     x    = multivariate random normal vector
  ! 2016-02-20, Kwangil Seon
  implicit none
  real(wp), intent(in)  :: chole(:)
  real(wp), intent(out) :: x(:)
  integer,optional,intent(in) :: dof_t
  logical,optional,intent(in) :: use_bactrian
  logical :: bactrian
  real(wp), parameter :: m   = 0.95_wp
  real(wp), parameter :: sig = sqrt(1.0_wp - m*m), cen = m
  real(wp) :: y

  ! local variables
  integer  :: i, j, n, n2, mvt_dof

  n = size(x)
  if (n*(n+1)/2 /= size(chole)) then
     write(*,*) 'something wrong in random_mvnorm'
     stop
  endif

  if (present(use_bactrian)) then
     if (use_bactrian) bactrian = use_bactrian
  endif

  mvt_dof = -1
  if (present(dof_t)) then
     if (dof_t > 0) mvt_dof = dof_t
  endif

  n2   = 2*n
  x(:) = 0.0_wp
  do j = 1,n
    if (.not.bactrian) then
       if (mvt_dof > 0) then
          y = rand_t(mvt_dof)
       else
          y = rand_gauss()
       endif
    else
       if (rand_number() < 0.5_wp) then
          y = sig * rand_gauss() - cen
       else
          y = sig * rand_gauss() + cen
       endif
    endif

    do i = j, n
      x(i) = x(i) + chole((j-1)*(n2-j)/2 + i) * y
    enddo
  enddo
  end subroutine random_mvnorm

  subroutine make_mvcovmat2(x, avg, cov, chole)
  ! setup Cholesky decomposition matrix for a given set of input data x(1:n,1:np)
  ! input:
  !    n           = number of data sample
  !    np          = number of parameters
  !    x(1:n,1:np) = input data
  ! output:
  !    avg(1:np)            = vector of means
  !    chole(1:np*(np+1)/2) = lower triangular decomposition of variance matrix
  ! 2016-02-20, Kwangil Seon
  implicit none
  real(wp), intent(in)  :: x(:,:)
  real(wp), intent(out) :: avg(:), cov(:), chole(:)

  integer  :: n, np, ncov
  integer  :: i, j, k, m, n2
  real(wp) :: y, v

  n    = size(x,1)
  np   = size(x,2)
  ncov = np*(np+1)/2
  if (n == 1 .or. np /= size(avg) .or. ncov /= size(cov) .or. ncov /= size(chole)) then
     write(*,*) 'something wrong in make_mvcovmat2'
     stop
  endif

  ! Calculate the mean values
  do j=1, np
    avg(j) = sum(x(:,j))/real(n, kind=wp)
  enddo

  ! Calculate the upper triangle of the covariance matrix
  ! covar(i+j*(j-1)/2) = (i,j)th element (j >= i)
  do j=1,np
     do i=1,j
       k = i + j*(j-1)/2
       cov(k) = sum((x(:,i)-avg(i)) * (x(:,j)-avg(j))) / real(n-1, kind=wp)
     enddo
  enddo

  ! Calculate the lower triangle of the Cholesky decomposition of the covariance matrix
  ! chole((j-1)*(2*np-j)/2+i) = (i, j)th element of lower triangular decomposition of variance matrix (i >= j)
  chole(:) = 0.0d0
  n2       = 2*np
  chole(1) = sqrt(cov(1))
  y        = 1d0/chole(1)
  do j = 2,np
     chole(j) = cov(1+j*(j-1)/2) * y
  enddo

  do i = 2, np
     v = cov(i*(i-1)/2+i)
     do m = 1,i-1
        v = v - chole((m-1)*(n2-m)/2+i)**2
     enddo
     if (v < 0d0) return
     v = sqrt(v)
     y = 1d0/v
     chole((i-1)*(n2-i)/2+i) = v
     do j = i+1, np
        v = cov(j*(j-1)/2+i)
        do m = 1, i-1
           v = v - chole((m-1)*(n2-m)/2+i)*chole((m-1)*(n2-m)/2 + j)
        enddo
        chole((i-1)*(n2-i)/2 + j) = v*y
     enddo
  enddo
  end subroutine make_mvcovmat2

  subroutine make_mvcovmat(x,avg,cov,chole,update)
  ! setup Cholesky decomposition matrix for a given set of input data x(1:n,1:np)
  ! input:
  !    x(1:np) = input data
  ! output:
  !    avg(1:np)            = vector of means
  !    cov(1:np*(np+1)/2)   = upper triangular of covariance matrix
  !    chole(1:np*(np+1)/2) = lower triangular decomposition of variance matrix
  ! 2016-02-20, Kwangil Seon
  implicit none
  real(wp), intent(in)    :: x(:)
  real(wp), intent(inout) :: avg(:), cov(:), chole(:)
  logical,optional,intent(in) :: update

  integer  :: np, ncov
  integer  :: i, j, k, m, n2
  real(wp) :: y, v
  integer,  save :: ncall_covmat = 0, nupdate = 0
  real(wp), save, allocatable :: avg_old(:), cov_old(:)

  np   = size(x)
  ncov = np*(np+1)/2
  if (np /= size(avg) .or. ncov /= size(cov) .or. ncov /= size(chole)) then
     write(*,*) 'something wrong in make_mvcovmat2'
     stop
  endif

  if (present(update)) then
      if (update) nupdate = nupdate + 1
  endif

  if (nupdate == 1) then
     if (allocated(avg_old)) deallocate(avg_old)
     if (allocated(cov_old)) deallocate(cov_old)
     allocate(avg_old(np))
     allocate(cov_old(ncov))
     avg_old = avg
     cov_old = cov
     ncall_covmat = 1
  endif

  ncall_covmat = ncall_covmat + 1
  if (ncall_covmat == 1 .and. nupdate == 0) then
     if (allocated(avg_old)) deallocate(avg_old)
     if (allocated(cov_old)) deallocate(cov_old)
     allocate(avg_old(np))
     allocate(cov_old(ncov))
     avg = x
     cov = epsilon(0.0_wp)
  else
     ! Calculate the mean values
     avg(:) = (ncall_covmat-1.0_wp)/real(ncall_covmat,kind=wp) * avg_old(:) + x(:)/real(ncall_covmat,wp)

     ! Calculate the upper triangle of the covariance matrix
     ! covar(i+j*(j-1)/2) = (i,j)th element (j >= i)
     do j=1,np
        do i=1,j
          k = i + j*(j-1)/2
          cov(k) = (ncall_covmat-2.0_wp)/(ncall_covmat-1.0_wp)*cov_old(k) &
                   + x(i)*x(j)/(ncall_covmat-1.0_wp) &
                   - ncall_covmat/(ncall_covmat-1.0_wp) * avg(i)*avg(j) &
                   + avg_old(i)*avg_old(j)
          if (i == j) cov(k) = cov(k) + epsilon(0.0_wp)
        enddo
     enddo
  endif
  avg_old = avg
  cov_old = cov

  ! Calculate the lower triangle of the Cholesky decomposition of the covariance matrix
  ! chole((j-1)*(2*np-j)/2+i) = (i, j)th element of lower triangular decomposition of variance matrix (i >= j)
  chole(:) = 0.0d0
  n2       = 2*np
  chole(1) = sqrt(cov(1))
  y        = 1d0/chole(1)
  do j = 2,np
     chole(j) = cov(1+j*(j-1)/2) * y
  enddo

  do i = 2, np
     v = cov(i*(i-1)/2+i)
     do m = 1,i-1
        v = v - chole((m-1)*(n2-m)/2+i)**2
     enddo
     if (v < 0d0) return
     v = sqrt(v)
     y = 1d0/v
     chole((i-1)*(n2-i)/2+i) = v
     do j = i+1, np
        v = cov(j*(j-1)/2+i)
        do m = 1, i-1
           v = v - chole((m-1)*(n2-m)/2+i)*chole((m-1)*(n2-m)/2 + j)
        enddo
        chole((i-1)*(n2-i)/2 + j) = v*y
     enddo
  enddo
  end subroutine make_mvcovmat

!==============================
   subroutine random_sphere(point)
   ! Draw a random point within an n-dimensional unit sphere
   implicit none
   real(wp), intent(inout) :: point(:)
   integer :: n
   real(wp), allocatable :: z(:)

   n = size(point)
   if (allocated(z)) deallocate(z)
   allocate(z(n))
   call random_gauss(z)
   point(:) = z(:) * rand_number()**(1.0_wp/n) / sqrt(sum(z**2))
   deallocate(z)
   return
   end subroutine

!==============================
  function rand_stretch(a) result(gw10)
  ! random number generator for stretch move in GW10
  !  Goodman, J., & Weare, J. 2010, Commun. Appl. Math. Comput. Sci., 5, 65
  ! g(x) = 1/sqrt(x) if 1/a <= x < = a
  !      = 0         otherwise
  implicit none
  real(wp), intent(in) :: a
  real(wp) :: gw10
  real(wp) :: a2
  a2   = sqrt(1.0_wp/a)
  gw10 = (a2 + rand_number()*(sqrt(a)-a2))**2
  end function rand_stretch

!==============================
  function rand_traverse(a) result(cf10)
  ! random number generator for traverse move in CF10
  !  Christen, J. A., & Fox, C. 2010, Bayesian Analysis, 5, 263
  ! g(x) = (a-1)*(a+1)/2a * x^a     if x <= 1
  !      = (a+1)*(a-1)/2a * x^(-a)  if x >  1
  implicit none
  real(wp), intent(in) :: a
  real(wp) :: cf10
  real(wp) :: a2
  a2 = 1.0_wp/a
  if (rand_number() <= 0.5_wp*(1-a2)) then
     cf10 = rand_number()**(1.0_wp/(1.0_wp+a))
  else
     cf10 = rand_number()**(1.0_wp/(1.0_wp-a))
  endif
  end function rand_traverse

!==============================
! Note this is equivalent with rand_stretch if set a_stretch = 1 - a_walk.
!  function rand_walk(a) result(cf10)
!  ! random number generator for walk move in CF10
!  !  Christen, J. A., & Fox, C. 2010, Bayesian Analysis, 5, 263
!  implicit none
!  real(wp), intent(in) :: a
!  real(wp) :: cf10
!  real(wp) :: u
!  u    = rand_number()
!  cf10 = a/(1.0_wp+a) * (-1.0_wp + 2.0_wp * u + a * u*u)
!  end function rand_walk

!==============================
  function rand_permutation(n) result(perm)
  ! (unbiased) random permutation
  ! The "inside-out" shuffle version of the Fisher-Yates shuffle (also known as the Knuth shffle)
     implicit none
     integer, intent(in)   :: n
     integer, dimension(n) :: perm
     integer  :: i, j
     real(wp) :: u

     perm(1) = 1
     do i=2,n
        call random_number(u)
        j       = floor(i*u) + 1
        perm(i) = perm(j)
        perm(j) = i
     enddo
  end function rand_permutation

  function rand_pick(nele,npick) result(pick)
  !-- written by Jong-Ho Shinn (2017-...)
     implicit none
     integer, intent(in)   :: nele, npick
     integer, dimension(nele) :: perm
     integer, dimension(npick) :: pick
     integer  :: i, j, temp
     real(wp) :: u

     perm=(/(i,i=1,nele)/)
     do i=nele, nele-npick+1, -1
        call random_number(u)
        j       = floor(i*u) + 1
        temp    = perm(i)
        perm(i) = perm(j)
        perm(j) = temp
     enddo
     pick = perm(nele-npick+1:nele)
  end function rand_pick

  function rand_cyclic_permutation(n) result(perm)
  ! Author: Kwang-Il Seon (2016-01-01)
  !         The condition "perm(i) /= i" (i = 1,...,n) is imposed.
  !         This is in fact a cyclic permutation. (Sattolo's algorithm)
     implicit none
     integer, intent(in)   :: n
     integer, dimension(n) :: perm
     integer  :: i, j
     real(wp) :: u

     perm(1) = 2
     if (n >= 2) perm(2) = 1
     do i=3,n
        call random_number(u)
        j       = floor((i-1)*u) + 1
        perm(i) = perm(j)
        perm(j) = i
     enddo
  end function rand_cyclic_permutation
  !------------------------------
  function rand_binomial(pp,ntrial) result(bnldev)
    implicit none
    real(wp), intent(in) :: pp
    integer,  intent(in) :: ntrial
    integer              :: bnldev
    ! local variables
    integer        :: j
    integer, save  :: nold=-1
    real(wp)       :: am,em,g,h,p,sq,t,y,arr(24)
    real(wp), save :: pc,plog,pclog,en,oldg,pold=-1.0
    !$OMP THREADPRIVATE(nold,pc,plog,pclog,en,oldg,pold)

    p  = merge(pp, 1.0_wp-pp, pp <= 0.5_wp )
    am = ntrial*p
    if (ntrial < 25) then
       call random_number(arr(1:ntrial))
       bnldev = count(arr(1:ntrial) < p)
    else if (am < 1.0) then
       g = exp(-am)
       t = 1.0
       do j=0,ntrial
          call random_number(h)
          t = t*h
          if (t < g) exit
       end do
       bnldev = merge(j,ntrial, j <= ntrial)
    else
       if (ntrial /= nold) then
          en   = ntrial
          oldg = gammln(en+1.0_wp)
          nold = ntrial
       end if
       if (p /= pold) then
          pc    = 1.0_wp-p
          plog  = log(p)
          pclog = log(pc)
          pold  = p
       end if
       sq = sqrt(2.0_wp*am*pc)
       do
          call random_number(h)
          y  = tan(PI*h)
          em = sq*y+am
          if (em < 0.0 .or. em >= en+1.0_wp) cycle
          em = int(em)
          t  = 1.2_wp*sq*(1.0_wp+y**2)*exp(oldg-gammln(em+1.0_wp)-&
               gammln(en-em+1.0_wp)+em*plog+(en-em)*pclog)
          call random_number(h)
          if (h <= t) exit
       end do
       bnldev = em
    end if
    if (p /= pp) bnldev = ntrial-bnldev
    contains
     !------------------------------
     function gammln(xx)
       implicit none
       real(wp), intent(in) :: xx
       real(dp) :: gammln
       real(dp) :: tmp,x,ser,y
       real(dp) :: stp = 2.5066282746310005_dp
       real(dp), dimension(6) :: coef = (/76.18009172947146_dp,&
                                         -86.50532032941677_dp,&
                                          24.01409824083091_dp,&
                                          -1.231739572450155_dp,&
                                           0.1208650973866179e-2_dp,&
                                          -0.5395239384953e-5_dp/)
       integer :: i
       x   = xx
       tmp = x+5.5_dp
       tmp = (x+0.5_dp)*log(tmp)-tmp
       ser = 1.000000000190015_dp
       y   = x
       do i=1, size(coef)
          y   = y+1.0_dp
          ser = ser+coef(i)/y
       end do
       gammln = tmp + log(stp*ser/x)
    end function gammln
  end function rand_binomial
  !------------------------------
  function rand_multinomial(p,ntrial) result(ix)
  ! Generates a multinomial random deviate.
  !    P(1:NP)  : the probability that an event will be classified into category I=1,...,NP.
  !    NTRIAL   : number of trials. (0 <= NTRIAL)
  !    IX(1:NP) : a random observation from the multinomial distribution. IX(:) >= 0 and sum(IX) = NTRIAL.
  !
    implicit none
    integer,  intent(in) :: ntrial
    real(wp), intent(in) :: p(:)
    integer :: ix(size(p))

    ! local variables
    integer  :: i, ntot, np
    real(wp) :: prob, ptot
    !integer :: rand_binomial

    !  Initialize variables.
    np    = size(p)
    ntot  = ntrial
    ptot  = 1.0_wp
    ix(:) = 0

    ! Generate the observation.
    do i = 1, np - 1
      prob  = p(i) / ptot
      ix(i) = rand_binomial(prob,ntot)
      ntot  = ntot - ix(i)
      if (ntot <= 0) then
        return
      end if
      ptot = ptot - p(i)
    end do
    ix(np) = ntot

    return
  end function rand_multinomial
  !------------------------------
  function rand_index1(p) result(ix)
    implicit none
    real(wp), intent(in) :: p(:)
    integer :: ix
    integer :: i, np
    integer :: ind(size(p))

    np = size(p)
    if (np == 1) then
       ix = 1
    else
       ind = rand_multinomial(p,1)
       do i=1,np
          if (ind(i) == 1) then
             ix = i
             exit
          endif
       enddo
    endif
    return
  end function rand_index1
  function rand_indexn(p,ntrial) result(ix)
    implicit none
    integer,  intent(in) :: ntrial
    real(wp), intent(in) :: p(:)
    integer :: ix(ntrial)
    integer :: i,j,np
    integer :: ind(size(p))

    np = size(p)
    if (np == 1) then
       ix(:) = 1
    else
       ind = rand_multinomial(p,ntrial)
       j = 1
       do i=1,np
          if (ind(i) >= 1) then
             ix(j:j+ind(i)-1) = i
             j = j + ind(i)
          endif
       enddo
       ix = ix(rand_permutation(ntrial))
    endif
    return
  end function rand_indexn
  !------------------------------
  !------------------------------
  function rand_resonance_vz_zheng(x,a) result(rand_u)
  !
  ! Random Number Generation for Voigt Profile (Resonance Scattering) using rejection method.
  !   To obtain a random z-velocity component of an atom that scatters photon.
  !      See Zheng & Miralda-Escude (2002, ApJ, 578, 33) for basic algorithm
  !          Semelin et al. (2007, A&A, 474, 365) for optimal selection of u0
  !          Lausen, Razoumov, & Sommer-Larsen (2009, ApJ, 696, 853) for optimal selection of u0
  ! 11/04/2016 bug-fixed in calculation of u0 in Laursen algorithm
  !            added Semelin et al. algorithm, but commented out.
  ! 10/23/2016 Cosmetic change
  ! 09/27/2010 Initial version, Kwang-il Seon
  !
  implicit none
  real(kind=wp), intent(in) :: x, a
  real(kind=wp) :: rand_u
  real(kind=wp) :: u0,th0,th,p
  real(kind=wp) :: loga, xcw
  !real(kind=wp) :: log10a, xcw

  !-- delta function (11/26/2016)
  !if (a == 0.0_wp) then
  !   rand_u = x
  !   return
  !endif

  ! Laursen, Razoumov & Sommer-Larsen (2009, ApJ, 696, 853)
  if (abs(x) < 0.2_wp) then
    u0 = 0.0_wp
  else
    !Log10 is correct to calculate xcw as a function of a in Laursen et al. (2009).
    !  But, for the purpose of this routine, using natural logarithm is faster. (comment added on 2018-02-20)
    !log10a = log10(a)
    !xcw    = 1.59_wp - (0.60_wp + 0.03_wp*log10a)*log10a
    loga = log(a)
    xcw  = 1.59_wp - (0.60_wp + 0.03_wp*loga)*loga
    if (abs(x) < xcw) then
      u0 = abs(x) - 0.01_wp*a**(1.0_wp/6.0_wp)*exp(1.2_wp*abs(x))
    else
      u0 = 4.5_wp
    endif
  endif

  ! Semelin et al. (2007, A&A, 474, 365)
  ! This works well for 10^-4 < a < 10^-1.
  ! But, Laursen's argorithm is faster after bug-fix.
  !if (abs(x) <= 3.0_wp .or. abs(x) > 10.0_wp) then
  !  u0 = 0.0_wp
  !else
  !  u0 = 1.85_wp - log10(a)/6.73_wp + log(log(abs(x)))
  !endif

  ! Zheng & Miralda-Escude (2002, ApJ, 578, 33)
  if (x >= 0.0_wp) then
    u0  = abs(u0)
    th0 = atan((u0-x)/a)
    p   = (th0 + HALFPI)/((HALFPI + th0) + exp(-u0**2)*(HALFPI - th0))

    do while(.true.)
      if (rand_number() <= p) then
        th     = (th0 + HALFPI)*rand_number() - HALFPI
        rand_u = a * tan(th) + x
        if (rand_number() <= exp(-rand_u**2)) return
      else
        th     = (HALFPI - th0)*rand_number() + th0
        rand_u = a * tan(th) + x
        if (rand_number() <= exp(-rand_u**2 + u0**2)) return
      endif
    enddo
  else
    u0  = -abs(u0)
    th0 = atan((u0-x)/a)
    p   = exp(-u0**2)*(th0 + HALFPI)/((HALFPI - th0) + exp(-u0**2)*(HALFPI + th0))

    do while(.true.)
      if (rand_number() <= p) then
        th     = (th0 + HALFPI)*rand_number() - HALFPI
        rand_u = a * tan(th) + x
        if (rand_number() <= exp(-rand_u**2+u0**2)) return
      else
        th     = (HALFPI - th0)*rand_number() + th0
        rand_u = a * tan(th) + x
        if (rand_number() <= exp(-rand_u**2)) return
      endif
    enddo
  endif

  return
  !----------------------------------------------------------------
  end function rand_resonance_vz_zheng
   function rand_resonance_vz_seon(x0in,a) result(vz)
   !use, intrinsic :: iso_fortran_env, only: real64
   !use random
   implicit none
   !integer, parameter :: wp = real64
   real(kind=wp), intent(in) :: x0in, a
   real(kind=wp) :: x0,vz
   real(kind=wp) :: A0,A1,A2,Atot
   real(kind=wp) :: u0,u1,x1
   real(kind=wp), parameter :: PI = 3.141592653589793238462643383279502884197_wp
   real(kind=wp), parameter :: two_over_PI = 2.0_wp/PI
   real(kind=wp), parameter :: xc      = 1.0_wp + sqrt(2.0_wp)
   real(kind=wp), parameter :: x0_crit = 0.8_wp
   real(kind=wp) :: h0,h1,hmax
   real(kind=wp) :: x,t1,t2,del_t,u ,yy
   real(kind=wp) :: rr, r0, r1

   x0 = abs(x0in)
   if (x0 <= x0_crit) then
      do while(.true.)
         vz = x0 + a*tan(PI*(rand_number()-0.5_wp))
         if (rand_number() <= exp(-vz**2)) exit
      enddo
   else
      u0 = exp(-x0**2/2.0_wp)
      u1 = u0 + sqrt(two_over_PI*a*(1.0_wp - u0)*u0*x0)
      if (u1 > 0.0_wp) then
         h0 = 0.5_wp * u0
         !----- first area
         A0 = h0 * u0
         !----- second area
         x1 = sqrt(-2.0_wp*log(u1))
         A1 = (u1 - u0)*h0
         !----- third area
         !h1 = u1/PI * (atan((x1-x0)/a)+atan((x1+x0)/a))
         !h1 = u1/PI * atan(2.0_wp*a*x1/(a**2+(x0**2-x1**2)))
         h1 = a * two_over_PI * u1 *x1/(x0**2 - x1**2)
         if (x0 > xc) then
            hmax = a*0.3861_wp/(x0**2 - 1.373_wp)
            h1   = maxval([h1, hmax])
         endif
         A2   = (1.0_wp - u1)*h1
         Atot = A0 + A1 + A2
         r0   = A0/Atot
         r1   = (A0+A1)/Atot
         do while(.true.)
            rr = rand_number()
            if (rr <= r0) then
               !-- using two randome number is slightly faster than using sqrt.
               !u  = u0*sqrt(rand_number())
               u  = u0*(1d0-abs(rand_number()-rand_number()))
               yy = u  * rand_number()
            else if (rr <= r1) then
               u  = (u1-u0)*rand_number() + u0
               yy = h0 * rand_number()
            else
               u  = (1.0_wp-u1)*rand_number() + u1
               yy = h1 * rand_number()
            endif
            !if (rr > r1) then
            !   u  = (1.0_wp-u1)*rand_number() + u1
            !   yy = h1 * rand_number()
            !else if (rr > r0) then
            !   u  = (u1-u0)*rand_number() + u0
            !   yy = h0 * rand_number()
            !else
            !   !-- using two randome number is slightly faster than using sqrt.
            !   !u  = u0*sqrt(rand_number())
            !   u  = u0*(1.0_wp-abs(rand_number()-rand_number()))
            !   yy = u  * rand_number()
            !endif
            x  = sqrt(-2.0_wp*log(u))
            t2 = atan(( x-x0)/a)
            t1 = atan((-x-x0)/a)
            del_t = t2 - t1
            if (u /PI * del_t >= yy) exit
         enddo
         vz = x0 + a * tan(del_t * rand_number() + t1)
      else
         hmax = a*0.3861_wp/(x0**2 - 1.373_wp)
         do while(.true.)
            u  = rand_number()
            yy = hmax * rand_number()
            x  = sqrt(-2.0_wp*log(u))
            t2 = atan(( x-x0)/a)
            t1 = atan((-x-x0)/a)
            del_t = t2 - t1
            if (u /PI * del_t >= yy) exit
         enddo
         vz = x0 + a * tan(del_t * rand_number() + t1)
      endif
   endif
   if (x0in < 0.0_wp) vz = -vz
   end function rand_resonance_vz_seon
  !----------------------------------------------------------------
  ! random number generator of the scattering angle for resonance scattering.
  ! P(x) = (3/8)E1 x^2 + (4-E1)/8, {x,-1,1}
  ! 1-E1 is the contribution of isotropic scattering.
  ! -0.5 < E1 < 1 for Lyman alpha.
  ! This is the most general generator for the resonance scattering angle. (2017-09-05)
  function rand_resonance(E1) result(cost)
    implicit none
    real(kind=wp), intent(in) :: E1
    real(kind=wp) :: p2, Q, W
    real(kind=wp) :: cost
    real(kind=wp), parameter :: one_over_three = 1.0_wp/3.0_wp
    if (E1 > 0.0_wp) then
      p2   = sqrt((4.0_wp-E1)/(3.0_wp*E1))
      Q    = (4.0_wp * rand_number() - 2.0_wp)/(E1*p2**3)
      W    = (Q + sqrt(Q*Q+1.0_wp))**one_over_three
      cost = p2 * (W - 1.0_wp/W)
    else if (E1 < 0.0_wp) then
      p2   = sqrt(abs((4.0_wp-E1)/(3.0_wp*E1)))
      Q    = (4.0_wp * rand_number() - 2.0_wp)/(E1*p2**3)
      cost = 2.0_wp * p2 * cos((acos(Q)+fourpi)/3.0_wp)
    else
      cost = 2.0_wp * rand_number() - 1.0_wp
    endif
  return
  end function rand_resonance
  !----------------------------------------------------------------
  ! random number generator for the scattering angle measured from polarization vector.
  ! Scattering angle is defined as the angle between polarizqtion vector and new propagation vector,
  ! as done by Rybiki & Loeb (1998), in which rejection method is used. But, I am going to use inversion method.
  ! P(x) = -(3/4)E1 x^2 + (2+E1)/4, {x,-1,1}
  ! 1-E1 is the contribution of isotropic scattering.
  ! -0.5 < E1 < 1 for Lyman alpha.
  ! This is the most general generator for the resonance scattering angle. (2017-09-05)
  function rand_resonance_rybicki(E1) result(cost)
    implicit none
    real(kind=wp), intent(in) :: E1
    real(kind=wp) :: p2, Q, W
    real(kind=wp) :: cost
    if (E1 > 0.0_wp) then
      p2   = sqrt(abs((2.0_wp+E1)/(3.0_wp*E1)))
      Q    = -(2.0_wp * rand_number() - 1.0_wp)/(E1*p2**3)
      cost = 2.0_wp * p2 * cos((acos(Q)+fourpi)/3.0_wp)
    else if (E1 < 0.0_wp .and. E1 > -2.0_wp) then
      p2   = sqrt(abs((2.0_wp+E1)/(3.0_wp*E1)))
      Q    = -(2.0_wp * rand_number() - 1.0_wp)/(E1*p2**3)
      W    = (Q + sqrt(Q*Q+1.0_wp))**(1.0_wp/3.0_wp)
      cost = p2 * (W - 1.0_wp/W)
    else
      cost = 2.0_wp * rand_number() - 1.0_wp
    endif
    return
  end function rand_resonance_rybicki
  !------------------------------------------------------
  function rand_henyey_greenstein(g) result(hgdev)
  !
  ! Randoum Number Generator for Henyey-Greenstein function
  ! g  = asymmetry phase factor
  !
    implicit none
    real(kind=wp), intent(in) :: g
    real(kind=wp) :: hgdev
    real(kind=wp) :: x,g2,twog

    x = rand_number()
    if (g == 0.0_wp) then
       hgdev = 2.0_wp*x-1.0_wp
    else
       g2    = g*g
       twog  = 2.0_wp * g
       !hgdev = ((1.0_wp+g2)-((1.0_wp-g2)/(1.0_wp-g+2.0_wp*g*x))**2)/(2.0_wp*g)
       hgdev = ((1.0_wp+g2)-((1.0_wp-g2)/(1.0_wp-g+twog*x))**2)/twog
    endif
    return
  end function rand_henyey_greenstein
  !------------------------------------------------------
  function rand_rayleigh() result(rand_r)
  !
  ! Random Number Generation for Rayleigh (Dipole) Scattering Phase Function
  ! Output : cos(theta)
  ! 09/18/2010 Seon (2006, PASJ, 58, 439)
  !    This seems to be the fastest method.
  ! 10/23/2016 Cosmetic change
  ! 09/05/2017 More general routine is given by rand_resonance.
  implicit none
  real(kind=wp) :: rand_r
  real(kind=wp) :: x, y
  x      = 2.0_wp*(2.0_wp*rand_number() - 1.0_wp)
  y      = (x + sqrt(x**2 + 1.0_wp))**(1.0_wp/3.0_wp)
  rand_r = y - 1.0_wp/y
  return
  end function rand_rayleigh
  !------------------------------------------------------
  !function rand_cauchy(x,a) result(rand_c)
  function rand_cauchy() result(rand_c)
  !
  ! Random Number Generation for Cauchy-Lorentz Distribution
  ! 09/20/2010 Kwang-il Seon
  !
  implicit none
  real(kind=wp)  :: rand_c
  !real(kind=wp), intent(in) :: x,a
  !rand_c = a * tan(PI*rand_number()-HALFPI) + x
  rand_c = tan(PI*rand_number()-HALFPI)
  return
  end function rand_cauchy
  !------------------------------------------------------
  function rand_voigt(a) result(rand_v)
  ! random number generator for voigt function, which is a convolution of Cauchy-Lorentz and Gaussian.
  ! 2017-09-05, KI Seon
  implicit none
  real(kind=wp), intent(in) :: a
  real(kind=wp) :: rand_v
  real(kind=wp), parameter :: one_over_sqrt2 = 1.0_wp/sqrt(2.0_wp)
  rand_v = a * rand_cauchy() + rand_gauss() * one_over_sqrt2
  end function rand_voigt

  !------------------------------
  function rand_planck_num32(T,wavelength) result(energy)
  ! Random energy sampled from a number density function for blackbody photons with temperature T
  ! The algorithm is taken from Barnett and Canfield.
  ! Wavelength of photon is returned in um, if optional wavelength = .true. is given.
  implicit none

  real(kind=4), intent(in) :: T
  logical, intent(in), optional :: wavelength
  real(kind=4) :: energy

  ! local variables
  ! Riemann Zeta function zeta(3) and Boltzmann Constant
  real(kind=4), parameter :: zeta3 = 1.202056903159594285399738161511449990764986292_8
  real(kind=4), parameter :: kB = 1.3806488e-16_8 ! erg/K
  real(kind=8), parameter :: hc = 6.6260755e-27_8 * 2.99792458e14_8
  real(kind=4) :: r1,r2,r3,r4,a,s
  integer :: m

  r1 = zeta3 * rand_number()
  r2 = rand_number()
  r3 = rand_number()
  r4 = rand_number()

  m = 1
  a = 1.0
  s = 1.0
  do
    if (r1 < s) exit
    m = m + 1
    a = 1.0/m
    s = s + a*a*a
  enddo
  energy = -log(r2*r3*r4)*a
  ! Convert to Energy
  energy = kB*T * energy

  if (present(wavelength)) then
     if (wavelength) then
        energy = hc/energy
     endif
  endif
  end function rand_planck_num32
  !------------------------------
  function rand_planck_num64(T,wavelength) result(energy)
  ! Random energy sampled from a number density function for blackbody photons with temperature T
  ! The algorithm is taken from Barnett and Canfield.
  ! Wavelength of photon is returned in um, if optional wavelength = .true. is given.
  implicit none

  real(kind=8), intent(in) :: T
  logical, intent(in), optional :: wavelength
  real(kind=8) :: energy

  ! local variables
  ! Riemann Zeta function zeta(3) and Boltzmann Constant
  real(kind=8), parameter :: zeta3 = 1.202056903159594285399738161511449990764986292_8
  real(kind=8), parameter :: kB = 1.3806488e-16_8 ! erg/K
  real(kind=8), parameter :: hc = 6.6260755e-27_8 * 2.99792458e14_8
  real(kind=8) :: r1,r2,r3,r4,a,s
  integer :: m

  r1 = zeta3 * rand_number()
  r2 = rand_number()
  r3 = rand_number()
  r4 = rand_number()

  m = 1
  a = 1.0
  s = 1.0
  do
    if (r1 < s) exit
    m = m + 1
    a = 1.0/m
    s = s + a*a*a
  enddo
  energy = -log(r2*r3*r4)*a
  ! Convert to Energy
  energy = kB*T * energy

  if (present(wavelength)) then
     if (wavelength) then
        energy = hc/energy
     endif
  endif
  end function rand_planck_num64
  !------------------------------
  function rand_planck32(T,wavelength) result(energy)
  ! Random energy sampled from a number density function for blackbody photons with temperature T
  ! The algorithm is taken from Barnett and Canfield.
  ! Wavelength of photon is returned in um, if optional wavelength = .true. is given.
  implicit none

  real(kind=4), intent(in) :: T
  logical, intent(in), optional :: wavelength
  real(kind=4) :: energy

  ! local variables
  ! Riemann Zeta function zeta(3) and Boltzmann Constant
  real(kind=4), parameter :: zeta4 = 1.0823232_8
  real(kind=4), parameter :: kB = 1.3806488e-16_8 ! erg/K
  real(kind=8), parameter :: hc = 6.6260755e-27_8 * 2.99792458e14_8
  real(kind=4) :: r1,r2,r3,r4,r5,a,s
  integer :: m

  r1 = zeta4 * rand_number()
  r2 = rand_number()
  r3 = rand_number()
  r4 = rand_number()
  r5 = rand_number()

  m = 1
  a = 1.0
  s = 1.0
  do
    if (r1 < s) exit
    m = m + 1
    a = 1.0/m
    s = s + a*a*a*a
  enddo
  energy = -log(r2*r3*r4*r5)*a
  ! Convert to Energy
  energy = kB*T * energy

  if (present(wavelength)) then
     if (wavelength) then
        energy = hc/energy
     endif
  endif
  end function rand_planck32
  !------------------------------
  function rand_planck64(T,wavelength) result(energy)
  ! Random energy sampled from a number density function for blackbody photons with temperature T
  ! The algorithm is taken from Barnett and Canfield.
  ! Wavelength of photon is returned in um, if optional wavelength = .true. is given.
  implicit none

  real(kind=8), intent(in) :: T
  logical, intent(in), optional :: wavelength
  real(kind=8) :: energy

  ! local variables
  ! Riemann Zeta function zeta(3) and Boltzmann Constant
  real(kind=8), parameter :: zeta4 = 1.0823232_8
  real(kind=8), parameter :: kB = 1.3806488e-16_8 ! erg/K
  real(kind=8), parameter :: hc = 6.6260755e-27_8 * 2.99792458e14_8
  real(kind=8) :: r1,r2,r3,r4,r5,a,s
  integer :: m

  r1 = zeta4 * rand_number()
  r2 = rand_number()
  r3 = rand_number()
  r4 = rand_number()
  r5 = rand_number()

  m = 1
  a = 1.0
  s = 1.0
  do
    if (r1 < s) exit
    m = m + 1
    a = 1.0/m
    s = s + a*a*a*a
  enddo
  energy = -log(r2*r3*r4*r5)*a
  ! Convert to Energy
  energy = kB*T * energy

  if (present(wavelength)) then
     if (wavelength) then
        energy = hc/energy
     endif
  endif
  end function rand_planck64
  !------------------------------
end module random
