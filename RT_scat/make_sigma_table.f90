program make_sigma_table
    use cons
    use atom_mod
    use voigt_mod
    implicit none

    integer :: i, n
    real(kind=rkd) :: T, v_th
    real(kind=rkd) :: dnu_th_K, dnu_th_H
    real(kind=rkd) :: a_K, a_H
    real(kind=rkd) :: nu_min, nu_max, dnu, nu
    real(kind=rkd) :: xnuK, xnuH
    real(kind=rkd) :: sigmaK, sigmaH, sigma_atom_test
    type(atom_type) :: atm

    call set_atoms()
    atm = C_IV

    T = 1.0d5
    v_th = sqrt(2.d0*k*T/atm%mass)
    !v_th = 30d5
    
    dnu_th_K = v_th/c * atm%nuK
    dnu_th_H = v_th/c * atm%nuH

    a_K = atm%gamma_K / (4.d0*pi*dnu_th_K)
    a_H = atm%gamma_H / (4.d0*pi*dnu_th_H)

    n = 1000
    nu_min = atm%nuH - 5.d0*dnu_th_H
    nu_max = atm%nuK + 5.d0*dnu_th_K
    dnu = (nu_max - nu_min)/real(n-1, kind=rkd)

    open(unit=10, file='sigma_table.txt', status='replace', action='write', form='formatted')
    write(10,'(A)') 'nu sigma_atom_test'

    do i = 1, n
        nu = nu_min + real(i-1, kind=rkd)*dnu

        xnuK = (nu - atm%nuK)/dnu_th_K
        xnuH = (nu - atm%nuH)/dnu_th_H

        sigmaK = voigt(xnuK, a_K)/dnu_th_K
        sigmaH = voigt(xnuH, a_H)/dnu_th_H

        sigma_atom_test = atm%sigma_0 * &
             (atm%f12_K*sigmaK + atm%f12_H*sigmaH) / atm%f12

        write(10,'(ES24.16,1X,ES24.16)') nu, sigma_atom_test
    end do

    close(10)
end program make_sigma_table