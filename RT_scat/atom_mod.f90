module atom_mod                                                                          
use cons
implicit none
public

public set_atoms

contains

subroutine set_atoms()
use cons
!type(atom_type) :: K_I, Mg_II, C_IV, O_VI, N_V

	H_I%gamma_H = 6.2649d+8
	H_I%gamma_K = 6.2648d+8
	H_I%f12_H   = 0.13881
	H_I%f12_K   = 0.2776
	H_I%f12     = H_I%f12_H + H_I%f12_K 
	H_I%wlH     = 1215.673644608d-8
	H_I%wlK     = 1215.668237310d-8
	H_I%nuH     = c/H_I%wlH
	H_I%nuK     = c/H_I%wlK
	H_I%nu_0    = (H_I%nuH + 2.d0*H_I%nuK)/3.d0
	H_I%wlc_0   = c/H_I%nu_0*1d8
	H_I%sigma_0 = H_I%f12*sqrt(pi)*e**2/(m_e*c) 
	H_I%mass    = 1.00784*m_u

	Mg_II%gamma_H = 2.57d+8 
	Mg_II%gamma_K = 2.60d+8 
	Mg_II%f12_H   = 0.303d0 
	Mg_II%f12_K   = 0.608d0 
	Mg_II%f12     = Mg_II%f12_H + Mg_II%f12_K 
	Mg_II%wlH     = 2802.705d-8
	Mg_II%wlK     = 2795.528d-8 
	Mg_II%nuH     = c/Mg_II%wlH
	Mg_II%nuK     = c/Mg_II%wlK
	Mg_II%nu_0    = (Mg_II%f12_H*Mg_II%nuH + Mg_II%f12_K*Mg_II%nuK)/Mg_II%f12
	Mg_II%wlc_0   = c/Mg_II%nu_0*1d8
	Mg_II%sigma_0 = Mg_II%f12*sqrt(pi)*e**2/(m_e*c) 
	Mg_II%mass    = 24.305d0*m_u - m_e

	C_IV%gamma_H = 2.64d+8 
	C_IV%gamma_K = 2.65d+8 
	C_IV%f12_H   = 0.0952d0
	C_IV%f12_K   = 0.190d0
	C_IV%f12     = C_IV%f12_H + C_IV%f12_K 
	C_IV%wlH     = 1550.772d-8 
	C_IV%wlK     = 1548.187d-8 
	C_IV%nuH     = c/C_IV%wlH
	C_IV%nuK     = c/C_IV%wlK
	C_IV%nu_0    = (C_IV%f12_H*C_IV%nuH + C_IV%f12_K*C_IV%nuK)/C_IV%f12
	C_IV%wlc_0   = c/C_IV%nu_0*1d8
	C_IV%sigma_0 = C_IV%f12*sqrt(pi)*e**2/(m_e*c) 
	C_IV%mass    = 12.0107d0*m_u - 3.d0*m_e

	O_VI%gamma_H = 4.09d+8 
	O_VI%gamma_K = 4.16d+8 
	O_VI%f12_H   = 0.0660d0
	O_VI%f12_K   = 0.133d0
	O_VI%f12     = O_VI%f12_H + O_VI%f12_K 
	O_VI%wlH     = 1037.613d-8
	O_VI%wlK     = 1031.912d-8
	O_VI%nuH     = c/O_VI%wlH
	O_VI%nuK     = c/O_VI%wlK
	O_VI%nu_0    = (O_VI%f12_H*O_VI%nuH + O_VI%f12_K*O_VI%nuK)/O_VI%f12
	O_VI%wlc_0   = c/O_VI%nu_0*1d8
	O_VI%sigma_0 = O_VI%f12*sqrt(pi)*e**2/(m_e*c) 
	O_VI%mass    = 15.999d0*m_u - 5.d0*m_e

	N_V%gamma_H = 3.356d+8 
	N_V%gamma_K = 3.391d+8 
	N_V%f12_H   = 0.07823d0
	N_V%f12_K   = 0.1570d0
	N_V%f12     = N_V%f12_H + N_V%f12_K 
	N_V%wlH     = 1242.804d-8
	N_V%wlK     = 1238.821d-8
	N_V%nuH     = c/N_V%wlH
	N_V%nuK     = c/N_V%wlK
	N_V%nu_0    = (N_V%f12_H*N_V%nuH + N_V%f12_K*N_V%nuK)/N_V%f12
	N_V%wlc_0   = c/N_V%nu_0*1d8
	N_V%sigma_0 = N_V%f12*sqrt(pi)*e**2/(m_e*c) 
	N_V%mass    = 14.0067d0*m_u - 4.d0*m_e

end subroutine set_atoms


end module atom_mod
