import numpy as np                                                                        
import matplotlib.pyplot as plt
import pyCloudy as pc
import pyneb as pn
from astropy    import constants as const
from astropy.io import ascii
import pandas as pd
from scipy import interpolate
import warnings
from scipy.integrate import quad, IntegrationWarning
import scipy.integrate as integrate
from numpy import log10, exp
import os
from astropy.constants import h, c
import astropy.units as u
from scipy import special
import matplotlib.gridspec as gridspec

warnings.filterwarnings('ignore', category=IntegrationWarning)

kpc = const.kpc.cgs.value
h_ev = h.to(u.eV *u.s).value
c_cms = c.to(u.cm/u.s).value
c_kms = c.to(u.km/u.s).value
Ly_a_K = 1215.673644609e-8
Ly_a_H = 1215.668237310e-8
C_IV_K = 1548.187e-8
C_IV_H = 1550.772e-8


def calculate_order_and_value(value):
    """ 주어진 값에 대해 변환된 값과 해당 order 반환 """

    if value == 0 :
        return "000" , 0
    elif value == 1:
        return int(value * 100), 0
    elif value < 100:
        return int(value * 10), 1
    elif value < 1000:
        return int(value), 2

    return int(value / 10), 3


def RT_path(z_red_Shift,v_out, v_emit, v_rand, Geometry, atom, Lumin,idx,metals,Column_density_order):

    if v_out == 0:
        expand, vout_order = "000", 0
    elif v_out >= 1000:
        expand, vout_order = int(v_out/10), 3
    else:
        expand, vout_order = v_out, 2
    # print(v_emit)
    emit, emit_order = calculate_order_and_value(v_emit)    
    # print(emit,emit_order)
    rand, rand_order = calculate_order_and_value(v_rand)
    # print(rand)
    ll = 0
    # 파일 경로 설정

    lum = int(Lumin * 10)

    if Geometry.upper() == 'NEBULA' :
        geo  = 2 
    elif Geometry.upper() == 'QSO' :
        geo = 3
    elif Geometry.upper() == 'Continuum' :
        geo = 4
    else : geo =1 # Test

    
    if idx == 1 :
        mode='W'
    else : mode = 'WO'

    col = int(Column_density_order*10)

    if metals < 1:
        metals_int = int(metals * 1000)   # 0.001 → 1, 0.01 → 10 등
        metals_str = f"{metals_int:04d}"  # 1 → '0001', 10 → '0010'
    else:
        metals_str = str(int(metals))     # 1.0 → '1', 2.0 → '2'

        
    folder_name = f'{mode}{atom}L{lum}M{metals_str}NH{col}'
    folder_path = f'/home/jin/RT/RT_scat/{folder_name}'

    if geo == 1 :
            path_rt = (f'/home/jin/RT/RT_scat/{mode}{atom}L{lum}M{metals_str}NH{col}/N_atom{geo}00E+10_'
                    f'Vexp000E+00_Vemit100E+00_'
                    f'tauD000E+00_Vran000E+00radi.dat')      
    else :
        path_rt = (f'/home/jin/RT/RT_scat/{mode}{atom}L{lum}M{metals_str}NH{col}/N_atom{geo}00E+10_'
                    f'Vexp{expand}E+0{vout_order}_Vemit{emit}E+0{emit_order}_'
                    f'tauD000E+00_Vran{rand}E+0{rand_order}radi.dat')      

    return path_rt





def RT_SB(path):
    """RT 산출물에서 Surface Brightness 데이터를 읽어오는 함수"""
    if not os.path.exists(path):
        print(f"Warning: RT file not found: {path}")
        return None
    
    try:
        name = ['radius','SB_K','SB_H','SB_tot','1','2','3']
        data_sp = pd.read_csv(path, sep='\s+', header=None,names=name)
        rad, SB_t, SB_k,SB_h =  data_sp['radius'].to_numpy(),data_sp['SB_tot'].to_numpy(),data_sp['SB_K'].to_numpy(),data_sp['SB_H'].to_numpy()
        return rad*100, rad*100*kpc, SB_t
    except Exception as e:
        print(f"Error reading RT file {path}: {e}")
        return None 




def RT_make_parameter(z_red_Shift,v_out, v_emit, v_rand, Geometry, atom, Lumin,idx,metals,Column_density_order):
    """RT 시뮬레이션 파라미터를 생성하고 데이터를 읽어오는 함수"""
    # 기본값 설정
    radius_RT, radius_kpc_RT, SB_RT = np.array([0]), np.array([0]), np.array([0])
    normal_RT = 1  # ZeroDivisionError 방지

    try:
        if v_out == 0:
            expand, vout_order = "000", 0
        elif v_out >= 1000:
            expand, vout_order = int(v_out/10), 3
        else:
            expand, vout_order = v_out, 2
            
        emit, emit_order = calculate_order_and_value(v_emit)    
        rand, rand_order = calculate_order_and_value(v_rand)

        path_rt = RT_path(z_red_Shift,v_out, v_emit, v_rand, Geometry, atom, Lumin,idx,metals,Column_density_order)     

        # RT_SB 함수 호출
        red_shift_factor = (1 + z_red_Shift)**4
        rt_output = RT_SB(path_rt)
        if rt_output is not None:
            radius_RT, radius_kpc_RT, SB_RT = rt_output

        # SB_RT 값이 None이거나 비어있을 경우 기본값 설정
        if SB_RT is None or len(SB_RT) == 0:
            SB_RT = np.array([0])
        normal_RT = np.sum(SB_RT) if np.sum(SB_RT) != 0 else 1

        # print(f"make parameters of v_rand = {v_rand} km/s, v_exp = {v_out} km/s, v_emit = {v_emit} km/s for {atom}")

        return radius_RT, radius_kpc_RT, SB_RT/normal_RT, SB_RT  / red_shift_factor  
        
    except Exception as e:
        print(f"Error in RT_make_parameter: {e}")
        return np.array([0]), np.array([0]), np.array([0]), np.array([0])   


# def vout_make_parameter(z_red_Shift,lum,v_out, v_emit ,v_rand,mode,Geometry):
#     for ii in v_out:
#         nn = int(ii / 100)
#         globals()['RT_radius_vout_{}'.format(nn)] , globals()['RT_radius_kpc_vout_{}'.format(nn)], globals()['Norm_RT_SB_vout_{}'.format(nn)] , globals()['Origin_SB_vout_{}'.format(nn)]= RT_make_parameter(z_red_Shift,lum,ii, v_emit, v_rand,'CIV',mode,Geometry)[:4]
#     return print("Make data!!")

def photon_number_SB(radius, origin_SB, CIV_lum):
    """광자 수 기반 Surface Brightness 계산 함수"""
    try:
        if len(radius) < 2 or len(origin_SB) != len(radius):
            print("Error: Invalid input arrays for photon_number_SB")
            return np.array([0]), np.array([0])
            
        R_rt = radius / 100 
        dR = R_rt[1] - R_rt[0]
        number_dis = np.zeros(len(R_rt))
        surface_brightness_RT = np.zeros(len(R_rt))
        
        # 첫 번째 루프: number_dis 계산
        for ii, R in enumerate(R_rt):
            if R == 0:
                area = np.pi * (0.5*dR)**2
            elif R == R_rt[-1]:
                area = np.pi * (2*R + 0.5*dR) * 0.5*dR 
            else:
                area = 2*np.pi*R*dR
            number_dis[ii] = origin_SB[ii] * area 
            
        total_Number = np.sum(number_dis)
        if total_Number == 0:
            print("Warning: Total number is zero")
            return number_dis, surface_brightness_RT
            
        factor_atom = CIV_lum / total_Number 
        
        # 두 번째 루프: surface_brightness_RT 계산
        for ii, R in enumerate(R_rt):
            if R == 0:
                area = np.pi * (0.5*dR)**2
            elif R == R_rt[-1]:
                area = np.pi * (2*R + 0.5*dR) * 0.5*dR 
            else:
                area = 2*np.pi*R*dR
            factor_area = (100*kpc)**2
            surface_brightness_RT[ii] = number_dis[ii] * factor_atom / (area*factor_area) 

        return number_dis, surface_brightness_RT
        
    except Exception as e:
        print(f"Error in photon_number_SB: {e}")
        return np.array([0]), np.array([0])


def K_H_from_Combine_data_com(Line , atom_num , atom_index, vout, vemit, vran):
    out, out_order = calculate_order_and_value(vout)
    emit, emit_order = calculate_order_and_value(vemit)
    ran , ran_order = calculate_order_and_value(vran)

    path = r'/home/jin/data_CIV/N_atom{}0E+{}_Vexp{}E+0{}_Vemit{}E+0{}_tauD000E+00_Vran{}E+0{}spec_com.dat'.format(atom_num, atom_index , out, out_order, emit, emit_order, ran, ran_order)
    try:
        data = pd.read_csv(path, sep='\s+', header=None)
    except:
        print('파일을 찾을 수 없습니다.',path)


    lam = data[0].to_numpy()
    spec_tot = data[1].to_numpy()
    spec_sc = data[2].to_numpy()
   # spec_pol_tot = data[3]
   # spec_pol_scat = data[4]
    return lam , spec_tot , spec_sc



def K_H_from_Combine_data_com(z_red_Shift,v_out, v_emit, v_rand, Geometry, atom, Lumin,idx,metals,Column_density_order):

    if v_out == 0:
        expand, vout_order = "000", 0
    elif v_out >= 1000:
        expand, vout_order = int(v_out/10), 3
    else:
        expand, vout_order = v_out, 2
    # print(v_emit)
    emit, emit_order = calculate_order_and_value(v_emit)    
    # print(emit,emit_order)
    rand, rand_order = calculate_order_and_value(v_rand)
    # print(rand)
    ll = 0
    # 파일 경로 설정

    lum = int(Lumin * 10)

    if Geometry.upper() == 'NEBULA' :
        geo  = 2 
    elif Geometry.upper() == 'QSO' :
        geo = 3
    else : geo =1 # Test

    
    if idx == 1 :
        mode='W'
    else : mode = 'WO'

    col = int(Column_density_order*10)

    if metals < 1:
        metals_int = int(metals * 1000)   # 0.001 → 1, 0.01 → 10 등
        metals_str = f"{metals_int:04d}"  # 1 → '0001', 10 → '0010'
    else:
        metals_str = str(int(metals))     # 1.0 → '1', 2.0 → '2'

        
    folder_name = f'{mode}{atom}L{lum}M{metals_str}NH{col}'
    folder_path = f'/home/jin/RT/RT_scat/{folder_name}'

    if geo == 1 :
            path_rt = (f'/home/jin/RT/RT_scat/{mode}{atom}L{lum}M{metals_str}NH{col}/N_atom{geo}00E+10_'
                    f'Vexp000E+00_Vemit100E+00_'
                    f'tauD000E+00_Vran000E+00spec_com.dat')      
    else :
        path_rt = (f'/home/jin/RT/RT_scat/{mode}{atom}L{lum}M{metals_str}NH{col}/N_atom{geo}00E+10_'
                    f'Vexp{expand}E+0{vout_order}_Vemit{emit}E+0{emit_order}_'
                    f'tauD000E+00_Vran{rand}E+0{rand_order}spec_com.dat')      

    try:
        data = pd.read_csv(path_rt , sep='\s+', header=None)
    except:
        print('파일을 찾을 수 없습니다.',path_rt)

    lam = data[0].to_numpy()
    spec_tot = data[1].to_numpy()
    spec_sc = data[2].to_numpy()
   # spec_pol_tot = data[3]
   # spec_pol_scat = data[4]
    return lam , spec_tot , spec_sc






def RT_path_f_esc(z_red_Shift,v_out, v_emit, v_rand, Geometry, atom, Lumin,idx,metals,Column_density_order):

    if v_out == 0:
        expand, vout_order = "000", 0
    elif v_out >= 1000:
        expand, vout_order = int(v_out/10), 3
    else:
        expand, vout_order = v_out, 2
    # print(v_emit)
    emit, emit_order = calculate_order_and_value(v_emit)    
    # print(emit,emit_order)
    rand, rand_order = calculate_order_and_value(v_rand)
    # print(rand)
    ll = 0
    # 파일 경로 설정

    lum = int(Lumin * 10)

    if Geometry.upper() == 'NEBULA' :
        geo  = 2 
    elif Geometry.upper() == 'QSO' :
        geo = 3
    elif Geometry.upper() == 'Continuum' :
        geo = 4
    else : geo =1 # Test

    
    if idx == 1 :
        mode='W'
    else : mode = 'WO'

    col = int(Column_density_order*10)

    if metals < 1:
        metals_int = int(metals * 1000)   # 0.001 → 1, 0.01 → 10 등
        metals_str = f"{metals_int:04d}"  # 1 → '0001', 10 → '0010'
    else:
        metals_str = str(int(metals))     # 1.0 → '1', 2.0 → '2'

        
    folder_name = f'{mode}{atom}L{lum}M{metals_str}NH{col}'
    folder_path = f'/home/jin/RT/RT_scat/{folder_name}'

    if geo == 1 :
            path_rt = (f'/home/jin/RT/RT_scat/{mode}{atom}L{lum}M{metals_str}NH{col}/N_atom{geo}00E+10_'
                    f'Vexp000E+00_Vemit100E+00_'
                    f'tauD000E+00_Vran000E+00radi.dat')      
    else :
        path_rt = (f'/home/jin/RT/RT_scat/{mode}{atom}L{lum}M{metals_str}NH{col}/N_atom{geo}00E+10_'
                    f'Vexp{expand}E+0{vout_order}_Vemit{emit}E+0{emit_order}_'
                    f'tauD000E+00_Vran{rand}E+0{rand_order}_f_esc.dat')      

    return path_rt


# #separate K and H line
# def K_H_from_Combine_data_x(Line , atom_num , atom_index, vout, vemit, vran):
#     out, out_order = calculate_order_and_value(vout)
#     emit, emit_order = calculate_order_and_value(vemit)
#     ran , ran_order = calculate_order_and_value(vran)

#     path = r'/home/jin/data_CIV/N_atom{}0E+{}_Vexp{}E+0{}_Vemit{}E+0{}_tauD000E+00_Vran{}E+0{}spec.dat'.format(atom_num, atom_index , out, out_order, emit, emit_order, ran, ran_order)
#     try:
#         data = pd.read_csv(path, sep='\s+', header=None)
#     except:
#         print('파일을 찾을 수 없습니다.',path)


#     x = data[0].to_numpy()
#     if Line == 'h' or Line =='H':
#         lam = -  C_IV_H_A / ( (vran/cc_k)*x -1)
#         spec_tot = data[2].to_numpy()
#         spec_scat = data[4].to_numpy()

#     elif Line == 'k' or Line =='K':
#         lam = - C_IV_K_A / ( (vran/cc_k)*x -1)
#         spec_tot = data[1].to_numpy()
#         spec_scat = data[3].to_numpy()       

    
#     return lam , spec_tot , spec_scat , x




# def K_H_from_Combine_data(Line , atom_num , atom_index, vout, vemit, vran):
#     lam , spec_tot , spec_halo =  K_H_from_Combine_data_com(Line , atom_num , atom_index, vout, vemit, vran)
#     lam_c =  (C_IV_K_A + C_IV_H_A) / 2
#     if Line == 'k' or Line =='K':
#         ioc = np.where(lam<=lam_c)[0]
#         lam_x = lam[ioc]
#         spec_tot_x = spec_tot[ioc]
#         spec_halo_x  = spec_halo[ioc]

#     elif Line == 'h' or Line =='H':
#         ioc = np.where(lam>=lam_c)[0]
#         lam_x = lam[ioc]
#         spec_tot_x = spec_tot[ioc]
#         spec_halo_x  = spec_halo[ioc]
#     else :
#         lam_x  = lam 
#         spec_tot_x = spec_tot
#         spec_halo_x = spec_halo

#     return lam_x , spec_tot_x , spec_halo_x