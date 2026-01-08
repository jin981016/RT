from astropy.coordinates.representation import geodetic
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
from scipy.interpolate import interp1d

warnings.filterwarnings('ignore', category=IntegrationWarning)

kpc = const.kpc.cgs.value
h_ev = h.to(u.eV *u.s).value
c_cms = c.to(u.cm/u.s).value
c_kms = c.to(u.km/u.s).value
Ly_a_K = 1215.673644609e-8
Ly_a_H = 1215.668237310e-8
C_IV_K = 1548.187e-8
C_IV_H = 1550.772e-8
C_IV_K_A = 1548.187e-8
C_IV_H_A = 1550.772

# data_path = 

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

def _infer_multi_factor_from_column_density_order(Column_density_order):
    try:
        val = float(Column_density_order)
    except Exception:
        raise ValueError("Column_density_order must be convertible to float (e.g., 20.0 or 20.5)")

    fractional_part = abs(val - np.floor(val))
    if np.isclose(fractional_part, 0.0, atol=1e-6):
        return 1.0
    if np.isclose(fractional_part, 0.5, atol=1e-6):
        return 3.2
    raise ValueError("Only xx.0 or xx.5 are supported for Column_density_order to infer multi_factor")



def Data_path(path,style, atom_num, atom_index , V_out, V_emit, V_rand) :
    out, out_order = calculate_order_and_value(V_out)
    emit, emit_order = calculate_order_and_value(V_emit)
    ran , ran_order = calculate_order_and_value(V_rand)

    path_civ = r'{}/N_atom{}0E+{}_Vexp{}E+0{}_Vemit{}E+0{}_tauD000E+00_Vran{}E+0{}{}.dat'.format(path,atom_num, atom_index , out, out_order, emit, emit_order, ran, ran_order,style)
    if not os.path.exists(path_civ):
        print('파일을 찾을 수 없습니다.', path_civ)
        print('Style 확인해주세요. spec_com : spectrum_combine , spec : separated spectrum (only show K, H line) , radi : surface_brightness information , _f_esc : escape fraction + number of scattering')
    # print(path_civ)
    path_real =  r'{}/N_atom{}0E+{}_Vexp{}E+0{}_Vemit{}E+0{}_tauD000E+00_Vran{}E+0{}'.format(path,atom_num, atom_index , out, out_order, emit, emit_order, ran, ran_order)
    return path_real



def RT_CLOUDY_path(path,v_out, v_emit, v_rand, geo, atom, Lumin,idx,metals,Column_density_order):

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

# geo 변수의 타입을 먼저 확인하고 처리
    if isinstance(geo, str):
        geo_upper = geo.upper()
        if geo_upper == 'NEBULA':
            geo = 2
        elif geo_upper == 'QSO':
            geo = 3
        elif geo_upper == 'CONTINUUM':
            geo = 4
        elif geo_upper == 'TEST':
            geo = 1
    elif isinstance(geo, int):
        # 이미 숫자인 경우 그대로 사용 (유효성 검사 추가 가능)
        if geo not in [1, 2, 3, 4]:
            raise ValueError(f"Invalid geo value: {geo}")
    else:
        raise TypeError(f"geo must be string or int, got {type(geo)}")

    if idx == 1 :
        mode='W'
    else : mode = 'WO'

    col = int(Column_density_order*10)

    if metals < 1:
        metals_int = int(metals * 1000)   # 0.001 → 1, 0.01 → 10 등
        metals_str = f"{metals_int:04d}"  # 1 → '0001', 10 → '0010'
    else:
        metals_str = str(int(metals))     # 1.0 → '1', 2.0 → '2'

    # if style.upper() =='CLOUDY' :
    path_tt = f'{path}/{mode}{atom}L{lum}M{metals_str}NH{col}'

    # folder_path = f'{path}{folder_name}'

    # if geo == 1 :
    #         path_rt = (f'{path}/N_atom{geo}00E+10_'
    #                 f'Vexp000E+00_Vemit100E+00_'
    #                 f'tauD000E+00_Vran000E+00{style}')      
    # else :
    path_rt = (f'{path_tt}/N_atom{geo}00E+10_'
                f'Vexp{expand}E+0{vout_order}_Vemit{emit}E+0{emit_order}_'
                f'tauD000E+00_Vran{rand}E+0{rand_order}')      

    return path_rt



def spec_com(path):
    path = f'{path}spec_com.dat'
    try:
        data = pd.read_csv(path, sep='\s+', header=None)
    except:
        print('파일을 찾을 수 없습니다.',path)


    lam = data[0].to_numpy()
    spec_tot = data[1].to_numpy() # total photon
    spec_sc = data[2].to_numpy()  # scattering photon
   # spec_pol_tot = data[3]  # polization of the total photon
   # spec_pol_scat = data[4]  $ polization of the scattering photon
    print('Lambda, Escape+Scattering Photon Spectrum, Scattering Photon Spectrum ')
    return lam , spec_tot , spec_sc



#separate K and H line
def spec(path , line):
    path = f'{path}spec.dat'
    try:
        data = pd.read_csv(path, sep='\s+', header=None)
        print("v_rand = 11.8 km/s")
    except:
        print('파일을 찾을 수 없습니다.',path)

    vran = 11.8 #kms
    x = data[0].to_numpy()
    try:
        if line == 'h' or line == 'H':
            lam = -  C_IV_H_A / ( (vran/c_kms)*x -1)
            spec_tot = data[2].to_numpy()
            spec_scat = data[4].to_numpy()

        elif line == 'k' or line == 'K':
            lam = - C_IV_K_A / ( (vran/c_kms)*x -1)
            spec_tot = data[1].to_numpy()
            spec_scat = data[3].to_numpy()
        else:
            raise ValueError('invalid line')

        print('Lambda, Escape+Scattering Photon Spectrum, Scattering Photon Spectrum ')     
        return lam , spec_tot , spec_scat
    except Exception:
        print('line을 입력하세요 K or H')
        return None, None, None


def f_esc(path):
    path = f'{path}_f_esc.dat'
    try:
        data = pd.read_csv(path, sep='\s+', header=None)
    except:
        print('파일을 찾을 수 없습니다.',path)

    Total_esc = data.iloc[0,0] # Total escape photon / Input photon
    Ratio_K_H = data.iloc[0,1] # Escape photons as K photon  / Escape photons as H photon 
    Ratio_K = data.iloc[0,2] # Ratio of Escape photons as K photon / input K photon (total * 2/3 한듯?)
    Ratio_H = data.iloc[0,3] # Ratio of Escape photons as H photn / input H photon (total * 1/3 한듯?)
    NS_K = data.iloc[0,4] # Number of scattering of a photon (K line)
    NS_H = data.iloc[0,5] # Number of scattering of a photon (K line)
    NS_dust_K = data.iloc[0,6]  # Dust scattering of K
    NS_dust_H = data.iloc[0,7]  # Dust scattering of H 
    path_K = data.iloc[0,8] # Path_K
    path_H =  data.iloc[0,9] # Path_H
    Dir_esc_K = data.iloc[0,10] # Direct escape of K line / input K photon (total * 2/3 한듯?)
    Dir_esc_H = data.iloc[0,11] # Direct escape of H line / input H photon (total * 1/3 한듯?)
    NS_Clump_K = data.iloc[0,12] # Scattering in Clumpy medium - K
    NS_Clump_H = data.iloc[0,13]

    return {
        'total_esc': Total_esc,
        'ratio_k_h': Ratio_K_H,
        'ratio_k': Ratio_K,
        'ratio_h': Ratio_H,
        'ns_k': NS_K,
        'ns_h': NS_H,
        'ns_dust_k': NS_dust_K,
        'ns_dust_h': NS_dust_H,
        'path_k': path_K,
        'path_h': path_H,
        'dir_esc_k': Dir_esc_K,
        'dir_esc_h': Dir_esc_H,
        'ns_clump_k': NS_Clump_K,
        'ns_clump_h': NS_Clump_H,
    }
    


def K_H_from_spec_com(path,Line):
    lam , spec_tot , spec_halo =  spec_com(path)
    lam_c =  (C_IV_K_A + C_IV_H_A) / 2
    if Line == 'k' or Line =='K':
        ioc = np.where(lam<=lam_c)[0]
        lam_x = lam[ioc]
        spec_tot_x = spec_tot[ioc]
        spec_halo_x  = spec_halo[ioc]

    elif Line == 'h' or Line =='H':
        ioc = np.where(lam>=lam_c)[0]
        lam_x = lam[ioc]
        spec_tot_x = spec_tot[ioc]
        spec_halo_x  = spec_halo[ioc]
    else :
        lam_x  = lam 
        spec_tot_x = spec_tot
        spec_halo_x = spec_halo

    return lam_x , spec_tot_x , spec_halo_x



def RT_SB(path):
    path_sb = f'{path}radi.dat'
    # print(path_sb)
    """RT 산출물에서 Surface Brightness 데이터를 읽어오는 함수"""
    if not os.path.exists(path_sb):
        print(f"Warning: RT file not found: {path_sb}")
        return None
    
    try:
        name = ['radius','SB_K','SB_H','SB_tot','1','2','3']
        data_sp = pd.read_csv(path_sb, sep='\s+', header=None,names=name)
        rad, SB_t, SB_k,SB_h =  data_sp['radius'].to_numpy(),data_sp['SB_tot'].to_numpy(),data_sp['SB_K'].to_numpy(),data_sp['SB_H'].to_numpy()
        # print('radius, rdius[kpc], Surface_Brightness_Total')
        return rad*100, rad*100*kpc, SB_t 
    except Exception as e:
        print(f"Error reading RT file {path_sb}: {e}")
        return None 


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

# ----------------------------------------------------
#CLOUDY_data


def CLOUDY_path(Lumin,idx,metals,Column_density_order):

    # if idx == 1 :
    #     mode = "W" 
    # else : mode = "WO"
    multi_factor = _infer_multi_factor_from_column_density_order(Column_density_order)
    # 디렉터리명용 order: xx.5 -> xx.0 으로 보정
    try:
        _val = float(Column_density_order)
    except Exception:
        _val = Column_density_order
    if isinstance(_val, (float, int)):
        _frac = abs(_val - np.floor(_val))
        _dir_order = np.floor(_val) if np.isclose(_frac, 0.5, atol=1e-6) else _val
        dir_order_str = f"{_dir_order:.1f}"
    else:
        dir_order_str = str(Column_density_order)

    # path_CIV = f"/home/jin/RT/Test_CLOUDY/Lum_{Lumin}_{idx}/metal_{metals}/N_H_{multi_factor}_{dir_order_str}/QSO_WO"
    path_way = f'/home/jin/CLOUDY_new_Data/Lum_{Lumin}_{idx}/metal_{metals}/N_H_{multi_factor}_{dir_order_str}/QSO_WO'

    # path_CIV = os.path.join(path_way, f'QSO_{mode}/CIV_QSO')
    return path_way
def SB(z, radius_kpc, emissivity, dr):
    r_min, r_max = radius_kpc.min(), radius_kpc.max()
    Project_R = np.linspace(0, 100, 70) * kpc
    N = len(Project_R)
    surface_brightness = np.zeros(N)
    Lumin = np.zeros(N)

    
    emis_interp = interp1d(radius_kpc, emissivity, bounds_error=False, fill_value=0)
    
    for ii, R in enumerate(Project_R):
        # 적분 함수 정의
        def integrand(r):
            if r < R:
                return 0
            else:
                emis = emis_interp(r)
                return emis * r / np.sqrt(r**2 - R**2)
            
        surface_brightness[ii], _ = quad(integrand, R, r_max)
        surface_brightness[ii] *= 2 / (1+z)**4
    dR =  Project_R[1] - Project_R[0]    
    # def lumin_integrand(R):
    #     if R == 0 :
    #         area = np.pi * (0.5*dR)**2
    #     elif R == r_max:
    #         area = np.pi * (2*R +0.5*dR)*0.5*dR 
    #     else:
    #         area = 2 * np.pi * R * np.interp(R, Project_R, surface_brightness)
        
    #     return area
    # Lumin, _ = quad(lumin_integrand, 0, r_max)
    Lumin = np.trapz(2 * np.pi * Project_R * surface_brightness, Project_R)
    
    return Project_R / kpc, surface_brightness, Lumin

def QSO_SED(path_way):
    file_name_w = os.path.join(path_way, f'QSO.sed')
    file_w = pd.read_csv(
    file_name_w,
    comment='#',
    sep=r'\s+',
    engine='python',
    header=None,
    names=['Ryd', 'nufnu']
    )
    Ryd_w, nufnu_w = file_w['Ryd'].to_numpy(), file_w['nufnu'].to_numpy()
    return  Ryd_w, nufnu_w



def CLOUDY_data_path(path_way):
    path_CIV = os.path.join(path_way, f'CIV_QSO')
    # print(path_CIV )
    Mod = pc.CloudyModel(path_CIV)
    Mod.ionic_names
    N_H = sum(Mod.dr*Mod.nH)

    # solar_metallicity
    frac_He =1.00E-01
    frac_C = 2.45E-04
    frac_O = 4.90E-04
    frac_N = 8.51E-05
    frac_Mg = 3.47E-05

    N_HI = sum(Mod.dr*Mod.nH*Mod.get_ionic('H',0))
    N_HII = sum(Mod.dr*Mod.nH*Mod.get_ionic('H',1))
    N_HeII = frac_He*sum(Mod.dr*Mod.nH*Mod.get_ionic('He',1))
    N_OVI = frac_O*sum(Mod.dr*Mod.nH*Mod.get_ionic('O',5))
    N_NV = frac_N*sum(Mod.dr*Mod.nH*Mod.get_ionic('N',4))
    N_CIV = frac_C*sum(Mod.dr*Mod.nH*Mod.get_ionic('C',3))

    num = len(Mod.nH)
    r_CIV = path_CIV +  '.ele_C'
    f = open(r_CIV,'r')
    header = f.readline()
    CIV_frac = np.zeros(num)
    i = 0
    for line in f:
        line = line.strip()
        columns = line.split()
        j = float(columns[4])
        CIV_frac[i] = j
        i = i + 1

    r_CIV = path_CIV +  '.ele_C'
    f = open(r_CIV,'r')
    header = f.readline()
    CV_frac = np.zeros(num)
    i = 0
    for line in f:
        line = line.strip()
        columns = line.split()
        j = float(columns[5])
        CV_frac[i] = j
        i = i + 1

    r_CIV = path_CIV +  '.ele_C'
    f = open(r_CIV,'r')
    header = f.readline()
    CIII_frac = np.zeros(num)
    i = 0
    for line in f:
        line = line.strip()
        columns = line.split()
        j = float(columns[3])
        CIII_frac[i] = j
        i = i + 1

    r_He = path_CIV +  '.ele_He'
    f = open(r_He,'r')
    header = f.readline()
    HeII_frac = np.zeros(num)
    i = 0
    for line in f:
        line = line.strip()
        columns = line.split()
        j = float(columns[2])
        HeII_frac[i] = j
        i = i + 1

    r_He = path_CIV +  '.ele_He'
    f = open(r_He,'r')
    header = f.readline()
    HeIII_frac = np.zeros(num)
    i = 0
    for line in f:
        line = line.strip()
        columns = line.split()
        j = float(columns[3])
        HeIII_frac[i] = j
        i = i + 1
        
    r_He = path_CIV +  '.ele_He'
    f = open(r_He,'r')
    header = f.readline()
    HeI_frac = np.zeros(num)
    i = 0
    for line in f:
        line = line.strip()
        columns = line.split()
        j = float(columns[1])
        HeI_frac[i] = j
        i = i + 1



    r_H = path_CIV +  '.ele_H'
    f = open(r_H,'r')
    header = f.readline()
    HII_frac = np.zeros(num)
    i = 0
    for line in f:
        line = line.strip()
        columns = line.split()
        j = float(columns[2])
        HII_frac[i] = j
        i = i + 1

    r_H = path_CIV +  '.ele_H'
    f = open(r_H,'r')
    header = f.readline()
    HI_frac = np.zeros(num)
    i = 0
    for line in f:
        line = line.strip()
        columns = line.split()
        j = float(columns[1])
        HI_frac[i] = j
        i = i + 1



    radius = Mod.radius / kpc
    radius_kpc = Mod.radius
    dr = Mod.dr

    n_H = Mod.nH
    n_e = Mod.ne
    n_He = n_H * frac_He
    n_C = n_H * frac_C
    nden_CIV = CIV_frac * n_C
    nden_HeII = HeII_frac * n_He

    CIV_Lum = float(Mod.get_emis_vol('C__4_154819A')) + float(Mod.get_emis_vol('C__4_155078A'))
    CIV_emis = Mod.get_emis('C__4_154819A') + Mod.get_emis('C__4_155078A')
    CIV_den = nden_CIV

    Lya_Lum = float(Mod.get_emis_vol('H__1_121567A'))
    Lya_emis = Mod.get_emis('H__1_121567A')
    Lya_den = n_H

    HeII_Lum = float(Mod.get_emis_vol('HE_2_164043A'))
    HeII_emis = Mod.get_emis('HE_2_164043A')
    HeII_den = nden_HeII

    CIV_Column_density = np.sum(dr * CIV_den)
    HeII_Column_density = np.sum(dr * HeII_den)
    Lya_Column_density = np.sum(dr * Lya_den)
    H_Column_density = np.sum(dr * n_H)

    # SB 계산 결과
    radius_p_CIV, SB_CIV, Lumin_CIV = SB(0, radius_kpc, CIV_emis, dr)
    radius_p_HeII, SB_HeII, Lumin_HeII = SB(0, radius_kpc, HeII_emis, dr)
    radius_p_Lya, SB_Lya, Lumin_Lya = SB(0, radius_kpc, Lya_emis, dr)

    # 리턴할 값들 딕셔너리에 정리
    result = {
        f'radius_p': radius_p_CIV,
        f'SB_CIV': SB_CIV,
        f'Lumin_CIV': Lumin_CIV,
        f'SB_HeII': SB_HeII,
        f'Lumin_HeII': Lumin_HeII,
        f'SB_Lya': SB_Lya,
        f'Lumin_Lya': Lumin_Lya,
        f'radius': radius,
        f'radius_kpc': radius_kpc,
        f'frac_CIII': CIII_frac,
        f'frac_CV': CV_frac,
        f'frac_HeI': HeI_frac,
        f'frac_HeIII': HeIII_frac,
        f'frac_HI': HI_frac,
        f'frac_HII': HII_frac,
        f'ne': n_e,
        f'Te': Mod.te,
        f'Teff': Mod.Teff,
        f'logU': Mod.log_U,
        f'nH': n_H,
        f'frac_CIV': CIV_frac,
        f'emis_CIV': CIV_emis,
        f'nden_CIV': CIV_den,
        f'frac_HeII': HeII_frac,  # 중복 없이 한 번만
        f'emis_HeII': HeII_emis,
        f'nden_HeII': HeII_den,
        f'Lum_CIV': CIV_Lum,
        f'Lum_HeII': HeII_Lum,
        f'Lum_Lya': Lya_Lum,
        f'Column_density_CIV': CIV_Column_density,
        f'Column_density_HeII': HeII_Column_density,
        f'Column_density_Lya': Lya_Column_density,
        f'Column_density_H': H_Column_density,
    }

    return result