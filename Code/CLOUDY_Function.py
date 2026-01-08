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
from astropy.constants import h, c, k_B, kpc
import astropy.units as u
from scipy import special
import warnings
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


def path(Lumin,idx,metals,Column_density_order):

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

    path_CIV = f"/home/jin/RT/Test_CLOUDY/Lum_{Lumin}_{idx}/metal_{metals}/N_H_{multi_factor}_{dir_order_str}/QSO_WO"
    path_way = f'/home/jin/RT/CLOUDY_new_Data/Lum_{Lumin}_{idx}/metal_{metals}/N_H_{multi_factor}_{dir_order_str}/QSO_WO'

    # path_CIV = os.path.join(path_way, f'QSO_{mode}/CIV_QSO')
    return path_way
def SB(z, radius_kpc, emissivity, dr):
    r_min, r_max = radius_kpc.min(), radius_kpc.max()
    Project_R = np.linspace(0, 100, 80) * kpc
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
    def lumin_integrand(R):
        if R == 0 :
            area = np.pi * (0.5*dR)**2
        elif R == r_max:
            area = np.pi * (2*R +0.5*dR)*0.5*dR 
        else:
            area = 2 * np.pi * R * np.interp(R, Project_R, surface_brightness)
        
        return area
    
    Lumin, _ = quad(lumin_integrand, 0, r_max)
    
    return Project_R / kpc, surface_brightness, Lumin


def CLOUDY_data(Lumin,idx,metals,Column_density_order):
    path_CIV = path(Lumin,idx,metals,Column_density_order)
    path_way = os.path.join(path_CIV, f'CIV_QSO')
    Mod = pc.CloudyModel(path_way)
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
    r_CIV = path_way +  '.ele_C'
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

    r_CIV = path_way +  '.ele_C'
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

    r_CIV = path_way +  '.ele_C'
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

    r_He = path_way +  '.ele_He'
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

    r_He = path_way +  '.ele_He'
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
        
    r_He = path_way +  '.ele_He'
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



    r_H = path_way +  '.ele_H'
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

    r_H = path_way +  '.ele_H'
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
        f'radius_p_{Lumin}': radius_p_CIV,
        f'SB_CIV_{Lumin}': SB_CIV,
        f'Lumin_CIV_{Lumin}': Lumin_CIV,
        f'SB_HeII_{Lumin}': SB_HeII,
        f'Lumin_HeII_{Lumin}': Lumin_HeII,
        f'SB_Lya_{Lumin}': SB_Lya,
        f'Lumin_Lya_{Lumin}': Lumin_Lya,
        f'radius_{Lumin}': radius,
        f'radius_kpc_{Lumin}': radius_kpc,
        f'frac_CIII_{Lumin}': CIII_frac,
        f'frac_CV_{Lumin}': CV_frac,
        f'frac_HeI_{Lumin}': HeI_frac,
        f'frac_HeIII_{Lumin}': HeIII_frac,
        f'frac_HI_{Lumin}': HI_frac,
        f'frac_HII_{Lumin}': HII_frac,
        f'ne_{Lumin}': n_e,
        f'Te_{Lumin}': Mod.te,
        f'Teff_{Lumin}': Mod.Teff,
        f'logU_{Lumin}': Mod.log_U,
        f'nH_{Lumin}': n_H,
        f'frac_CIV_{Lumin}': CIV_frac,
        f'emis_CIV_{Lumin}': CIV_emis,
        f'nden_CIV_{Lumin}': CIV_den,
        f'frac_HeII_{Lumin}': HeII_frac,  # 중복 없이 한 번만
        f'emis_HeII_{Lumin}': HeII_emis,
        f'nden_HeII_{Lumin}': HeII_den ,
        f'Lum_CIV_{Lumin}': CIV_Lum,
        f'Lum_HeII_{Lumin}': HeII_Lum,
        f'Lum_Lya_{Lumin}': Lya_Lum,
        f'Column_density_CIV_{Lumin}': CIV_Column_density,
        f'Column_density_HeII_{Lumin}': HeII_Column_density,
        f'Column_density_Lya_{Lumin}': Lya_Column_density,
        f'Column_density_H_{Lumin}': H_Column_density,
    }

    return result

def QSO_SED(path):
    file_name_w = os.path.join(path, f'QSO.sed')
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
    r_CIV = path_way +  '.ele_C'
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

    r_CIV = path_way +  '.ele_C'
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

    r_CIV = path_way +  '.ele_C'
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

    r_He = path_way +  '.ele_He'
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

    r_He = path_way +  '.ele_He'
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
        
    r_He = path_way +  '.ele_He'
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



    r_H = path_way +  '.ele_H'
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

    r_H = path_way +  '.ele_H'
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