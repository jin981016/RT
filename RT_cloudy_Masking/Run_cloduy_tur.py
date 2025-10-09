import numpy as np
import matplotlib.pyplot as plt
import pyCloudy as pc
import pyneb as pn
from astropy import units as u
import os
from astropy import constants as const
from astropy.io import ascii
from numpy import log10, exp
import shutil
import pandas as pd
from astropy.constants import h, c, k_B
from scipy.integrate import trapezoid


kpc = const.kpc.cgs.value


# Lum = np.arange(42,47,1) # f(nu) range
# Lum = np.arange(42,44,1)
# Lum = np.array([43.5,46.5,47])
Lum = np.array([45.5])
# N_H_odrer= np.arange(19,25,0.5)

# N_H_odrer= np.arange(20,23.5,0.5)
N_H_odrer= np.array([22])
# N_H_odrer= np.array([22.0])


multi_factor = np.array([1])

turbulance = np.array([1,10,11.8,15,30,50,100])

path = r"/home/jin/RT/RT_cloudy_Masking/"
usr_cloudy = r"/home/jin/Cloudy/c17.03/source/cloudy.exe"
# usr_cloudy = r"/home/jin/Cloudy/source/cloudy.exe"

#--------------------------------------------------
emis=['H  1  1215.67A',
'H  1  1025.72A',
'H  1  4861.33A',
'H  1  6562.81A',
'O  6  1031.91A',
'O  6  1037.62A',
'He 2  949.31A',
'He 2  972.108A',
'He 2  1025.27A',
'He 2  1640.43A',
'He 2  4859A',
'He 2  4339A',
'He 2  6527A',
'He 2  6560A',
'C  4  1550.78',
'O  3  5006.84A',
'N  5  1238.82',
'N  5  1242.80',
'c  4  1548.19',
'c  4  1550.78',
'mg 2  2795.53',
'mg 2  2802.71'
]

#--------------------------------------------------

#--------------------------------------------------
h_ev = h.to(u.eV *u.s).value

Ly_a_K = 1215.673644609e-8
Ly_a_H = 1215.668237310e-8
C_IV_K = 1548.187e-8  # cm
C_IV_H = 1550.772e-8  # cm  
c_cms=c.to(u.cm/u.s).value

nu_CIV_K = c_cms /C_IV_K
nu_CIV_H = c_cms /C_IV_H

nu_Lya_K = c_cms /Ly_a_K
nu_Lya_H = c_cms /Ly_a_H

Ryd_CIV_K=  h_ev*nu_CIV_K / 13.6
Ryd_CIV_H=  h_ev*nu_CIV_H / 13.6

Ryd_Lya_K=  h_ev*nu_Lya_K / 13.6
Ryd_Lya_H=  h_ev*nu_Lya_H / 13.6



def CLOUDY_RUN(Lumin,Column_density_order,multi_factor,tur):

    r_in_e  = 1.0         # [kpc]
    r_out_e = 100.        # [kpc]
    r_e   = 0.5 * r_out_e
    NHI_total = multi_factor*10**(Column_density_order)    # [cm^-2]

    def CLOUDY_QSO(Lumin,Column_density_order,multi_factor,tur):

        folder_path = os.path.join(path, f"Lum_{Lumin}_1/N_H_{multi_factor}_{Column_density_order}/tur_{tur}", "w_LT/QSO")  # ← '/' 제거
        os.makedirs(folder_path, exist_ok=True)

        # dlaw 파일 생성
        file_dlaw = "dlaw_HI_exp.ini"
        file_dlaw_full = os.path.join(folder_path, file_dlaw)
        n0 = 1.0
        rr = np.logspace(log10(r_in_e), log10(r_out_e), 101)

        nn_exp = n0 * exp(-rr / r_e)
        n0_analytic = NHI_total / (exp(-r_in_e / r_e) - exp(-r_out_e / r_e)) / (r_e * kpc)
        NHI_integ = trapezoid(nn_exp, rr) * kpc
        n0 = NHI_total / NHI_integ
        nn_exp = n0 * exp(-rr / r_e)

        rr_pad = r_out_e * (rr[1] / rr[0])
        nn_pad = n0 * exp(-rr_pad / r_e)

        with open(file_dlaw_full, 'w') as fp:
            fp.write("dlaw table\n")
            for r_i, n_i in zip(rr, nn_exp):
                fp.write("continue {:8.5f} {:8.5f}\n".format(log10(r_i * kpc), log10(n_i)))
            fp.write("continue {:8.5f} {:8.5f}\n".format(log10(rr_pad * kpc), log10(nn_pad)))
            fp.write("end of dlaw\n")

        sed = os.path.join(path, "QSO.sed")
        sed_QSO = os.path.join(folder_path, "QSO.sed")
        shutil.copy(sed, sed_QSO)


        name = ['a','b','c']
        data = pd.read_csv(sed_QSO,sep='\t',comment='#',names=name)
        data = data[:-1]
        Ryd_x = data['a'].to_numpy().astype(float)
        Ryd_y = data['b'].to_numpy()

        Ryd_y = Ryd_y / (Ryd_x*13.6/ h_ev)
        with open(sed_QSO, "w") as f:
            f.write("#Interpolation QSO SED from Arrigoni-Battaia et al. (2015)\n")  # 헤더
            for xi, yi in zip(Ryd_x, Ryd_y):
                f.write(f"{xi}\t{yi}\n")



        model_name = "CIV_QSO"
        full_path = os.path.join(folder_path, model_name)

        options = [
            'sphere',
            'init "dlaw_HI_exp.ini"',
            'table SED "QSO.sed"',
            f'nuL(nu) = {Lumin} at 1 Ryd',
            f'turbulence {tur} km/s',
            'stop temperature off',
            'stop column density 24.0'
        ]


        r_in, r_out = 21.50, 23.50

        c_input = pc.CloudyInput(full_path)

        c_input.set_radius(r_in=r_in, r_out=r_out)
        c_input.set_iterate(to_convergence=True)
        c_input.set_other(options)
        c_input.set_emis_tab(emis)

        # 생성 확인
        c_input.print_input() # ← 확실하게 in 파일 만들기

        pc.log_.message(f'Running {full_path}', calling='test_1')
        pc.config.cloudy_exe = usr_cloudy
        pc.log_.timer('Starting Cloudy', calling=f'test_{model_name}')
        c_input.run_cloudy()
        pc.log_.timer('Cloudy ended after seconds:', calling=f'test_{model_name}')



        folder_path = os.path.join(path, f"Lum_{Lumin}_2/N_H_{multi_factor}_{Column_density_order}/tur_{tur}", "w_LT/QSO")  # ← '/' 제거
        os.makedirs(folder_path, exist_ok=True)

        # dlaw 파일 생성
        file_dlaw = "dlaw_HI_exp.ini"
        file_dlaw_full = os.path.join(folder_path, file_dlaw)
        n0 = 1.0
        rr = np.logspace(log10(r_in_e), log10(r_out_e), 101)

        nn_exp = n0 * exp(-rr / r_e)
        n0_analytic = NHI_total / (exp(-r_in_e / r_e) - exp(-r_out_e / r_e)) / (r_e * kpc)
        NHI_integ = trapezoid(nn_exp, rr) * kpc
        n0 = NHI_total / NHI_integ
        nn_exp = n0 * exp(-rr / r_e)

        rr_pad = r_out_e * (rr[1] / rr[0])
        nn_pad = n0 * exp(-rr_pad / r_e)

        with open(file_dlaw_full, 'w') as fp:
            fp.write("dlaw table\n")
            for r_i, n_i in zip(rr, nn_exp):
                fp.write("continue {:8.5f} {:8.5f}\n".format(log10(r_i * kpc), log10(n_i)))
            fp.write("continue {:8.5f} {:8.5f}\n".format(log10(rr_pad * kpc), log10(nn_pad)))
            fp.write("end of dlaw\n")

        sed = os.path.join(path, "QSO.sed")
        sed_QSO = os.path.join(folder_path, "QSO.sed")
        shutil.copy(sed, sed_QSO)
        
        name = ['a','b','c']
        data = pd.read_csv(sed_QSO,sep='\t',comment='#',names=name)
        data = data[:-1]
        Ryd_x = data['a'].to_numpy().astype(float)
        Ryd_y = data['b'].to_numpy()

        Ryd_y = Ryd_y / (Ryd_x*13.6/ h_ev)
        with open(sed_QSO, "w") as f:
            f.write("#Interpolation QSO SED from Arrigoni-Battaia et al. (2015)\n")  # 헤더
            for xi, yi in zip(Ryd_x, Ryd_y):
                f.write(f"{xi}\t{yi}\n")



        model_name = "CIV_QSO"
        full_path = os.path.join(folder_path, model_name)

        options = [
            'sphere',
            'init "dlaw_HI_exp.ini"',
            'table SED "QSO.sed"',
            'no line transfer',
            f'nuL(nu) = {Lumin} at 1 Ryd',
            f'turbulence {tur} km/s',
            'stop temperature off',
            'stop column density 24.0'
        ]


        r_in, r_out = 21.50, 23.50

        c_input = pc.CloudyInput(full_path)

        c_input.set_radius(r_in=r_in, r_out=r_out)
        c_input.set_iterate(to_convergence=True)
        c_input.set_other(options)
        c_input.set_emis_tab(emis)

        # 생성 확인
        c_input.print_input() # ← 확실하게 in 파일 만들기

        pc.log_.message(f'Running {full_path}', calling='test_1')
        pc.config.cloudy_exe = usr_cloudy
        pc.log_.timer('Starting Cloudy', calling=f'test_{model_name}')
        c_input.run_cloudy()
        pc.log_.timer('Cloudy ended after seconds:', calling=f'test_{model_name}')
       
        

    def CLOUDY_QSO_Masking(Lumin,Column_density_order,multi_factor,tur):


    #--------------------------------------------------------------------------------------------------------
        name = ['a','b','c']
        data = pd.read_csv(f'{path}/QSO.sed',sep='\t',comment='#',names=name)
        data = data[:-1]
        Ryd_x = data['a'].to_numpy().astype(float)
        Ryd_y = data['b'].to_numpy()

        ii = np.where((Ryd_x >= 0.1) & (Ryd_x <= 1))[0]
        jj = ii[-1]+1

        Ryd_xx = np.arange(Ryd_x[ii][0],Ryd_x[jj],0.001)
        Ryd_yy= np.interp(Ryd_xx,Ryd_x,Ryd_y)

        # 보간 데이터 추가
        Ryd_x_new = np.concatenate([Ryd_x, Ryd_xx])
        Ryd_y_new = np.concatenate([Ryd_y, Ryd_yy])

        # x 기준 정렬
        sort_idx = np.argsort(Ryd_x_new)
        Ryd_x_new = Ryd_x_new[sort_idx]
        Ryd_y_new = Ryd_y_new[sort_idx]

        Ryd_x_new, unique_idx = np.unique(Ryd_x_new, return_index=True)
        Ryd_y_new = Ryd_y_new[unique_idx]
        # 특정 구간 선형 보간 (예시)

        kk = np.where((Ryd_x_new >= 0.1) & (Ryd_x_new <= 1))[0]
        slope = (Ryd_y_new[kk][-1] - Ryd_y_new[kk][0]) / (Ryd_x_new[kk][-1] - Ryd_x_new[kk][0])
        Ryd_in = slope * (Ryd_x_new[kk] - Ryd_x_new[kk][0]) + Ryd_y_new[kk][0]
        Ryd_y_new[kk] = Ryd_in

        Ryd_result = Ryd_y_new.copy()

        cut_idx = 30
        # CIV 영역 처리
        idx_CIV_K = np.abs(Ryd_x_new - Ryd_CIV_K).argmin()  # Ryd_CIV_K와 가장 가까운 x값의 인덱스
        idx_CIV_H = np.abs(Ryd_x_new - Ryd_CIV_H).argmin()  # Ryd_CIV_H와 가장 가까운 x값의 인덱스

        # CIV 영역 마스킹 (두 값 사이와 외각 +2)
        start_CIV = max(0, min(idx_CIV_K, idx_CIV_H) - cut_idx)  # 더 작은 인덱스에서 cut_idx 뺀 값
        end_CIV = min(len(Ryd_x_new), max(idx_CIV_K, idx_CIV_H) + (cut_idx+1))  # 더 큰 인덱스에서 cut_idx 더한 값 

        Ryd_result[start_CIV:end_CIV] = (Ryd_x_new[start_CIV:end_CIV]*(h_ev/13.6)) 

        cut_idx = 15
        idx_Lya_K = np.abs(Ryd_x_new - Ryd_Lya_K).argmin()  # Ryd_Lya_K와 가장 가까운 x값의 인덱스
        idx_Lya_H = np.abs(Ryd_x_new - Ryd_Lya_H).argmin()  # Ryd_Lya_H와 가장 가까운 x값의 인덱스

        # Lyman-alpha 영역 마스킹 (두 값 사이와 외각 +2)
        start_Lya = max(0, min(idx_Lya_K, idx_Lya_H) - cut_idx)
        end_Lya = min(len(Ryd_x_new), max(idx_Lya_K, idx_Lya_H) + (cut_idx+1))


        Ryd_result[start_Lya:end_Lya] = (Ryd_x_new[start_Lya:end_Lya]*(h_ev/13.6)) 


    #--------------------------------------------------------------------------------------------------------
 # ← '/' 제거
        folder_path = os.path.join(path, f"Lum_{Lumin}_3/N_H_{multi_factor}_{Column_density_order}/tur_{tur}", "w_LT/QSO_M")
        os.makedirs(folder_path, exist_ok=True)

        # dlaw 파일 생성
        file_dlaw = "dlaw_HI_exp.ini"
        file_dlaw_full = os.path.join(folder_path, file_dlaw)
        n0 = 1.0
        rr = np.logspace(log10(r_in_e), log10(r_out_e), 101)

        nn_exp = n0 * exp(-rr / r_e)

        # Scale to match the total H column density
        # Analytic solution
        n0_analytic = NHI_total / (exp(-r_in_e / r_e) - exp(-r_out_e / r_e)) / (r_e * kpc)

        # Numerical integration for arbitrary functional form using trapz
        NHI_integ = trapezoid(nn_exp, rr) * kpc
        n0 = NHI_total / NHI_integ
        nn_exp = n0 * exp(-rr / r_e)

        rr_pad = r_out_e * (rr[1] / rr[0])
        nn_pad = n0 * exp(-rr_pad / r_e)



        with open(file_dlaw_full, 'w') as fp:
            fp.write("dlaw table\n")
            for r_i, n_i in zip(rr, nn_exp):
                fp.write("continue {:8.5f} {:8.5f}\n".format(log10(r_i * kpc), log10(n_i)))
            fp.write("continue {:8.5f} {:8.5f}\n".format(log10(rr_pad * kpc), log10(nn_pad)))
            fp.write("end of dlaw\n")


        Ryd_result = Ryd_result / (Ryd_x_new*13.6/ h_ev)
        sed_QSO = os.path.join(folder_path, "QSO_masking.sed")
        with open(sed_QSO, "w") as f:
            f.write("#Interpolation QSO SED from Arrigoni-Battaia et al. (2015)\n")  # 헤더
            for xi, yi in zip(Ryd_x_new, Ryd_result):
                f.write(f"{xi}\t{yi}\n")





        # # 모델 세팅
        model_name = "CIV_QSO"
        full_path = os.path.join(folder_path, model_name)

        options = [
            'sphere',
            'init "dlaw_HI_exp.ini"',
            'table SED "QSO_masking.sed"',
            f'nuL(nu) = {Lumin} at 1 Ryd',
            f'turbulence {tur} km/s',   
            'stop temperature off',
            'stop column density 24.0'
        ]


        r_in, r_out = 21.50, 23.50

        c_input = pc.CloudyInput(full_path)

        c_input.set_radius(r_in=r_in, r_out=r_out)
        c_input.set_iterate(to_convergence=True)
        c_input.set_other(options)
        c_input.set_emis_tab(emis)

        # 생성 확인
        c_input.print_input() # ← 확실하게 in 파일 만들기

        pc.log_.message(f'Running {full_path}', calling='test_1')
        pc.config.cloudy_exe = usr_cloudy
        pc.log_.timer('Starting Cloudy', calling=f'test_{model_name}')
        c_input.run_cloudy()
        pc.log_.timer('Cloudy ended after seconds:', calling=f'test_{model_name}')


    #--------------------------------------------------------------------------------------------------------
        folder_path = os.path.join(path, f"Lum_{Lumin}_4/N_H_{multi_factor}_{Column_density_order}/tur_{tur}", "w_LT/QSO_M")
        os.makedirs(folder_path, exist_ok=True)

        # dlaw 파일 생성
        file_dlaw = "dlaw_HI_exp.ini"
        file_dlaw_full = os.path.join(folder_path, file_dlaw)
        n0 = 1.0
        rr = np.logspace(log10(r_in_e), log10(r_out_e), 101)

        nn_exp = n0 * exp(-rr / r_e)

        # Scale to match the total H column density
        # Analytic solution
        n0_analytic = NHI_total / (exp(-r_in_e / r_e) - exp(-r_out_e / r_e)) / (r_e * kpc)

        # Numerical integration for arbitrary functional form using trapz
        NHI_integ = trapezoid(nn_exp, rr) * kpc
        n0 = NHI_total / NHI_integ
        nn_exp = n0 * exp(-rr / r_e)

        rr_pad = r_out_e * (rr[1] / rr[0])
        nn_pad = n0 * exp(-rr_pad / r_e)



        with open(file_dlaw_full, 'w') as fp:
            fp.write("dlaw table\n")
            for r_i, n_i in zip(rr, nn_exp):
                fp.write("continue {:8.5f} {:8.5f}\n".format(log10(r_i * kpc), log10(n_i)))
            fp.write("continue {:8.5f} {:8.5f}\n".format(log10(rr_pad * kpc), log10(nn_pad)))
            fp.write("end of dlaw\n")

        Ryd_result = Ryd_result / (Ryd_x_new*13.6/ h_ev)
        sed_QSO = os.path.join(folder_path, "QSO_masking.sed")
        with open(sed_QSO, "w") as f:
            f.write("#Interpolation QSO SED from Arrigoni-Battaia et al. (2015)\n")  # 헤더
            for xi, yi in zip(Ryd_x_new, Ryd_result):
                f.write(f"{xi}\t{yi}\n")





        # # 모델 세팅
        model_name = "CIV_QSO"
        full_path = os.path.join(folder_path, model_name)

        options = [
            'sphere',
            'init "dlaw_HI_exp.ini"',
            'table SED "QSO_masking.sed"',
            'no line transfer',
            f'nuL(nu) = {Lumin} at 1 Ryd',
            f'turbulence {tur} km/s',
            'stop temperature off',
            'stop column density 24.0'
        ]


        r_in, r_out = 21.50, 23.50

        c_input = pc.CloudyInput(full_path)

        c_input.set_radius(r_in=r_in, r_out=r_out)
        c_input.set_iterate(to_convergence=True)
        c_input.set_other(options)
        c_input.set_emis_tab(emis)

        # 생성 확인
        c_input.print_input() # ← 확실하게 in 파일 만들기

        pc.log_.message(f'Running {full_path}', calling='test_1')
        pc.config.cloudy_exe = usr_cloudy
        pc.log_.timer('Starting Cloudy', calling=f'test_{model_name}')
        c_input.run_cloudy()
        pc.log_.timer('Cloudy ended after seconds:', calling=f'test_{model_name}')

    
    CLOUDY_QSO(Lumin,Column_density_order,multi_factor,tur)
    CLOUDY_QSO_Masking(Lumin,Column_density_order,multi_factor,tur)
 
                
    return print('Done')



for jj, Column_density_order in enumerate(N_H_odrer):
    for kk , tur in enumerate(turbulance):
        for ii, Lumin in enumerate(Lum):
            if Lumin == 45.5:
                Lumin = 45.5
                CLOUDY_RUN(Lumin,Column_density_order,1,tur)
            else: 
                Lumin = int(Lumin)
                CLOUDY_RUN(Lumin,Column_density_order,1,tur)