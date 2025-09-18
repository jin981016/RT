import numpy as np
import pyCloudy as pc
import pandas as pd
import shutil, os
import time
from astropy import units as u
from astropy.constants import h, c, k_B, kpc
from numpy import log10, exp
from scipy.integrate import trapezoid
from multiprocessing import Pool

# ---------------------- 설정 ----------------------
mode = "WO"   # "WO", "W", "BOTH" 중 선택
overwrite = False  # True: 기존 파일 덮어쓰기, False: 기존 결과가 있으면 건너뜀

# Lum = np.array([37.0,38.0,39.0])
Lum = np.arange(40,47,0.5)
metals = np.array([1.0,0.1,0.01,0.001,10.0])
N_H_odrer = np.array([20.0,21.0,22.0,23.0,24.0,25.0]) # 20 - 25
multi_factor = np.array([1.0,3.2])

# 5 * 5 * 2 = 50 per 1 Lum  avg 5 min  = 250 mins    
# 14 Lum * 250 mins *2 / 14 node = 

path = r"/home/jin/RT/CLOUDY_new_Data/"
usr_cloudy = r"/home/jin/Cloudy/c17.03/source/cloudy.exe"
path_list_file = os.path.join(path, "path_list.txt")
path_done_list_file = os.path.join(path, "path_done_list.txt")

emis=['H  1  1215.67A','H  1  1025.72A','H  1  4861.33A','H  1  6562.81A',
'O  6  1031.91A','O  6  1037.62A','He 2  949.31A','He 2  972.108A',
'He 2  1025.27A','He 2  1640.43A','He 2  4859A','He 2  4339A',
'He 2  6527A','He 2  6560A','C  4  1550.78','O  3  5006.84A',
'N  5  1238.82','N  5  1242.80','c  4  1548.19','c  4  1550.78',
'mg 2  2795.53','mg 2  2802.71']

h_ev = h.to(u.eV *u.s).value
c_cms = c.to(u.cm/u.s).value

Ly_a_K = 1215.673644609e-8
Ly_a_H = 1215.668237310e-8
C_IV_K = 1548.187e-8
C_IV_H = 1550.772e-8

nu_CIV_K = c_cms /C_IV_K
nu_CIV_H = c_cms /C_IV_H
nu_Lya_K = c_cms /Ly_a_K
nu_Lya_H = c_cms /Ly_a_H

Ryd_CIV_K=  h_ev*nu_CIV_K / 13.6
Ryd_CIV_H=  h_ev*nu_CIV_H / 13.6
Ryd_Lya_K=  h_ev*nu_Lya_K / 13.6
Ryd_Lya_H=  h_ev*nu_Lya_H / 13.6

# ---------------------- SED 처리 함수 ----------------------
def process_sed_wo(sed, folder_path):
    sed_QSO = os.path.join(folder_path, "QSO.sed")
    shutil.copy(sed, sed_QSO)
    data = pd.read_csv(sed_QSO, sep='\t', comment='#', names=['a','b','c'])
    data = data[:-1]
    Ryd_x = data['a'].to_numpy().astype(float)
    Ryd_y = data['b'].to_numpy()
    Ryd_y = Ryd_y / (Ryd_x*13.6/ h_ev)
    # Ryd_y = Ryd_y  #*Ryd_x / h_ev
    with open(sed_QSO, "w") as f:
        f.write("#Interpolation QSO SED (WO)\n")
        for xi, yi in zip(Ryd_x, Ryd_y):
            f.write(f"{xi}\t{yi}\n")
    return sed_QSO

def process_sed_w(sed, folder_path):
    sed_QSO = os.path.join(folder_path, "QSO.sed")
    shutil.copy(sed, sed_QSO)
    data = pd.read_csv(sed_QSO, sep='\t', comment='#', names=['a','b','c'])
    data = data[:-1]
    Ryd_x = data['a'].to_numpy().astype(float)
    Ryd_y = data['b'].to_numpy()

    # Ryd_y = Ryd_y*Ryd_x / h_ev
    # --- 보간 + 마스킹 ---
    ii = np.where((Ryd_x >= 0.1) & (Ryd_x <= 1))[0]
    jj = ii[-1]+1
    Ryd_xx = np.arange(Ryd_x[ii][0],Ryd_x[jj],0.001)
    Ryd_yy= np.interp(Ryd_xx,Ryd_x,Ryd_y)

    Ryd_x_new = np.concatenate([Ryd_x, Ryd_xx])
    Ryd_y_new = np.concatenate([Ryd_y, Ryd_yy])
    sort_idx = np.argsort(Ryd_x_new)
    Ryd_x_new = Ryd_x_new[sort_idx]
    Ryd_y_new = Ryd_y_new[sort_idx]
    Ryd_x_new, unique_idx = np.unique(Ryd_x_new, return_index=True)
    Ryd_y_new = Ryd_y_new[unique_idx]

    kk = np.where((Ryd_x_new >= 0.1) & (Ryd_x_new <= 1))[0]
    slope = (Ryd_y_new[kk][-1] - Ryd_y_new[kk][0]) / (Ryd_x_new[kk][-1] - Ryd_x_new[kk][0])
    Ryd_y_new[kk] = slope * (Ryd_x_new[kk] - Ryd_x_new[kk][0]) + Ryd_y_new[kk][0]
    Ryd_result = Ryd_y_new.copy()

    cut_idx = 0.025/13.6
    idx_CIV_K = np.abs(Ryd_x_new - Ryd_CIV_K).argmin()
    idx_CIV_H = np.abs(Ryd_x_new - Ryd_CIV_H).argmin()
    center_idx_civ = int(round((idx_CIV_K + idx_CIV_H) / 2))
    left_civ = max(0, center_idx_civ - 50)
    right_civ = min(len(Ryd_x_new) - 1, center_idx_civ + 50)
    local_dx_civ = np.median(np.diff(Ryd_x_new[left_civ:right_civ+1]))
    cut_val_civ = cut_idx if cut_idx <= 1 else cut_idx * local_dx_civ
    mask_CIV = (Ryd_x_new >= (min(Ryd_CIV_K, Ryd_CIV_H)-cut_val_civ)) & (Ryd_x_new <= (max(Ryd_CIV_K, Ryd_CIV_H)+cut_val_civ))
    Ryd_result[mask_CIV] = (Ryd_x_new[mask_CIV] * (h_ev/13.6))

    cut_idx_lya = 0.025/13.6
    idx_Lya_K = np.abs(Ryd_x_new - Ryd_Lya_K).argmin()
    idx_Lya_H = np.abs(Ryd_x_new - Ryd_Lya_H).argmin()
    center_idx_lya = int(round((idx_Lya_K + idx_Lya_H) / 2))
    left_lya = max(0, center_idx_lya - 50)
    right_lya = min(len(Ryd_x_new) - 1, center_idx_lya + 50)
    local_dx_lya = np.median(np.diff(Ryd_x_new[left_lya:right_lya+1]))
    cut_val_lya = cut_idx_lya if cut_idx_lya <= 1 else cut_idx_lya * local_dx_lya
    mask_Lya = (Ryd_x_new >= (min(Ryd_Lya_K, Ryd_Lya_H)-cut_val_lya)) & (Ryd_x_new <= (max(Ryd_Lya_K, Ryd_Lya_H)+cut_val_lya))
    Ryd_result[mask_Lya] = (Ryd_x_new[mask_Lya] * (h_ev/13.6))

    with open(sed_QSO, "w") as f:
        f.write("#Interpolation QSO SED with masking (W)\n")
        for xi, yi in zip(Ryd_x_new, Ryd_result):
            f.write(f"{xi}\t{yi}\n")
    return sed_QSO

# ---------------------- 메인 실행 ----------------------
def CLOUDY_RUN(Lumin, Column_density_order, multi_factor, metals, idx, extra_option, mode):
    folder_path = os.path.join(
        path, f"Lum_{Lumin}_{idx}/metal_{metals}/N_H_{multi_factor}_{Column_density_order}", f"QSO_{mode}"
    )
    os.makedirs(folder_path, exist_ok=True)

    # 결과 파일 존재 시 스킵 (overwrite=False)
    model_name = "CIV_QSO"
    full_path = os.path.join(folder_path, model_name)
    done_marker = full_path + ".done"
    folder_done_marker = os.path.join(folder_path, ".done")
    if not overwrite and os.path.exists(folder_done_marker):
        print('skip', folder_done_marker)
        return 'Skipped'

    # dlaw 생성
    file_dlaw = "dlaw_HI_exp.ini"
    file_dlaw_full = os.path.join(folder_path, file_dlaw)
    rr = np.logspace(log10(1.0), log10(100.), 101)
    r_e = 50.0
    NHI_total = multi_factor*10**(Column_density_order)
    nn_exp = exp(-rr / r_e)
    NHI_integ = trapezoid(nn_exp, rr) * kpc.cgs.value
    n0 = NHI_total / NHI_integ
    nn_exp = n0 * exp(-rr / r_e)
    rr_pad = 100.* (rr[1]/rr[0])
    nn_pad = n0 * exp(-rr_pad/r_e)
    with open(file_dlaw_full,'w') as fp:
        fp.write("dlaw table\n")
        for r_i,n_i in zip(rr,nn_exp):
            fp.write("continue {:8.5f} {:8.5f}\n".format(log10(r_i*kpc.cgs.value), log10(n_i)))
        fp.write("continue {:8.5f} {:8.5f}\n".format(log10(rr_pad*kpc.cgs.value), log10(nn_pad)))
        fp.write("end of dlaw\n")

    # SED 처리
    sed = os.path.join(path, "QSO.sed")
    sed_QSO = process_sed_wo(sed, folder_path) if mode=="WO" else process_sed_w(sed, folder_path)

    # Cloudy 실행
    options = [
        'sphere','init "dlaw_HI_exp.ini"', f'table SED "QSO.sed"',
        f'nuL(nu) = {Lumin} at 1 Ryd','stop temperature off', f'metals {metals}'
    ]
    if extra_option: options.insert(3, extra_option)

    c_input = pc.CloudyInput(full_path)
    c_input.set_radius(r_in=21.50, r_out=23.50)
    c_input.set_iterate(to_convergence=True)
    c_input.set_other(options)
    c_input.set_emis_tab(emis)
    c_input.print_input()

    pc.config.cloudy_exe = usr_cloudy
    pc.log_.timer('Starting Cloudy', calling=f"Lum_{Lumin}_{idx}/metal_{metals}/N_H_{multi_factor}_{Column_density_order}/{mode}")
    c_input.run_cloudy()
    pc.log_.timer('Cloudy ended after seconds:', calling=f"Lum_{Lumin}_{idx}/metal_{metals}/N_H_{multi_factor}_{Column_density_order}/{mode}")
    with open(done_marker, 'w') as _fp:
        _fp.write('OK\n')
        _fp.write(full_path + '\n')
        _fp.write(folder_path + '\n')
    print('done', done_marker)
    with open(folder_done_marker, 'w') as _ffp:
        _ffp.write('OK\n')
        _ffp.write(folder_path + '\n')
    print('done', folder_done_marker)

    # 완료된 작업을 path_done_list.txt에 누적 기록
    try:
        with open(path_done_list_file, 'a') as _dfp:
            _dfp.write(folder_path + '\n')
        print('append_done', path_done_list_file)
    except Exception as _e:
        print('warn: cannot append to path_done_list', _e)

    return 'Done'

def CLOUDY_RUN_star(args):
    return CLOUDY_RUN(*args)

def _format_seconds(seconds):
    seconds = int(max(0, round(seconds)))
    h = seconds // 3600
    m = (seconds % 3600) // 60
    s = seconds % 60
    return f"{h:02d}:{m:02d}:{s:02d}"

if __name__ == '__main__':
    params=[]
    for Column_density_order in N_H_odrer:
        for mul in multi_factor:
            for Lumin in Lum:
                for metal in metals:
                    for idx, extra_option in enumerate(['', 'no line transfer'], 1):
                        # mode 분리: BOTH이면 WO/W를 나누어 병렬화
                        for m in ["WO","W"] if mode=="BOTH" else [mode]:
                            params.append((Lumin, Column_density_order, mul, metal, idx, extra_option, m))

    # 예정된 모든 작업의 폴더 경로를 path_list.txt에 기록 (중복 제거)
    try:
        folder_paths = []
        for (Lumin, Column_density_order, mul, metal, idx, _extra_option, m) in params:
            folder_path = os.path.join(
                path, f"Lum_{Lumin}_{idx}/metal_{metals[0] if np.size(metals)==1 else metal}/N_H_{mul}_{Column_density_order}", f"QSO_{m}"
            )
            folder_paths.append(os.path.abspath(folder_path))
        unique_paths = sorted(set(folder_paths))
        with open(path_list_file, 'w') as _pf:
            _pf.write('\n'.join(unique_paths) + ('\n' if unique_paths else ''))
        print('write', path_list_file, len(unique_paths))
    except Exception as _e:
        print('warn: cannot write path_list', _e)

    total = len(params)
    num_cpu= 14
    print('total jobs', total)
    t0 = time.time()
    print(f"progress 0/{total} (0.0%) ETA --:--:--", flush=True)
    with Pool(processes=num_cpu) as pool:
        for i, _res in enumerate(pool.imap_unordered(CLOUDY_RUN_star, params), 1):
            pct = 100.0 * i / total
            elapsed = time.time() - t0
            eta_sec = (total - i) * (elapsed / max(1, i))
            if i == 1 or i == total or i % 10 == 0:  # ETA 간격 수정.
                print(f"progress {i}/{total} ({pct:.1f}%) ETA {_format_seconds(eta_sec)}", flush=True)

