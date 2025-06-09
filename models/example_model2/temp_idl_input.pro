; ==== JISMO startup ====
COMMON jismo_flags, noscreen
noscreen = 0
noscreen = 1
; ----- Set environment -----
!PATH = '../../IDL/AAstron/:' + !PATH
!PATH = '../../IDL/Coyote/:'  + !PATH
!PATH = '../../IDL/COSMOS/:'  + !PATH 
!PATH = '../../IDL/MPFIT/:'   + !PATH
!PATH = '../../IDL/:'         + !PATH

; ----- Compile all needed procedures before any call -----
.r alias
.r mp_absreDgaia
.r get_pol2xdr
.r mk_chi2pol
.r get_bestchi2pol
.r mk_rsv2aled
.r mp_absred_fixedLed
.r fit_target

; ----- Run actual procedure -----
fit_target, 'HD287150'
