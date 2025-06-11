Pro  mp_absred_fixedLed, target, ps=ps, no_mpfit=no_mpfit
;
; README of mp_absred_fixedLed, ps=ps, no_mpfit=no_mpfit
; Version 21.01.2025  
; IDL (GDL) Program :mp_absred_fixedLed
; ----------------------------------------------------------------- 
; Recomputes fits to the absolute Reddening curve with upper grain
; radius computed using mk_rsv2aled
; compile:
; .r mp_absreDgaia.pro
;   
; 1) Externals:
;   a)   ./MPFIT/mpfit.pro : MPFIT IDL fitting package
;   b)    fkt_absredd      : External function of mpfit() -- radii fixed
;   c)   ./a.j             : Executable of jsm2Dpol.f (called by fkt_absredd*pro)
;   d)   ck_abunMgd        : Check element abundance constraints are hold 
;                            and derive Gas/dust mass ratio.
;   e) mk_rsv2aled         : upper radius of dark dust that match Rsv
;
; INPUT: target = star and it will restore file =
; './Result/+target+_para.xdr computed by mk_rsv2aled
;
; Keywords:
;  ps: plot reddening curve and fitto pl_allRedd.pdf
;  no_mpfit: 1=nofit 0=mpfit is used (default)
;
; OUTPUT: 
;   './Result/'+ target+'_Kappa.out'    : Abs, sca cross sections K
;   './Result/'+ target+'_PolKappa.out' : Polarization 
;   './Result/'+ target+'_jsm12fit.inp' : Update of 2.2
;   './Result/'+ target+'_PAH2170.wq'   :
;   './Result/'+ target+'_para.xdr'      : 
;   './Result/'+ target+'_message.out'  : Messages of final a.crossec_dd 
;   idl.ps : PS file showing data and fti of the reddening curve
;
; HD*para.xdr: idl saveset which includes the varoious variables such
; as input and best fit model parameters, the relative reddening curve
; E(w-V)/E(B-V) of the star that shall be fit. (Variables: wdata,
; data, with rms error edata; E(B-V)=ebv of the reddening curve paper
; and Rv=Rv_mod of the model fit (returned by a.crossec_Ebv) and
; reduced chi2 (divided by dot).
;
; Libraries: IDL (GDL start "idl istart" or set:
; !PATH = './AAstron/:' + !PATH
; !PATH = './Coyote/:'  + !PATH
; !PATH = './MPFIT/:'   + !PATH
;
; Example: mp_absred_fixedLed, 'HD287150', /ps
;
; ============================================================
@jismo_noscreen.inc

IF noscreen EQ 0 THEN BEGIN
    set_plot, 'X'
    window, 0, xsize=700, ysize=700
    !p.thick = 2
    !p.charsize = 1.3
    !p.font = 7
ENDIF ELSE BEGIN
    !p.thick = 2
    !p.charsize = 1.3
    !p.font = 0  ; PostScript font
ENDELSE

IF keyword_set(ps) THEN BEGIN
    set_plot, 'PS'
    device, decomposed = 0

    IF noscreen EQ 0 THEN BEGIN
        a = PSWINDOW(/cm)
        device, filename='idl_reddfixed.ps', BITS_PER_PIXEL=8, /color, $
                XSIZE=a.xsize, YSIZE=a.ysize, XOFFSET=a.xoffset, YOFFSET=a.yoffset
    ENDIF ELSE BEGIN
        device, filename='idl_reddfixed.ps', BITS_PER_PIXEL=8, /color, XSIZE=15, YSIZE=10
    ENDELSE
ENDIF
; no_mpfit >0: do not compute fit to reddening curve just plot result
  if not keyword_set(no_mpfit) then      no_mpfit = 0
; ------------------------------------------------------------
; Get input:
  restore, './Result/'+ target+'_para.xdr'
  

  data  = data  * ebv_ref         ; absolute reddening
  edata = edata * ebv_ref        ; absolute reddening
  
; get_lun, iu
; openw,iu,'./Input/jsmTaufit.inp'
; printf, iu, ' tauV, ebv = '
; printf,iu, -data(0)/1.08573, ebv_ref, P_serk
; printf,iu, tauV, ebv_ref, P_serk
; close, iu
; free_lun, iu
; print, -data(0)/1.08573, ebv_ref, P_serk
; print, tauv
; stop, ' bis hier'

 spawn, 'cp ./Result/'+ target+'_jsm12fit.inp  ./Input/jsm12fit.inp'
 spawn, 'cp ./Result/'+ target+'_jsmTaufit.inp  ./Input/jsmTaufit.inp' 
        
readcol, './Input/jsm12fit.inp', /sil, skipline=1, numli=1,  $
          tabuc, tabusi, tabuvsi, tabucvgr, tabucpahs, tqmrn, talec,  talesi, $
         tarad_polmin_aC, tarad_polmin_Si, tarad_polmax, taled

; --------------------------------------------------------------------------------
; parameter check 
    tabuc           = float(tabuc(0)) 
    tabusi          = float(tabusi(0)) 
    tabuvsi         = float(tabuvsi(0)) 
    tabucvgr        = float(tabucvgr(0)) 
    tabucpahs       = float(tabucpahs(0)) 
    tqmrn           = float(tqmrn(0)) 
    talec           = float(talec(0)) 
    talesi          = float(talesi(0)) 
    tarad_polmin_aC = float(tarad_polmin_aC(0)) 
    tarad_polmin_Si = float(tarad_polmin_Si(0)) 
    tarad_polmax    = float(tarad_polmax(0)) 
    taled           = float(taled(0)) 
if abs(tabuc            - float(abuc)          ) gt 1e-3 then stop, 'check para 1' 
if abs(tabusi           - float(abusi)         ) gt 1e-3 then stop, 'check para 2' 
if abs(tabuvsi          - float(abuvsi)        ) gt 1e-3 then stop, 'check para 3' 
if abs(tabucvgr         - float(abucvgr)       ) gt 1e-3 then stop, 'check para 4' 
if abs(tabucpahs        - float(abucpahs)      ) gt 1e-3 then stop, 'check para 5' 
if abs(tqmrn            - float(qmrn)          ) gt 1e-3 then stop, 'check para 6' 
if abs(talec            - float(alec)          ) gt 1e-3 then stop, 'check para 7' 
if abs(talesi           - float(alesi)         ) gt 1e-3 then stop, 'check para 8' 
if abs(tarad_polmin_aC  - float(arad_polmin_aC)) gt 1e-3 then stop, 'check para 9' 
if abs(tarad_polmin_Si  - float(arad_polmin_Si)) gt 1e-3 then stop, 'check para 10' 
if abs(tarad_polmax     - float(arad_polmax)   ) gt 1e-3 then stop, 'check para 11' 
if abs(taled            - float(aled)          ) gt 1e-3 then stop, 'check para 12' 
; --------------------------------------------------------------------------------
 
get_lun, iu
openw,iu,'./Input/jsm12fit.inp'
printf, iu, '  abuc    abusi    avsi   avgr    apah     qmrn  rlec      rlesi     r_pmin_aC r_pmin_Si r_pmax    r_Darkmax '
printf, iu, format='(6f8.3, 6e10.3)', abuc, abusi, abuvsi, abucvgr, abucpahs, qmrn, $
        alec, alesi, arad_polmin_aC, arad_polmin_Si, arad_polmax, aled 
close, iu
free_lun, iu


   

 print
 print, ' ============= START target    =   ', target, '      ================ '
 print, ' Input start parameters of dust model used by a.j'
 print, ' abuc, abusi, abuvsi, abucvgr, abucpahs, qmrn, alec, alesi, arad_polmin_aC, arad_polmin_Si, arad_polmax, aled, x0, gam' 
 print, format='(6f8.3, 8e10.3)', abuc, abusi, abuvsi, abucvgr, abucpahs, qmrn, $
        alec, alesi, arad_polmin_aC, arad_polmin_Si, arad_polmax, aled, x0, gam


 
  xtol        = 1d-4
  para        = [abuc(0), abuvsi(0), abucvgr(0), abucpahs(0), qmrn(0),    $
                 alec(0), alesi(0), arad_polmin_aC(0), arad_polmin_Si(0), $
                 x0, gam]  * 1.d0

delvar, parain
parain = replicate({fixed:0, limited:[1,1], limits:[0.D,0.D],mpside:2},11)
parain(0).limits(0) =   1.d0
parain(0).limits(1) = 160.0d0
parain(1).limits(0) =   1.0d
parain(1).limits(1) =  20.0d0
parain(2).limits(0) =   1.0d-2
parain(2).limits(1) =  40.0d0
parain(3).limits(0) =   1.0d-2
parain(3).limits(1) =  40.0d0
parain(4).limits(0) =   2.d0         ; qmrn
parain(4).limits(1) =   4.d0         ; 
parain(5).limits(0) =  6d-7    ; alec
parain(5).limits(1) =  2.6d-5
parain(6).limits(0) =  6d-7    ; alesi
parain(6).limits(1) =  2.6d-5  ; 
parain(7).limits(0) =  1.d-6          ; arad_polmin_aC
parain(7).limits(1) =  2.6d-5  ; 
parain(8).limits(0) =  1.d-6          ; arad_polmin_Si
parain(8).limits(1) =  2.6d-5  ; 
parain(9).limits(0) =  4.3           ; PAH x0
parain(9).limits(1) =  4.9
parain(10).limits(0)=  0.5           ; PAH gam
parain(10).limits(1)=  2.0

; Relative dust abundances are treated so that we can fix one of them
; and set abuSi :=15ppm.
 
print, '  MP2: free: abundance+qmrn, fixed: radii         '
print

   parain.fixed      = 0
   parain(5:8).fixed = 1
   save, filename='./Input/tmp_parain.xdr', parain

  if no_mpfit ge 1 then begin
     res = para
  endif else begin
   stat = -1 
   delvar, res

   res  = MPFITFUN('fkt_absredd', wdata, data, edata, $
                 para, PARINFO=parain, perror=rms_para, status=stat,    $
                 yfit=reddfit, bestnorm=chi2, dof=dof, xtol=xtol)
   chi2redd  = chi2/dof         ; reduced chi2
   para           = res
   
   rmsabuc           = rms_para(0)
   rmsabuvsi         = rms_para(1)
   rmsabucvgr        = rms_para(2)
   rmsabucpahs       = rms_para(3)
   rmsqmrn           = rms_para(4)
   rmsalec           = rms_para(5)
   rmsalesi          = rms_para(6)
   rmsarad_polmin_aC = rms_para(7)
   rmsarad_polmin_Si = rms_para(8)
   rmsx0             = rms_para(9)
   rmsgam            = rms_para(10)   
  endelse
  
   para(0)        = ((para(0) > parain(0).limits(0)) < parain(0).limits(1)) 
   para(1)        = ((para(1) > parain(1).limits(0)) < parain(1).limits(1)) 
   para(2)        = ((para(2) > parain(2).limits(0)) < parain(2).limits(1)) 
   para(3)        = ((para(3) > parain(3).limits(0)) < parain(3).limits(1)) 
   para(4)        = ((para(4) > parain(4).limits(0)) < parain(4).limits(1))
   para(5)        = ((para(5) > parain(5).limits(0)) < parain(5).limits(1))
   para(6)        = ((para(6) > parain(6).limits(0)) < parain(6).limits(1)) 
   para(7)        = ((para(7) > parain(7).limits(0)) < parain(7).limits(1))
   para(8)        = ((para(8) > parain(8).limits(0)) < parain(8).limits(1)) 
   para(7)        =   para(7) < para(5)
   para(8)        =   para(8) < para(6)
   para(9)        = ((para(9) > parain( 9).limits(0)) < parain( 9).limits(1))
   para(10)       = ((para(10)> parain(10).limits(0)) < parain(10).limits(1)) 

   abuc           =   para(0)
   abusi          =   abusi(0)*1d0
   abuvsi         =   para(1)
   abucvgr        =   para(2)
   abucpahs        =  para(3)
   qmrn           =   para(4)
   alec           =   para(5)
   alesi          =   para(6)
   arad_polmin_aC =   para(7)
   arad_polmin_Si =   para(8)   
   x0             =   para(9)
   gam            =   para(10)
   

 
get_lun, iu
openw,iu,'./Input/jsm12fit.inp'
printf, iu, '  abuc    abusi    avsi   avgr    apah     qmrn  rlec      rlesi     r_pmin_aC r_pmin_Si r_pmax    r_Darkmax '
printf, iu, format='(6f8.3, 6e10.3)', abuc, abusi, abuvsi, abucvgr, abucpahs, qmrn, $
        alec, alesi, arad_polmin_aC, arad_polmin_Si, arad_polmax, aled 
close, iu
free_lun, iu







; use org Drude parameters not those form redd fit !
   x0org = x0 & gamOrg = gam
   get_lun, iu
   openw,iu,'./Input/PAH2170.wq'
   printf, iu, 'x0 (mu)    gam : Drude Parameters of PAH at 2175AA bump'
   printf,iu, x0, gam          ; change if ext papra shall be used
   printf,iu, x0org, gamorg    ; NOT used. default parameters or as in Gordon+09 
   close, iu
   free_lun, iu
   

  print, format='(6f8.3, 8e10.3)', abuc, abusi, abuvsi, abucvgr, abucpahs, qmrn, $
        alec, alesi, arad_polmin_aC, arad_polmin_Si, arad_polmax, aled, x0, gam

  
 print
 print, ' MPfit is done for star = ', target
 print
 chi2redd  = chi2/dof           ; reduced chi2
  res     = -9
 spawn,   './a.j >  message.out'
 spawn, 'grep Mass_mrn message.out', res 
 Mmrn     = float(strmid(res, 43,9))
 Mdark    = float(strmid(res, 55,9))
 fdark    = float(strmid(res, 67,9))
 Mmrn     = Mmrn(0) & Mdark=Mdark(0) & fdark = fdark(0)  
 print, 'star: Mmrn, Mdark, fdark'
 print, format='(a10,3f7.2)', target, Mmrn, Mdark, fdark
 

 
; Save MPFIT results and readig Kappa.out
rdfloat, './Output/Kappa.out', skipline=3, w, sac, ssac, sasi, $
        sssi, savgr, ssvgr, savsi, ssvsi, sapah, sad, ssd, st, /sil
print, ' in xdr save file =', './Result/'+ target+'_para.xdr' 
print, ' relative reddening E(x-V)/Ebv is saved'
       data =  data / ebv_ref           ; save relative reddening
      edata = edata / ebv_ref           ; save relative reddening

print, 'no rpp, oeRsv yet'
; no rpp, oeRsv yet
     save, /xdr, filename='./Result/'+ target+'_para.xdr', $ 
  target, kvis, kblue, $ 
  abuc, abusi,  abuvsi,    abucvgr,    abucpahs,  acd, asid, actd, asiTD, Mgd, $
  qmrn, alec, alesi, aled,  arad_polmin_aC, arad_polmin_Si, arad_polmax, x0, gam, $
  Mmrn, Mdark, fdark, tauV, Ebv_ref,  Ebv_mod, Vmag, Mv, Dlum, $
  wredd_org, redd_org, eredd_org,  wdata, data, edata, areddata, areddmod, $
  w, sac, ssac, sasi, sssi, savgr, ssvgr, savsi, ssvsi, sapah, sad, ssd, st, $
 wpd, pdata, epd, scale_P, Omega, pm, pmax, P_serk, rsv, rpp, dRsv, $ 
 oI850, op850, oep850, oRsv, oRpp, oeRsv,  oeRpp, $
      chi2Rsv, chi2redd, chi2amin, chi2pmin


; ---------------------- check  abundances and Mgas/Mdust ----------------------    
; check abundance constraints [C]/[Si] < 5.25
 if  abuc gt 5.25*(abusi + abuvsi) -abucvgr - abucpahs then stop, 'abundance constraint not ok after  mpfit'


 ck_abunMgd, target
 print, 'oeRsv, rpp:', oeRsv, rpp 
 ; SAVE final parameters 
 
 restore, './Result/'+ target+'_para.xdr'



save, /xdr, filename='./Result/'+ target+'_para.xdr', $
 target, kvis, kblue, $ 
 abuc, abusi,  abuvsi,    abucvgr,    abucpahs,  acd, asid, actd, asiTD, Mgd, $
 qmrn, alec, alesi, aled,  arad_polmin_aC, arad_polmin_Si, arad_polmax, x0, gam, $
 Mmrn, Mdark, fdark, tauV, Ebv_ref,  Ebv_mod, Vmag, Mv, Dlum, $
 wredd_org, redd_org, eredd_org,  wdata, data, edata, areddata, areddmod, $
 w, sac, ssac, sasi, sssi, savgr, ssvgr, savsi, ssvsi, sapah, sad, ssd, st, $
 wpd, pdata, epd, scale_P, Omega, pm, pmax, P_serk, rsv, rpp, dRsv, $ 
 oI850, op850, oep850, oRsv, oRpp, oeRsv,  oeRpp, $
      chi2Rsv, chi2redd, chi2amin, chi2pmin
      

   
; -------------------------  PLOT and save  final fit result ---------
 data = data * ebv_ref           ; absulte reddening
edata = edata * ebv_ref          ; absulte reddening

     
 res     = -9
 spawn,   './a.j | tee message.out'
 spawn, 'grep Mass_mrn message.out', res 
 Mmrn     = float(strmid(res, 43,9))
 Mdark    = float(strmid(res, 55,9))
 fdark    = float(strmid(res, 67,9))
 Mmrn     = Mmrn(0) & Mdark=Mdark(0) & fdark = fdark(0)  & 
 print, 'star: Mmrn, Mdark, fdark'
 print, target, Mmrn, Mdark, fdark


; Kappa.out
rdfloat, './Output/Kappa.out', skipline=3, w, sac, ssac, sasi, $
        sssi, savgr, ssvgr, savsi, ssvsi, sapah, sad, ssd, st, /sil
  w   = w*1e4
  kvis  = (where(w le 0.548))(0) & kblue  = (where(w le 0.445))(0)
  Rv_mod      = st(kvis) / (st(kblue) - st(kvis))
 Ebv_mod      = 2.5/alog(10.) * (st(kblue) - st(kvis))
  Av_mod      = Rv_mod * Ebv_mod
 absredd_mod  = 2.5/alog(10.) * (st/st(kvis) -1.) * st(kvis) 

 print, ' w(kvis), w(kblue)     : ',  w(kvis), w(kblue)
 print, ' Ebv_ref, Ebv_mod, Rv_mod : ', ebv_ref, ebv_mod, Rv_mod


    salec  = strmid(string(nint(alec*1e7)),  5, 7) 
    salesi = strmid(string(nint(alesi*1e7)), 5, 7)
    saled  = strmid(string(nint(aled*1e7)),  4, 7) 
    spc    = strmid(string(nint(arad_polmin_aC*1e7)), 5, 7) 
    spsi   = strmid(string(nint(arad_polmin_si*1e7)), 5, 7) 

    tit    = 'c2=' + strmid(string(nint(100*chi2redd)/100.),4,4)   + $
             ' R!DC!N= '  + salec+ ' R!DSi!N= ' + salesi + ' R!DD!N= ' + saled 
    
    yrange = [min([data-2.*edata])-0.2, max(data+edata)+0.2]

    ; IF noscreen EQ 0 THEN BEGIN
    if keyword_set(ps) then begin
        plot, 1./w, absredd_mod, xrange=[-0.1,11.1], yrange=yrange, /xsty, /ysty, $
        xthick=4, ythick=4, /nodata
    
    oplot, 1./w, ((sac+ssac + sasi+sssi)/st(kvis)   -1.)* Av_mod, color=fsc_color('brown', /NOWIDGET)
    oplot, 1./w, ((savsi+ssvsi+ sapah+savgr+ssvgr)/st(kvis) -1.)* Av_mod, color=fsc_color('forest green', /NOWIDGET)
    oplot, 1./w, ((sad+ssd)/st(kvis)     -1.)            * Av_mod, thick=!P.thick+1
    oplot, 1./w, absredd_mod, thick=4, color=fsc_color('magenta', /NOWIDGET)



    oploterror, 1./wdata, data, wdata*0, 0.1*abs(data) > 0.05, /nohat, $
                color=fsc_color('dark gray', /NOWIDGET), psym=3
    oplot, 1./wdata(1:*), data(1:*),  psym=plotsym(/circle,scale=1.01,color=fsc_color('gray', /NOWIDGET), /fill)
    oplot, 1./wdata(1:*), data(1:*),  psym=plotsym(/circle,scale=1.01)
    ; model if tauV from Dgaia:
    oplot, 1./wdata(0) *[1,1], [data(0), data(0)],  psym=plotsym(/box,scale=1.09,color=fsc_color('Charcoal', /NOWIDGET), /fill)
    oplot, 1./wdata(0) *[1,1], [data(0), data(0)],  psym=plotsym(/box,scale=1.1)

    
    oplot, 1./w, absredd_mod, thick=1, color=fsc_color('magenta', /NOWIDGET)
    xyouts, 1, max([data*0.85]), target, charsize=1.5
      endif else if noscreen EQ 0 then begin

          plot, 1./w, absredd_mod, xrange=[-0.1,11.1], yrange=yrange, /xsty, /ysty, $
              xthick=4, ythick=4, /nodata
          
          oplot, 1./w, ((sac+ssac + sasi+sssi)/st(kvis)   -1.)* Av_mod, color=fsc_color('brown', /NOWIDGET)
          oplot, 1./w, ((savsi+ssvsi+ sapah+savgr+ssvgr)/st(kvis) -1.)* Av_mod, color=fsc_color('forest green', /NOWIDGET)
          oplot, 1./w, ((sad+ssd)/st(kvis)     -1.)            * Av_mod, thick=!P.thick+1
          oplot, 1./w, absredd_mod, thick=4, color=fsc_color('magenta', /NOWIDGET)
      
      
      
          oploterror, 1./wdata, data, wdata*0, 0.1*abs(data) > 0.05, /nohat, $
                      color=fsc_color('dark gray', /NOWIDGET), psym=3
          oplot, 1./wdata(1:*), data(1:*),  psym=plotsym(/circle,scale=1.01,color=fsc_color('gray', /NOWIDGET), /fill)
          oplot, 1./wdata(1:*), data(1:*),  psym=plotsym(/circle,scale=1.01)
          ; model if tauV from Dgaia:
          oplot, 1./wdata(0) *[1,1], [data(0), data(0)],  psym=plotsym(/box,scale=1.09,color=fsc_color('Charcoal', /NOWIDGET), /fill)
          oplot, 1./wdata(0) *[1,1], [data(0), data(0)],  psym=plotsym(/box,scale=1.1)
      
          
          oplot, 1./w, absredd_mod, thick=1, color=fsc_color('magenta', /NOWIDGET)
          xyouts, 1, max([data*0.85]), target, charsize=1.5
      endif
    

    ; ENDIF

 
; ======================
; Save Fit results 
; =================



print, ' in xdr save file =', './Result/'+ target+'_para.xdr' 
print, ' relative reddening E(x-V)/Ebv is saved'

  data =  data / ebv_ref           ; saved relative reddening
 edata = edata / ebv_ref           ; saved relative reddening

 
 spawn, 'cp ./Output/Kappa.out    ./Result/'+ target+'_Kappa.out'
 spawn, 'cp ./Output/PolKappa.out ./Result/'+ target+'_PolKappa.out'
 spawn, 'cp ./Input/jsm12fit.inp  ./Result/'+ target+'_jsm12fit.inp'
 spawn, 'cp ./Input/PAH2170.wq    ./Result/'+ target+'_PAH2170.wq'
 spawn, 'mv message.out           ./Result/'+ target+'_message.out'

   
 IF keyword_set(ps) THEN BEGIN
    device, /close  ; Always close PS device
  
    IF noscreen EQ 0 THEN BEGIN
      set_plot, 'X'
      device, decomposed=1
    ENDIF
  
    print, ' Result of reddening fit in plot :  pl_allReddfixed.pdf'
    com = 'ps2pdf idl_reddfixed.ps pl_allReddfixed.pdf'
    spawn, com
    print, com + '     ->      DONE'
  ENDIF
  
  close, /all
  
  print
  print, ' mp_absred_fixeddLed:        DONE '
  print, ' ========================== '
  print
  END
