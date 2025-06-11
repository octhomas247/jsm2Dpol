pro get_bestchi2pol, target, ps=ps
;
@jismo_noscreen.inc
; writes best model fit for each star: tauV Ndrk Oma Pmax dP Rsv
; oRsv+/-err ksig c2Rsv c2p c2ap c2redd Model'

; -------------------------------------
print

; ------------- Output in ./ResultRsv/<target>_model.info and ./ResultRsv/<target>_model.pdf

print
print, '   *** Processing model: '
spawn, 'pwd', model
model = strmid(model,46,13)
print, '              ', model
print


   get_lun, iuf
   openw,  iuf, './ResultRsv/'+target+'_model.info'
   printf, iuf, '# Star    aled  tauV  Ndrk Oma  Pmax   dP  Rsv   oRsv+/-err  Rpp   oRpp+/-er   c2Rpp c2Rsv c2p   c2ap c2redd  Model'
   
;
; ------------- Input ------------------------------------------
;
;check files


fxdr   = findfile('./Result/' +target+'_para.xdr', cou=nfx)

finp  = findfile('./Result/' +target+'_jsm12fit.inp', cou=nfin)
ftau  = findfile('./Result/' +target+'_jsmTaufit.inp'   , cou=nft)

fpah  = findfile('./Result/' +target+'_PAH2170.wq'  , cou=nfpah)
finf  = findfile('./Result/' +target+'_chi2p.info'   , cou=nfi)
fchi2 = findfile('./Result/' +target+'_chi2p.out'   , cou=nfc)
if nfc ne 1 or nfin ne 1 or nfx ne 1 or nfin ne 1 or nft ne 1  then $
   print, ' check all files like fchi2 = :', fchi2

fxdr    = fxdr(0)
finp    = finp(0)
ftau    = ftau(0)
fpah    = fpah(0)
finf    = finf(0)
fchi2   = fchi2(0)


;   spawn, 'cp ' +finp  + ' ./Input/jsm12fit.inp' 
   spawn, 'cp ' +fpah  + ' ./Input/PAH2170.wq'
 
;print, format='(a)', fxdr, fpah(0), finp(0), fchi2, finf, fpol



; convert to absolute reddening curve E(w) and E(oo) < E(H,K) and
; recompute tauv accordingly
   restore, fxdr

   data    = data    * ebv_ref
   edata   = edata   * ebv_ref   
   data(0) = data(0)  < min([data(1:2)])
   tauv    = - data(0) * alog(10.)/2.5
   if abs(tauv  +data(0) * alog(10.)/2.5)  gt 0.001 then $
      stop, '   *** tauV check : tauv, - data(0) * alog(10.)/2.5 ', tauv, - data(0) * alog(10.)/2.5
   if tauv    lt 0  then stop, 'tauv < 0 check Dgaia for ', target


; Pdata:   
 npd  = n_elements(pdata)

; Copy to input file jsmTaufit.inp
 spawn, 'cp ' +ftau + ' ./Input/jsmTaufit.inp'
 
print, format='(a10,a14, f5.2,a14, f5.2, a9, f6.1, a9, f5.2, a5, f5.1,a4, f4.1, a5, f5.1,a4, f4.1)', target, $
        '  E(B-V)_ref= ', ebv_ref, ' P_serk= ', P_serk, ' fdark=', fdark,  '  tauv= ', tauV,$
        ' oRsv= ', orsv, ' +/-', oersv, ' oRpp= ', orpp, '+/-', oerpp

; --------------------------------------------------------------------------------------------
; Selec best model from file fchi2 = <target>_chi2p.ot using after
; outlier rejection the min chi2_p


readcol, fchi2, skipl=1, format='(a,f,f,f, f,f,f, f)',  StarChi2, apmin_aC, apmin_Si, Omega, vPmax, vrsv, chi2p, chi2a


; Rsv is fit within ksig 
                             ksig = 1.
c2a_med = 99.
c2p_med = 90.



; adjust aled with mk_Rsv2aled.pro
if aled lt 3.d-4 then begin
   if target eq 'HD037903' or $
      target eq 'HD038023' or $
      target eq 'HD091824' or $
      target eq 'HD092044' or $
;      target eq 'HD108927' or $
      target eq 'HD152245' or $
      target eq 'HD294304' or $
      target eq 'HD315021' or $
      target eq 'HD315023' or $
      target eq 'HD315032' then chi2a(*) = c2a_med 
endif



ii      = where(Omega lt 89. and chi2a le c2a_med and chi2p le c2p_med, nn)


print
print, ' best pol fit at start:   min(chi2p(ii))  =', min(chi2p(ii))
if nn lt 1 then begin
;   ii       = (where(chi2p le min(chi2p), nn))(0)
endif else begin
 for j =0, 9 do begin
  ii      = where(Omega le 89. and chi2a le c2a_med and chi2p le c2p_med, nn)
  if nn ge 3 then begin   
   c2a_med  = median(chi2a(ii))
   c2p_med  = median(chi2p(ii)) 
   print, j, nn, min(chi2p(ii)), c2p_med, min(chi2a(ii)), c2a_med
  endif
 endfor
endelse


; final selection:
if nn le 0 then c2p_min  = min(chi2p) else c2p_min  = min(chi2p)

ibest      = where(Omega le 89. and chi2a le c2a_med and chi2p le c2p_min, nn)
if nn lt 1 then ibest      = where(chi2p le c2p_min, nn)
ibest = ibest(0)

print, ' best pol fit at end:   min(chi2p(ii))  =', min(chi2p(ibest))
print


; ---------------------------------------------------
; Recompute and plot best model and plot
arad_polmin_aC = apmin_aC(ibest)*1e-7
arad_polmin_Si = apmin_Si(ibest)*1e-7

   
 get_lun, iu
  openw,iu,'./Input/jsm12fit.inp'
  printf, iu, '  abuc    abusi     abuvsi abucvgr abucpahs qmrn   alec       alesi      apolmin_aC apolmin_Si apolmax    aDarkmax'
  printf, iu, format='(6f8.3,6e11.3)', abuc, abusi, abuvsi, abucvgr, abucpahs, qmrn, alec, alesi, $ 
  arad_polmin_aC, arad_polmin_Si, arad_polmax, aled
  close, iu
 free_lun, iu

 spawn,   './a.j > message.out'



rdfloat, './Output/Kappa.out', skipline=3, w, sac, ssac, sasi, $
        sssi, savgr, ssvgr, savsi, ssvsi, sapah, sad, ssd, st, /sil
  w   = w*1e4
  kvis  = (where(w le 0.548))(0) & kblue  = (where(w le 0.445))(0)
  Rv_mod      = st(kvis) / (st(kblue) - st(kvis))
 Ebv_mod      = 2.5/alog(10.) * (st(kblue) - st(kvis))
  Av_mod      = Rv_mod * Ebv_mod
 absredd_mod  = 2.5/alog(10.) * (st/st(kvis) -1.) * st(kvis)

; abs redd data:
areddmod = interpol(absredd_mod,  w, wdata)


nr = n_elements(areddata)

chi2redd = total((areddmod-areddata)^2/edata^2)/ (nr -1. -8.) 



; -----------------
 

rdfloat, './Output/PolKappa.out', skipline=3,  wpol,spc, spsi,spd,stm, /sil
wpol  = wpol *1e4
spcd  = interpol(spc,  wpol, wpd)
spsid = interpol(spsi, wpol, wpd)
spdd  = interpol(spd,  wpol, wpd)

; convert to % ; use to get B field direction, d.Q* computed for Omega=60deg
; scale_P < 1.333 because Omega <90deg:
 pm      = (spcd + spsid + spdd) * 100.
 pmax    = max(pm)
 scale_P = P_Serk/Pmax < 1.333333
 sOmega  = sqrt(scale_P*1d0) * sin(60./180.d0 *!pi)

  Omega  = asin(sOmega) * 180./!pi
  pm     = pm * scale_p
  pmax   = max(pm)

; polarized emisson:
readcol, './Output/emipol.out', skipline=2, wemi, emis, emip_t, emip_c, emip_Si, emip_d, /sil
k850   = min(where(wemi le 850.))          ; & print, wemi(k850)
p850   = emip_t(k850)/emis(k850)   * 100.  * scale_p

Rsv    = p850 /(P_serk/tauv)
rpp    = p850 *  oI850/P_serk






; Rsv + pol fit

chi2pmin  = total((pdata - pm)^2 / epd^2) / (npd-1.-2.)


chi2amin  = total(([pdata, replicate(orsv,npd)] - [pm, replicate(rsv,npd)])^2 $
               / [epd,replicate(oersv, npd)]^2) / (2.*npd-1.-2.)


chi2Rsv  = (Rsv-orsv)^2/oersv^2
chi2Rpp  = (Rpp-orpp)^2/oerpp^2
   



print, ' E(x-V)/Ebv is saved in fxdr= ', './ResultRsv/'+ target+'_para.xdr'

 data =  data / ebv_ref         ; saved relatkive reddening
edata = edata / ebv_ref         ; saved relatkive reddening

save, /xdr, filename='./ResultRsv/'+ target+'_para.xdr', $ 
 target, kvis, kblue, $ 
 abuc, abusi,  abuvsi,    abucvgr,    abucpahs,  acd, asid, actd, asiTD, Mgd, $
 qmrn, alec, alesi, aled,  arad_polmin_aC, arad_polmin_Si, arad_polmax, x0, gam, $
 Mmrn, Mdark, fdark, tauV, Ebv_ref,  Ebv_mod, Vmag, Mv, Dlum, $
 wredd_org, redd_org, eredd_org,  wdata, data, edata, areddata, areddmod, $
 w, sac, ssac, sasi, sssi, savgr, ssvgr, savsi, ssvsi, sapah, sad, ssd, st, $
 wpd, pdata, epd, scale_P, Omega, pm, pmax, P_serk, rsv, rpp, dRsv, $ 
 oI850, op850, oep850, oRsv, oRpp, oeRsv,  oeRpp, $
      chi2Rsv, chi2Rpp, chi2redd, chi2amin, chi2pmin, scale_p


    spawn, 'cp ./Output/emipol.out    ./ResultRsv/'+ target+'_emipol.out'    
    spawn, 'cp ./Output/emis.out      ./ResultRsv/'+ target+'_emis.out'
    spawn, 'cp ./Output/Kappa.out     ./ResultRsv/'+ target+'_Kappa.out'
    spawn, 'cp ./Output/PolKappa.out  ./ResultRsv/'+ target+'_PolKappa.out'
    spawn, 'cp ./Input/jsm12fit.inp   ./ResultRsv/'+ target+'_jsm12fit.inp'
    spawn, 'cp ./Input/PAH2170.wq     ./ResultRsv/'+ target+'_PAH2170.wq'
    spawn, 'mv message.out            ./ResultRsv/'+ target+'_message.out'

    spawn, 'cp ./ResultRsv/'+target+'*.* ./Result/'
    print, '# Star   aled  tauV  Ndrk Oma  Pmax   dP  Rsv   oRsv+/-err  Rpp   oRpp+/-er  c2Rsv c2Rpp c2p   c2ap  c2redd  Model'    
    print, format='(a8, i6,f6.2, i5, i5, f6.2, i5, 11f6.2, 1x, a15)', target, nint(aled*1e7), tauv, nint(100*Mdark/(Mmrn+Mdark)), $
            nint(Omega), Pmax, nint(abs(Pmax/P_Serk-1.)*100.), Rsv, orsv, oersv, Rpp, orpp, oerpp, $
            chi2Rsv<99.,  chi2Rpp<99., chi2pmin<99., chi2amin<99., chi2redd<99., model
    
    printf, iuf, format='(a8, i6,f6.2, i5, i5, f6.2, i5, 11f6.2, 1x, a15)', target, nint(aled*1e7), tauv, nint(100*Mdark/(Mmrn+Mdark)), $
            nint(Omega), Pmax, nint(abs(Pmax/P_Serk-1.)*100.), Rsv, orsv, oersv, Rpp, orpp, oerpp, $
            chi2Rsv<99.,  chi2Rpp<99., chi2pmin<99., chi2amin<99., chi2redd<99., model
 print
 print, ' ----------------------------------------------------------- '
 print


 IF keyword_set(ps) THEN BEGIN
    setps, 'idl_bestchi2pol.ps'
  ENDIF
  
  IF noscreen EQ 0 THEN BEGIN
    set_plot, 'X'
    window, 0, xsize=500, ysize=900
    !p.multi = [0, 1, 2, 0]
    !p.font = 0
    !p.thick = 2
    !p.charsize = 1.5
  ENDIF
 
   title= ' aC=' + strmid(string(nint(arad_polmin_aC*1e7)), 5,3) + $
          ' Si=' + strmid(string(nint(arad_polmin_Si*1e7)), 5,3) + $
          '  P=' + strmid(string(nint(100*pmax)/100.),4,5)       + $
          ' Rsv='+strmid(string(nint(100*rsv)/100.),5,4) +' oRsv=' $
         + strmid(string(nint(100*orsv)/100.),5,4)

         !p.thick = 2
         !p.charsize = 1.3
         !P.font = 0  ; Use 0 (PostScript font) for PS, or 7 for screen
         
         IF noscreen EQ 0 THEN BEGIN
             set_plot, 'X'
             !P.font = 7  ; override for screen mode
             window, 0, xsize=700, ysize=700
         ENDIF

    IF keyword_set(ps) THEN BEGIN
        !P.font = 0
      
        IF noscreen EQ 0 THEN BEGIN
          a = PSWINDOW(/cm)
          set_plot, 'PS'
          device, decomposed=0
          device, filename='idl_bestchi2pol.ps', BITS_PER_PIXEL=8, /color, $
                  XSIZE=a.xsize, YSIZE=a.ysize, XOFFSET=a.xoffset, YOFFSET=a.yoffset
        ENDIF ELSE BEGIN
          set_plot, 'PS'
          device, decomposed=0
          device, filename='idl_bestchi2pol.ps', BITS_PER_PIXEL=8, /color, XSIZE=15, YSIZE=10
        ENDELSE
      ENDIF

    ; ----- Plotting section -----
    ploterror, 1./wdata, areddata, edata*ebv_ref,  xr=[-0.01,11], /xsty, /ysty, $
        xtitle='!9l !3(!9m!3m)', ytitle='abs redd E(w-V)', xthick=2, ythick=2, psym=3, $
        title= target + ' chi2a= ' + strmid(string(nint(100*chi2amin)/100.),5,4)

        
    oplot, 1./w, absredd_mod, color=fsc_color('magenta', /NOWIDGET), thick=2

    ploterror, wpd, pdata, epd, xr=[min(wpd)-0.01, max(wpd)+0.01], $
        yr=[min(pdata-epd)*0.9 > 0., max(pdata+epd)*1.05], /xsty, /ysty, $
        xtitle='!9l !3(!9m!3m)', ytitle='p (%)', xthick=2, ythick=2, title=title, psym=3

    oplot, wpd, pm, color=fsc_color('magenta', /NOWIDGET), thick=4

    ; ----- Convert PostScript to PDF -----
    IF keyword_set(ps) THEN BEGIN
    device, /close
    PRINT, ' Result of reddening fit in plot :  pl_allReddbestchi2pol.pdf'
    SPAWN, 'ps2pdf idl_bestchi2pol.ps pl_allReddbestchi2pol.pdf'
    SPAWN, 'cp pl_allReddbestchi2pol.pdf ./ResultRsv/'+target+'_model.pdf'
    PRINT, ' *** Done: Results in ./ResultRsv/'+target+'_model.pdf ./ResultRsv/'+target+'_model.info for Model = ', model
    ENDIF ELSE BEGIN
    PRINT, ' *** Done: NO PLOT  Results in ./ResultRsv/'+target+'_model.info for Model = ', model
    ENDELSE

    ; Restore GUI device if needed
    IF noscreen EQ 0 THEN BEGIN
    set_plot, 'X'
    device, decomposed=1
    !p.multi = 0
    ENDIF

end

