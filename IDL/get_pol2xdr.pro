pro get_pol2xdr, target, mRsv = mRsv
  
;
; -------------------------------------------------------------------
; For given target reads fxdr save set ./Result/'+target+'_para.xdr'
; computed with mpabsDgaia.pro and reads polarisation data form file
; fpol and polarisation parameters: P_Serk from ../../Data/mk_Serk.xdr
; and polarisation efficiency ratio orsv =p850/P_serk *tauv with error
; eorsv from ../../Data/mk_ForsPlanck.tab.
;
; Output: updates fxdr file with P_serl orsv,eorsv and writes files
; ./Input/jsmTaufit.inp and ./Result/'+ target+'_jsmTaufit.inp'
;
; Keyword: mRsv -take Rsv= P850/Pserk*tauV = 4.31+/-0.04 mean value by
; Vincent Guillet et al. A&A 641, A12 (2020),
; https://doi.org/10.1051/0004-6361/201833885 if keyword not set then
; reads from file: '../../Data/mk_ForsPlanck.tab'
; --------------------------------------------------------------

  fxdr = findfile('./Result/'+target+'_para.xdr', cou=nfx)
  fpol  = findfile('../../Data/DataRsv/'+target+'.pol'   , cou=nfp)
  
  if nfx ne 1 or nfp ne 1 then print, 'no/too many fxdr, fpol files check : ', nfx, fxdr
  
  fxdr = fxdr(0)
  fpol = fpol(0)
  


      restore, fxdr
      ARAD_POLMAX    = ARAD_POLMAX(0)*1d0    < aled
      
; convert to absolute reddening curve E(w) and E(oo) < E(H,K) and
; recompute tauv accordingly
   data    = data    * ebv_ref
   edata   = edata   * ebv_ref   
   data(0) = data(0)  < min([data(1:2)])
   tauv    = - data(0) * alog(10.)/2.5
   if abs(tauv  +data(0) * alog(10.)/2.5)  gt 0.001 then $
   stop, '   *** tauV check : tauv, - data(0) * alog(10.)/2.5 ', tauv, - data(0) * alog(10.)/2.5
   if tauv    lt 0  then stop, 'tauv < 0 check Dgaia for ', target
   

  restore, '../../Data/mk_Serk.xdr'
  star_Serk = star
  ii     = where(target eq star_serk, nserk) & if nserk ne 1 then $ 
                                  stop, ' no Serk fit for ', star(l)
  P_serk = V_P_MAX(ii(0))


; reads chi2 of Rsv for min alignement radii and gets's the best fit.
; o = observation

  drpp = -999.
  drsv = -999.
  
  if not keyword_set(mRsv) then begin
   readcol, '../../Data/mk_ForsPlanck.tab', $
         skipl=9, format='(a, f,f,f, f,f,f, f,f,f, f,f,f)', $
         starin, galb, flip, op_max, opv, oepV, otauV, oav850, $
         obsI850, obsp850, obsep850, obsRsv, obseRsv, obsRpp, obseRpp

   l  = (where(target eq starin, nfp))(0)
   oI850   = obsI850(l)
   op850   = obsp850(l)
   oep850  = obsep850(l)
   oRsv    = obsRsv(l)
   oeRsv   = obseRsv(l)
   oRpp    = obsRpp(l)
   oeRpp   = obseRpp(l)
   
   print, 'target, oI850,  op850, oep850, oRsv, oeRsv, oRpp, oeRpp'
   print, target, oI850,  op850, oep850, oRsv, oeRsv, oRpp, oeRpp
   
   
endif else begin
   oI850  = 1.
   op850  = 1.
   oep850 = -9.
   oRsv   = 4.31
   oRpp   = 5.42
   oeRsv  = 0.04
   oeRpp  = 0.05
endelse

  
; Pdata:   
 readcol, fpol,  wpd, Int, dInt, Qint, dQint, nQ, Uint, dUint, nU, $
          Pdata, epd,theta, dtheta,  skip=3, /sil
 npd  = n_elements(pdata)
 pmax = max(pdata) 
 
; ======================
; Save Fit results 
; =================


; write new output files:
   get_lun, iu
   openw,iu,'./Input/jsmTaufit.inp'
   printf, iu, '  tauV, ebv, P_serk = '
   printf,iu, -data(0)/1.0875, ebv_ref, P_serk
   printf,iu, tauV, ebv_ref, P_serk
   close, iu
   free_lun, iu
   spawn, 'cp ./Input/jsmTaufit.inp  ./Result/'+ target+'_jsmTaufit.inp'

; writes fxdr file
   
;    print, ' relative reddening E(x-V)/Ebv is saved'
    data =  data / ebv_ref           ; saved relative reddening
   edata = edata / ebv_ref           ; saved relative reddening


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

;           ------------------------ done 
   close, /all
   print, ' ****   get_pol2xdr : done xdr saved: '
   print, ' save file =', './Result/'+ target+'_para.xdr'
   print, ' -------------------------------------------- ' 
   print
end
