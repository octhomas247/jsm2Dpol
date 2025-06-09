pro mk_chi2pol, target
  
; computes  Rsv, chi2 of Serk fit with min alignement radii for aC, Si  free
;  40nm < arad_polmin_aC,arad_polmin_Si) < [alec,alesi]
;
; Output: files  ./Result/HD*chi2*
; ----------------------------------------------------

      fxdr = findfile('./Result/'+target+'_para.xdr',   cou=nfx)
      if nfx ne 1 then  print, 'no/too many fxdr files check : ', nfx, fxdr

      fxdr = fxdr(0)
      restore, fxdr
      
; convert to absolute reddening curve E(w) and E(oo) < E(H,K) and
; recompute tauv accordingly
   data    = data    * ebv_ref
   edata   = edata   * ebv_ref   
   data(0) = data(0)  < min([data(1:2)])
   if abs(tauv  +data(0) * alog(10.)/2.5)  gt 0.001 then begin
      print, '   *** tauV check : tauv, - data(0) * alog(10.)/2.5 ', tauv, - data(0) * alog(10.)/2.5
   endif
   if tauv    lt 0  then stop, 'tauv < 0 check Dgaia for ', target
   

     finp = findfile('./Result/'+target+'_jsm12fit.inp', cou=nfi)
     fpah = findfile('./Result/'+target+'_PAH2170.wq',   cou=nfp)
     print, format='(a)', ' -- restore files : ', fpah, finp, fxdr
     if alec eq -9. or nfi ne 1 then stop, ' s.th wrong alec=', alec, finp
     spawn, 'cp ' +finp(0)  + ' ./Input/jsm12fit.inp' 
     spawn, 'cp ' +fpah(0)  + ' ./Input/PAH2170.wq'



  print, format='(a8,a10, f5.2,a15, f5.2, a10, f6.1, a10, f5.2, a7, f5.1)', target, $
          ' : tauv = ', tauV, '  E(B-V)_ref = ', ebv_ref, ' fdark =', fdark, ' P_serk = ', P_serk, ' oRsv = ', orsv

; open result file:
get_lun, iuc
openw,iuc,'./Result/'+target+'_chi2p.out'
; open result file:
get_lun, iucf
openw,iucf,'./Result/'+target+'_chi2p.info'

printf, iuc, '# Star   apmin_aC apmin_Si  Omega   Pmax     Rsv    chi2p    chi2a '
printf, iucf, format='(a8,a7, f5.2,a14, f5.2, a9, f6.1, a9, f5.2, a6, f5.1,a5, f5.1)', target, $
        '  tauv= ', tauV, '  E(B-V)_ref= ', ebv_ref, ' fdark=', fdark,  $
        ' P_serk= ', P_serk, ' Rsv= ', orsv, ' +/- ', oersv

printf, iucf, ' # icou, P_serk, PMax '

; Pdata:   
 npd = n_elements(pdata)

; set up radii:
narad          = 166
rr             = 6.e-7* 1.05^ findgen(narad)

;
;  loop over all rarad_polmin ac (=i), Si (=j)- radii
;
   lpc  = where(rr ge 0.45d-5 and rr le alec, nnc)
   lpsi = where(rr ge 0.45d-5 and rr le alesi, nnsi)


chi2p_best   = 999.   
icou         = -1
for i = 0, nnc -1 do begin  ; over aC radii
   arad_polmin_aC = rr(lpc(i))
for j = 0, nnsi-1 do begin   ; over Si radii
   arad_polmin_Si = rr(lpsi(j))

if(arad_polmin_aC < alec      and arad_polmin_Si < alesi ) then begin
 icou = icou + 1
      
  get_lun, iu
  openw,iu,'./Input/jsm12fit.inp'
  printf, iu, '  abuc    abusi     abuvsi abucvgr abucpahs qmrn   alec       alesi      apolmin_aC apolmin_Si apolmax    aDarkmax'
  printf, iu, format='(6f8.3,6e11.3)', abuc, abusi, abuvsi, abucvgr, abucpahs, qmrn, alec, alesi, $ 
  arad_polmin_aC, arad_polmin_Si, arad_polmax, aled
  close, iu
  free_lun, iu
  
 spawn,   './a.j > message.out'



rdfloat, './Output/PolKappa.out', skipline=3,  wpol,spc, spsi,spd,stm, /sil
wpol  = wpol *1e4
spcd  = interpol(spc,  wpol, wpd)
spsid = interpol(spsi, wpol, wpd)
spdd  = interpol(spd,  wpol, wpd)

; Convert to % and compute B field direction for best match in
; Pmax. The d.Q* computed for Omega=60deg, scale_P < 1.333 because
; Omega <90deg otherwise nan.

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
P850   = emip_t(k850)/emis(k850)  * 100.  * scale_P
Rsv    = P850/(P_serk/tauv)


; chi2

; for 'HD038023': pol data at 0.49 < w <0.91mu  appear perculiar
if target eq 'HD038023' then chi2p  = total((pdata(12:49) - pm(12:49))^2 / epd(12:49)^2) / (37.-1.-2.)  else $
                             chi2p  = total((pdata      - pm)^2      / epd^2) / (npd-1.-2.)      


chi2a  = total(([pdata, replicate(orsv,npd)] - [pm, replicate(rsv,npd)])^2 $
                / [epd,replicate(oersv, npd)]^2) / (2.*npd-1.-2.) 

chi2p = chi2p < 99.
chi2a = chi2a < 99.


if chi2p lt chi2p_best then begin
   printf, iuc, format='(a8, 2f9.1, i7, f7.2, f8.1, 1x, f8.2, 1x,f8.2 )', $
          target, arad_polmin_aC*1e7, arad_polmin_Si*1e7, nint(Omega), Pmax, rsv, chi2p, chi2a

   print, format='(i4, 2x, a8, 2f9.1, i7, f6.1, 2f6.2 )', icou, $
          target, arad_polmin_aC*1e7, arad_polmin_Si*1e7, nint(Omega), rsv, chi2p, chi2a
endif


;if abs(1.-P_Serk/Pmax) gt 0.07 then print, icou, ' -- P_ serk ne Pmax ', P_serk, PMax

if abs(1.-P_Serk/Pmax) gt 0.07 then printf, iucf, format='(i5, 2f6.2)', icou, P_serk, PMax

chi2p_best   = min([chi2p_best, chi2p])

endif                           ; of arad_polmin_aC < alec and arad_polmin_Si < alesi   

endfor
endfor

close, iuc
free_lun, iuc
close, iucf
free_lun, iucf

print, format='(a8,a10, f5.2,a15, f5.2, a10, f6.1, a10, f5.2, a7, f5.1)', target, $
          ' : tauv = ', tauV, '  E(B-V)_ref = ', ebv_ref, ' fdark =', fdark, ' P_serk = ', P_serk, ' oRsv = ', orsv

   print
   print, ' *** chi2p, chi2a, rsv saved in file: ', './Result/'+target+'_chi2p.out'
   print, ' --------------------------------------------------------------------- '
   print

   close, /all
   
 print, ' *** Done all files processed'
end

