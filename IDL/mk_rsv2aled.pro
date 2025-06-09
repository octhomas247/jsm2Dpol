 pro  mk_rsv2aled, target, chi2JHK=chi2JHK, nodark=nodark, noRpp=noRpp
@jismo_noscreen.inc
; finds best upper radius of dark dust to match observed
; Rsv=p850/Pserk for Omega computed to fit optical max polarisation of
; the dust model to PSerk
; Input: files read form directory   ' ./Result/'
; -------------------


if keyword_set(chi2JHK) then $
print, ' *** Improve fit to JHK reddening (see ibest) for target = ', target

   
   if keyword_set(nodark) then begin
      print, ' Pol by dark dust ignored set: spdd=0 and emip_t = emip_c + emip_Si'
      print, '  Rsv canot be fit without dark dust continue?'
   endif


; set up radii and grid for computationof Rsv for new upper dark dust
; radius rled
narad   = 130
rr      = 6.e-7* 1.05^ findgen(narad) ; org radius grid
amax    = 3.0e-4
amin    = 6.0e-5
iir     = where(rr gt amin and rr lt amax, nr)

c2rled   = fltarr(nr)
c2pled   = fltarr(nr)
c2rsvled = fltarr(nr)
c2rppled = fltarr(nr)
Rsvled   = fltarr(nr)
Rppled   = fltarr(nr)     ; 850mu pol.Intensity /pV (pol in V band) := p850*oI850/P_serk (MJy/sr)
rled     = rr(iir)

; ---------------------------------------------
; read best model fit of the red and pol curve using get_bestchi2pol:
fxdr   = './ResultRsv/' +target+'_para.xdr'
restore, fxdr


aled_old = aled

  data        = data   *ebv_ref         ; absolute reddening
  edata       = edata  *ebv_ref         ; absolute reddening
  arad_polmax = aled

  if keyword_set(ps) then begin
    if noscreen NE 0 then begin
      set_plot, 'PS'
      device, filename='mk_rsv2aled.ps', /COLOR
    endif
  endif

  if keyword_set(ps) then begin
plot, wpd, pdata, psym=4, /ysty
endif else if noscreen EQ 0 then begin
    plot, wpd, pdata, psym=4, /ysty
endif
; alec, alesi as fixed parameters:
for i = nr-1, 0, -1 do begin

aled = rled(i)

  get_lun, iu
  openw,iu,'./Input/jsm12fit.inp'
  printf, iu, '  abuc    abusi    avsi   avgr    apah     qmrn  rlec      rlesi     r_pmin_aC r_pmin_Si r_pmax    r_Darkmax '

  printf, iu, format='(6f8.3, 6e10.3)', abuc, abusi, abuvsi, abucvgr, abucpahs, qmrn, $
        alec, alesi, arad_polmin_aC, arad_polmin_Si, arad_polmax, aled 
  close, iu
  free_lun, iu

spawn,   './a.j >  message.out'


rdfloat, './Output/PolKappa.out', skipline=3,  wpol,spc, spsi,spd,stm, /sil
wpol  = wpol *1e4
spcd  = interpol(spc,  wpol, wpd)
spsid = interpol(spsi, wpol, wpd)
spdd  = interpol(spd,  wpol, wpd)

; convert to % ; use to get B field direction, d.Q* computed for Omega=60deg
; scale_P < 1.333 because Omega <90deg:

if keyword_set(nodark) then spdd = 0.

 pm      = (spcd + spsid + spdd) * 100.
 pmax    = max(pm)
 scale_P = P_Serk/Pmax < 1.333333
 sOmega  = sqrt(scale_P*1d0) * sin(60./180.d0 *!pi)

  Omega  = asin(sOmega) * 180./!pi
  pm     = pm * scale_p
  pmax   = max(pm)
  c2pled(i)  = total((pdata      - pm)^2      / epd^2) 
  
; polarized emisson:
  readcol, './Output/emipol.out', skipline=2, wemi, emis, emip_t, emip_c, emip_Si, emip_d, /sil

if keyword_set(nodark) then  emip_t = emip_c + emip_Si
  
k850   = min(where(wemi le 850.))                      ; & print, wemi(k850)
p850   = emip_t(k850)/emis(k850)  * 100.  * scale_p




; abs redd data:
absredd_mod  = 2.5/alog(10.) * (stm/stm(kvis) -1.) * stm(kvis)
areddmod     = interpol(absredd_mod,  w, wdata)

c2rled(i)     = total((areddmod(1:3)-areddata(1:3))^2/areddata(1:3)^2)
Rsvled(i)     = p850/(P_serk/tauv)
c2rsvled(i)   = (rsvled(i) - orsv)^2/oersv^2

if Keyword_set(noRpp) then  Rppled(i) = oRpp else $ 
   Rppled(i)  = p850 * oI850/P_serk
c2rppled(i)   = (rppled(i) - orpp)^2/oerpp^2



; if i  eq 0 then  plot, 1./wdata, areddata
;  oplot, 1./wdata, areddmod  
; print, format='(a10,i7, 2f7.2, f9.3)', target, nint(rled(i)*1e7), rsvled(i), c2rsvled(i), c2rled(i)


if keyword_set(ps) then begin
    oplot, wpd, pm, linesty=i mod 2
endif else if noscreen EQ 0 then begin
        oplot, wpd, pm, linesty=i mod 2
    endif

endfor

; --------------------------------------------------------------------
; compute aled using best fitting Rsv and NIR redd curve


if Keyword_set(noRpp) then begin                    
   a0        = c2pled/median(c2pled)
   a1        = abs(c2rsvled)/median(abs(c2rsvled))
   a2        = 0.*a1
   b         = c2rled/median(c2rled)
   
endif else begin
   a0        = c2pled/median(c2pled)
   a1        = abs(c2rsvled)/median(abs(c2rsvled))
   a2        = abs(c2rppled)/median(abs(c2rppled))
   b         = c2rled/median(c2rled)
endelse

c2         = (a0^2  + a1^2  +a2^2 + b^2)/4.


; select best models that  fit within ksig * (error in Rsv, Rpp) 
ksig = 1.
ii = where(rsvled ge orsv-ksig*oersv and rppled ge orpp-ksig*oerpp, nb) 
if nb lt 1 then begin
   print
   print, ' -------------------------------------'
   print, ' no fit within 1sigma to Rsv or Rpp found -> take all for star', target
   print, ' -------------------------------------'
   ksig = 1d9
   ii = where(rsvled ge orsv-ksig*oersv and rppled ge orpp-ksig*oerpp, nb) 
  endif

print, '  *** mk_rsvaled: select models within ksig * (error in Rsv, Rpp) gives ksig, number of models nb = ', ksig, nb

ibest         = (where(c2 eq min(c2(ii)),nb))(0)
ibest         = ibest(0)

 if keyword_set(chi2JHK) then  ibest         = (where (b eq min(b))) (0)


 
aled          = rled(ibest)
arad_polmax   = rled(ibest)
Rsv           = rsvled(ibest)
Rpp           = rppled(ibest)
c2rsv         = c2rsvled(ibest)
c2rpp         = c2rppled(ibest)



; plot, rled*1e7, c2
; oplot, rled*1e7, a1, psym=plotsym(/circle,scale= 1.13,/fill, color=FSC_Color('blue'))
; oplot, rled*1e7, a2, psym=plotsym(/circle,scale=1.13,/fill, color=FSC_Color('orange'))
; oplot, rled*1e7, b, psym=plotsym(/circle,scale=1.38,/fill, color=FSC_Color('magenta'))
; oplot, [aled, aled]*1e7, [c2(ibest), c2(ibest)], psym=plotsym(/box,scale=2.8,/fill)

 
 print
 print, ' --------------------------------- '
 print, ' Best Rsv fit model selected:     '
 print, format='(a10,i6, f6.1, a6, 2f6.1, f6.1, a6, 2f6.1, a20, 2f7.2)', target, nint(aled*1e7), $
 rsvled(ibest), '=Rsv= ', orsv, oersv, rppled(ibest), '=Rpp= ', orpp, oerpp, ' c2(ibest,median)= ', c2(ibest), median(c2)

save, filename='./Result/'+target+'_Rsvled.xdr', rled, rsvled, Rppled, c2rsvled, c2rppled, c2rled, c2, ibest, aled, aled_old



; ==============================================

; restore, /verb, './Result/'+target+'_Rsvled.xdr' 



  get_lun, iu
  openw,iu,'./Input/jsm12fit.inp'
  printf, iu, '  abuc    abusi    avsi   avgr    apah     qmrn  rlec      rlesi     r_pmin_aC r_pmin_Si r_pmax    r_Darkmax '

  printf, iu, format='(6f8.3, 6e10.3)', abuc, abusi, abuvsi, abucvgr, abucpahs, qmrn, $
        alec, alesi, arad_polmin_aC, arad_polmin_Si, arad_polmax, aled 
  close, iu
  free_lun, iu


  data  = data   /ebv_ref         ; relative reddening
  edata = edata  /ebv_ref         ; relative reddening
  
  save, /xdr, filename='./Result/'+ target+'_para.xdr', $
  target, kvis, kblue, $ 
  abuc, abusi,  abuvsi,    abucvgr,    abucpahs,  acd, asid, actd, asiTD, Mgd, $
  qmrn, alec, alesi, aled,  arad_polmin_aC, arad_polmin_Si, arad_polmax, x0, gam, $
  Mmrn, Mdark, fdark, tauV, Ebv_ref,  Ebv_mod, Vmag, Mv, Dlum, $
  wredd_org, redd_org, eredd_org,  wdata, data, edata, areddata, areddmod, $
  w, sac, ssac, sasi, sssi, savgr, ssvgr, savsi, ssvsi, sapah, sad, ssd, st, $
 wpd, pdata, epd, scale_P, Omega, pm, pmax, P_serk, rsv, rpp, c2rsv, c2rpp, $ 
 oI850, op850, oep850, oRsv, oRpp, oeRsv,  oeRpp, $
 chi2Rsv, chi2redd, chi2amin, chi2pmin



 spawn, 'cp ./Output/Kappa.out    ./Result/'+ target+'_Kappa.out'
 spawn, 'cp ./Output/PolKappa.out ./Result/'+ target+'_PolKappa.out'
 spawn, 'cp ./Input/jsm12fit.inp  ./Result/'+ target+'_jsm12fit.inp'
 spawn, 'cp ./Input/PAH2170.wq    ./Result/'+ target+'_PAH2170.wq'
 spawn, 'mv message.out           ./Result/'+ target+'_message.out'

 if keyword_set(ps) then begin
    if noscreen NE 0 then begin
      device, /close
      spawn, 'ps2pdf mk_rsv2aled.ps mk_rsv2aled.pdf'
    endif
  endif

  close, /all

  if oI850 eq 1. then print, 'Rpp not used I850 (MJy/sr) unknown in ./Cat/mk_ForsdPlanck, check get_pol2xdr'



print,  ' ***bis hier  mk_rsv2aled done aled old, new (nm) = ', nint([aled_old, aled]*1e7)

  
print, ' *** mk_rsv2aled done aled old, new (nm) = ', nint([aled_old, aled]*1e7)
print
end
