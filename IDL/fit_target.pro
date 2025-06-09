pro fit_target, target_in
  @jismo_noscreen.inc
  compile_opt strictarr

  restore, /verb, '../../Data/mk_Tab1.xdr'
  nn = n_elements(star)
  found = 0
;   print, nn, star[0]
  for i = 0, nn - 1 do begin
    if star[i] eq target_in then begin
      fpol = findfile('../../Data/DataRsv/' + star[i] + '.pol', count=nfp)
      if nfp eq 1 then begin
        found = 1
        target = star[i]

        ; Extract star properties
        Vmag    = Vk[i]
        Ebv_ref = Av[i] / Rv[i]
        Mv_mag  = MV[i]
        Dgaia   = DP[i]

        chi2JHK = (target eq 'HD093222' or target eq 'HD1542245' or target eq 'HD146285')
        mRsv = ~(target eq 'HD027778' or target eq 'HD287150')  ; 0 if in list, else 1

        print, ' +++++++++++++++++++++++++++++++'
        print, '  START          ', target
        print, ' +++++++++++++++++++++++++++++++'
  
        mp_absreDgaia, target,   Vmag,  Ebv_ref,  Mv_mag, Dgaia, /ps
        spawn, 'mv pl_allRedd.pdf ./Result/'+target+'_redd_0.pdf'
        print, ' ----------------'
        print, ' call  get_pol2xdr '
        get_pol2xdr,        target, mRsv=mrsv 
        print, ' ----------------'
        print, ' call  mk_chi2pol 0'
        mk_chi2pol,         target
        print, ' ----------------'
        print, ' call  get_best  0'
        get_bestchi2pol,    target, /ps
        spawn, 'mv pl_allReddbestchi2pol.pdf ./Result/'+target+'_model_3mu.pdf'
        ; spawn, 'mv ./ResultRsv/'+target+'_model.pdf ./Result/'+target+'_model_3mu.pdf'
        print, ' ----------------'
        print, ' call  mk_rasv  1:'
        mk_rsv2aled,        target, chi2JHK = chi2JHK 

        print, ' ============================================================ '
        print, ' ; -----------------   1 iteration -----                   '
        print, ' ============================================================ '
        print, ' ----------------'
        print, ' call  mp_fixed_ 1'
        mp_absred_fixedLed, target, /ps
        spawn, 'mv pl_allReddfixed.pdf ./Result/'+target+'_redd_1.pdf'
        print, ' ----------------'
        print, ' call  mk_chi2pol 1 '
        mk_chi2pol,         target
        print
        print, ' ----------------'
        print, ' call  get_best  1'
        print
        get_bestchi2pol,    target, /ps
        spawn, 'cp ./Input/jsm12fit.inp  ./Result/'+ target+'_jsm12fit_best2.inp'
        spawn, 'mv ./ResultRsv/'+target+'_model.pdf ./Result/'+target+'_model_best2.pdf'

        print, ' ============================================================ '
        print, ' ; -----------------   2 iteration -----                   '
        print, ' ============================================================ '
        print, ' call  mk_rasv  2:'
        mk_rsv2aled,        target, chi2JHK = chi2JHK
        print, ' ----------------'
        print, ' call  mp_fixed_ 2'
        mp_absred_fixedLed, target, /ps
        spawn, 'mv pl_allReddfixed.pdf ./Result/'+target+'_redd_2.pdf'
        print, ' ----------------'
        print, ' call  mk_chi2pol 2 '
        mk_chi2pol,         target
        print, ' ----------------'
        print, ' call  get_best 2'
        get_bestchi2pol,    target, /ps
        
        
        print, ' +++++++++++++++++++++++++++++++'
        print, '  D O N E :         ', star[i]
        print, ' +++++++++++++++++++++++++++++++'
        print
        ; if mRsv then begin
        ;   print, 'no RSV'
          ; run the version without Rsv
        ;   fit_no_rsv, target, Vmag, Ebv_ref, Mv_mag, Dgaia
        ; endif else begin
        ;   print, 'RSV'
          ; run the version with Rsv iteration
        ;   fit_with_rsv, target, Vmag, Ebv_ref, Mv_mag, Dgaia
        ; endelse
      endif
    endif
  endfor

  if found eq 0 then print, 'Target not found or no pol file:', target_in
end