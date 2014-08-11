pro ar_extract,fmdi,pospropstr

t1=systim(/utc)

;Set up parameters---------------------------------->

;Read in the AR detection parameter file
fparam='/Users/sreejith/Solar/DeltaDet/ar_param_hmi.txt'
params=ar_loadparam(fparam=fparam)

;Input magnetogram
;fmdi='./hmi.m_45s.2011.02.14_23_31_30_TAI.magnetogram.fits'


;Start detecting ARs-------------------------------->

;Read in a fits file (including WCS and full header)
thismap=ar_readmag(fmdi)
thismap1k=map_rebin(thismap,/rebin1k)
;maporig=thismap1k
imgsz=size(thismap1k.data)

;create stupid mask with 1's where there are pixels above 500
dummask=fltarr(imgsz[1],imgsz[2])
dummask[500,500]=1
;dummask[where(abs(thismap.data) gt 500)]=1

;filedate=anytim(file2time(fmdi),/vms)
fdate=time2file(thismap1k.time,/date)
fyyyy=strmid(fdate,0,4)

;Turn on median filtering due to the noise...
params.DOMEDIANFILT=0
params.DOCOSMICRAY=0
;Create AR mask (includes processing of MDI image -> read out into THISPROC)
thisarstr=ar_detect_core(thismap1k, /nosmart, mapproc=thisproc, params=params,status=status, cosmap=cosmap, limbmask=limbmask,/doprocess)

mask=ar_core2mask(thisarstr.data)
;mask=thisarstr.data

;thisarstr.data=mask
;thisarstr=map_rebin(thisarstr,xy=[4096,4096])
;mask=thisarstr.data
if max(mask) gt 0. then begin
pospropstr=ar_posprop(map=thismap1k, mask=mask, cosmap=cosmap, params=params)
endif else pospropstr=-1
;loadct,0,/sil
;plot_image,rot(magscl(thismap.data),-thismap.roll_angle)
;setcolors,/sil,/sys
;plots,pospropstr.xcenbnd*4.,pospropstr.ycenbnd*4.,ps=4,color=!red

;print,'Computation time='
;print,anytim(systim(/utc))-anytim(t1),'s'
;
;print,'X,Y pixel positions of AR bounding box centers'
;print,[transpose(pospropstr.xcenbnd*4.),transpose(pospropstr.ycenbnd*4.)]
;
;stop

end
