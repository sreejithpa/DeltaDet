

;Find all delta spots and output a structure
;Name   : find_delta
;syntax : outstr=find_delta(cfname,mfname,roi=roi)
;INPUTS : cfname - Continuum fits file name
;	: mfname - Magnetic fits file name
;
;       : If cfname+mfname+[roi] are not input, then the following
;       : inputs are required - cimg, cindex, mimg, mindex
;
;KEYWORDS:
;	 - roi- Region of interest in [xcen,ycen,dx,dy] (Values from
;		roll-uncorrected image. roll angle correction will be done by the program.)
;	 - center,fov,xrange,yrange - similar to plot_map (give original values,
;		from roll corrected map/image
;	 - cimg,cindex,mimg,mindex - data images and index structures, if
;		included the program will use this and won't look for file to read.

;OUTPUT : A Structure containing following 29 elements
;	- CMAP 		: Map of continuum image
;	- MMAP 		: Map of Magnetic image
;	- DLTMAP	: Map of delta forming region umbra
;	- MSKMAP	: Map of mask marking umbra (+/-2 for P/N umbrae and 1
;			  for penumbra pixels.
;	- wcs		: WCS index (World coordinate system)
;	- CFNAME	: Continuum fits filename - input
;	- MFNAME	: Magnetic fits filename  - input
;	- UNBMAX        : Umbra Negative B_LOS maximum value
;	- UNBMIN        : Umbra Negative B_LOS minimum value
;	- UNBMEAN       : Umbra Negative B_LOS mean value
;	- UPBMAX        : Umbra Positive B_LOS maximum value
;	- UPBMIN        : Umbra Positive B_LOS minimum value
;	- UPBMEAN	: Umbra Positive B_LOS mean value
;	- TNEGFLX       : Negative Magnetic Flux in full ROI
;	- TPOSFLX       : Positive Magnetic flux infull ROI
;	- UNEGFLX       : Negative umbral magnetic flux
;	- UPOSFLX       : Positive umbral magnetic flux
;	- NDELTA	: Number of delta spots found
;	- DLTCEN	: Delta forming region center in arc secs
;	- DLTCENpx	: Delta forming region center in pixel
;	- DLTUNFLX  	: Negative umbral flux of delta formation
;	- DLTUPFLX   	: Positive umbral flux of delta formation
;	-chk		: 0 is failure 1 is success
;	-Comment	: comments
;	-dbstr		: a structure used for de-bugging - meant for
;				developers.
;EXAMPLES:
;	str=find_delta(cfname,mfname)
;	plot_map,str.cmap
;	plot_map,str.dltmap,/cont,/over


function distcal,x1,y1,x2,y2
	dist=sqrt((x2-x1)^2+(y2-y1)^2)
return,dist
end

;;Function to slelect umbra penumbra from continum and magnetic image
;; returns a byte array mask with 200 for positive and 50 for negative umbra
;; and 150 for penumbra pixels and 128 in other pixels


function umbsel,cimg,mimg,uilevel,umlevel,pilevel,pmlevel
   mask=byte(cimg*0.+128)
;Find the Quiet Sun peak from the data
   binsize=max(cimg>0)/100.
   tmp=max(histogram(cimg,min=2*binsize,bin=binsize),maxpos)
   cqsun=(maxpos+2)*binsize
; Normalise Continuum and  umbra selection
   cimgn=cimg/cqsun
;; Selection using levels
   umbselp=where(mimg ge umlevel[0] and mimg le umlevel[1] and $
       		cimgn le uilevel[1] and cimgn gt uilevel[0])
   umbseln=where(mimg le -umlevel[0] and mimg ge -umlevel[1] and $
       		cimgn le uilevel[1] and cimgn gt uilevel[0])
   pumbsel=where(abs(mimg) ge pmlevel[0] and abs(mimg) le pmlevel[1] and $
       		cimgn le pilevel[1] and cimgn gt pilevel[0])

   if n_elements(umbselp) gt 1 and umbselp[0] ne -1 then mask(umbselp)=200
   if n_elements(umbselp) gt 1 and umbseln[0] ne -1 then mask(umbseln)=50
   if n_elements(umbselp) gt 1 and pumbsel[0] ne -1 then mask(pumbsel)=150
return,mask
end


function find_delta,cfname,mfname,roi=inroi,fov=fov,center=center,$
    		xrange=xrange,yrange=yrange, $
                cimg=incimg, cindex=incindex, mimg=inmimg, mindex=inmindex
print,systim()
   version='1.0.1'
   distdeg=2.0		;Distance between opp. polarity umbra (defenition of
   			;delta condition)
   minusize=10		; Minimum size of umbra to be considered in pixel
   expdndelta=30	; Expected number of delta (max 127, using Byte array)
   uilevel=[0,0.6]	;Intensity levels to choose umbra
   umlevel=[500,7000]	; Magnetic levels to choose umbra
   pilevel=[0.6,0.9]	; Intensity levels to choose Penumbra
   pmlevel=[50,7000]	; Magnetic levels to choose Penumbra
   pfr=0.5		; penumbra fraction p_obs/p_exp
   pshare=5		;common penumbra sharing in pixels
   pesz=10		;size of expected penumbra in pixels

   params={version:version,distdeg:distdeg,minusize:minusize,expdndelta:expdndelta,$
       	     uilevel:uilevel,umlevel:umlevel,pilevel:pilevel,pmlevel:pmlevel,pfr:pfr,$
	     pshare:pshare,pesz:pesz}
;Determine if filename or image+index is to be used as input
       if n_elements(cfname) eq 1 and n_elements(mfname) eq 1 then doread=1 else doread=0
       if doread then print,'DOREAD=1: Using Mag. and Int. filenames as input' else print,'DOREAD=0: Using image arrays, indexes, and coord arrays as input'

;Rename input variables if no filenames supplied
       if not doread then begin
          cimg=incimg
          cindex=incindex
          cfname=''
          mimg=inmimg
          mindex=inmindex
          mfname=''
       endif


;Skip over the initial reading/processing if maps are input instead of filenames
    if doread then begin

;Read continuum image
        read_sdo,cfname,cindex,cimg,/uncomp_delete
;Do limb correction

        xyr = [ cindex.crpix1, cindex.crpix2, cindex.rsun_obs/cindex.CDELT1 ]
           darklimb_correct, cimg, cimg1, lambda = cindex.WAVELNTH, limbxyr = xyr
    ;Get the input ROI else find ROI to be used

        if n_elements(inroi) eq 4 then roi=inroi else begin
	    if n_elements(fov) ne 0 then begin
	        if n_elements(center) eq 2 then center1=float(center) $
		  else center1=[cindex.crpix1*cindex.cdelt1,cindex.crpix2*cindex.cdelt2]
	        if (cindex.crota2 gt 170) then center1=-1*center1
	        nfov=n_elements(fov)
                dfov=60.*float([fov(0),fov(nfov-1)])
                half_fov=dfov/2.
                xrange1=(([center1(0)-half_fov(0),center1(0)+half_fov(0)])/cindex.cdelt1)+cindex.crpix1
                yrange1=(([center1(1)-half_fov(1),center1(1)+half_fov(1)])/cindex.cdelt2)+cindex.crpix2
	        roi=[round((xrange1[0]+xrange1[1])/2.),round((yrange1[0]+yrange1[1])/2.),$
	          round((xrange1[1]-xrange1[0])+2),round((yrange1[1]-yrange1[0])+2)]
	        fov=0 & dfov=0 & center=0
	    endif
	    if n_elements(xrange) eq 2 and n_elements(yrange) eq 2 then begin
	        xrange1=(float(xrange)/cindex.cdelt1)+cindex.crpix1
                yrange1=(float(yrange)/cindex.cdelt2)+cindex.crpix2
	        if (cindex.crota2 gt 175 and cindex.crota2 le 185) then begin
		    xrange1=(float(-1*xrange)/cindex.cdelt1)+cindex.crpix1
		    yrange1=(float(-1*yrange)/cindex.cdelt2)+cindex.crpix2
	        endif
	        roi=[round((xrange1[0]+xrange1[1])/2.),round((yrange1[0]+yrange1[1])/2.),$
	          abs(round((xrange1[1]-xrange1[0]))),abs(round((yrange1[1]-yrange1[0])))]
            endif
        endelse

        if n_elements(roi) eq 4 then begin
	    xcen=round(roi[0]) & ycen=round(roi[1]) & dx = round(roi[2]) & dy = round(roi[3])

; Read Continuum image - only FOV region (again??)
;NOTE: you have already read-in the CIMG above. Why are you doing it again??
;	--Answer to the NOTE: It was done initially, so that index file keywords are
;	automatically updated with selected FOV info. Can be avoided by manually
;	changing the respective index keywords. Will do that in next version.
       	    read_sdo,cfname,cindex,cimg,xcen-round(dx/2.),ycen-round(dy/2.),dx,dy,/uncomp_delete

;Take sub image from limb-dark corrected image
       	    cimg=cimg1[xcen-round(dx/2.)-1:xcen-round(dx/2.)-1+dx-1,ycen-round(dy/2.)-1:ycen-round(dy/2.)-1+dy-1]

; Read Magnetic image - only FOV region
       	    read_sdo,mfname,mindex,mimg,xcen-round(dx/2.),ycen-round(dy/2.),dx,dy,/uncomp_delete

        endif else begin
	    cimg=cimg1  ; Full disk
; Read Magnetic image - Full disk
      	    read_sdo,mfname,mindex,mimg,/uncomp_delete
        endelse
        if (anytim2tai(cindex.date_obs)-anytim2tai(mindex.date_obs) gt 30 ) then begin
            print,"ERROR:Continuum and Magnetic images are not simultaneous"
       	    return,0
        endif

    	if cindex.crota2 ge 175. and cindex.crota2 le 185. then begin
            cimg=rotate(cimg,2)
            cindex.crpix1 = cindex.naxis1 - cindex.crpix1 + 1
            cindex.crpix2 = cindex.naxis2 - cindex.crpix2 + 1
            ;cindex.crota2 = cindex.crota2 - 180
            cindex.crota2 = 0.0
	    a=(cindex.crota2)
	    cindex.xcen=cindex.CRVAL1 + cindex.CDELT1*cos(a)*((cindex.NAXIS1+1)/2 $
	      -cindex.CRPIX1) -cindex.CDELT2*sin(a)*((cindex.NAXIS2+1)/2 $
		-cindex.CRPIX2)
	    cindex.ycen= cindex.CRVAL2 + cindex.CDELT1*sin(a)*((cindex.NAXIS1+1)/2 $
	      - cindex.CRPIX1) + cindex.CDELT2*cos(a)*((cindex.NAXIS2+1)/2 $
		- cindex.CRPIX2)
	endif

   	if mindex.crota2 ge 170. then begin
            mimg=rotate(mimg,2)
            mindex.crpix1 = mindex.naxis1 - mindex.crpix1 + 1
            mindex.crpix2 = mindex.naxis2 - mindex.crpix2 + 1
            ;mindex.crota2 = mindex.crota2 - 180
            mindex.crota2 = 0.
	    a=mindex.crota2
	    mindex.xcen=mindex.CRVAL1 + mindex.CDELT1*cos(a)*((mindex.NAXIS1+1)/2 $
	      -mindex.CRPIX1) -mindex.CDELT2*sin(a)*((mindex.NAXIS2+1)/2 $
		-mindex.CRPIX2)
	    mindex.ycen= mindex.CRVAL2 + mindex.CDELT1*sin(a)*((mindex.NAXIS1+1)/2 $
	      - mindex.CRPIX1) + mindex.CDELT2*cos(a)*((mindex.NAXIS2+1)/2 $
		- mindex.CRPIX2)

    	endif

    endif
;Determine the pixel -> cm^2 conversion
    pxcmsq=cindex.cdelt1*cindex.cdelt2*700e5*700e5

    wcs=fitshead2wcs(cindex)
    coord=wcs_get_coord(wcs)
    wcs_convert_from_coord,wcs,coord,'hg',lon,lat

     index2map,cindex,cimg,cmap
     index2map,mindex,float(mimg),mmap
     index2map,cindex,byte(cimg),mskmap
     datasz=size(cimg,/dim)
     imgcenpx=round(datasz/2)-1
     ;Structure that gives database of umbra satisfying condition one and two.
     dbstr={up:0d,un:0d,dltpos:bytarr(200,200),distar:fltarr(200,200),pfrnar:fltarr(200,200),$
	 pfrpar:fltarr(200,200),pshar:fltarr(200,200),uppos:fltarr(2,200),unpos:fltarr(2,200)}
     ; OUTPUT STR Def.
      str1={unbmax:0d, unbmin:0d, unbmean:0d,upbmax:0d, upbmin:0d, upbmean:0d, $
        tnegflx:0d,tposflx:0d,unegflx:0d, uposflx:0d,$
        wcs:wcs,mfname:mfname,cfname:cfname,$
	ndelta:0d,dltcen:fltarr(2,expdndelta),dltcenpx:intarr(2,expdndelta),dltunflx:dblarr(expdndelta),$
	dltupflx:dblarr(expdndelta),cmap:cmap,mmap:mmap,dltmap:mskmap,mskmap:mskmap,$
    	comment:' ',chk:0d,dbstr:dbstr,params:params}
;; Call umbsel function to select umbral and penumbral pixels
    mask=umbsel(cimg,mimg,uilevel,umlevel,pilevel,pmlevel)
    str1.mskmap.data=mask

    umbselp=where(mask eq 200)
    umbseln=where(mask eq 50)
    pumbsel=where(mask eq 150)

    if umbselp[0] eq -1 then begin
;	print,"No Positive polarity umbra in the FOV"
	str1.comment= "No Positive polarity umbra in the FOV"
        str1.chk=0
	return,str1
    endif else begin
    	mskup=float(cimg*0.)
    	mskup[umbselp]=1.
    endelse
    if umbseln[0] eq -1 then begin
	;print,"No Negative polarity umbra in the FOV"
	str1.comment="No Negative polarity umbra in the FOV"
        str1.chk=0
	return,str1
    endif else begin
   	mskun=float(cimg*0.)
   	mskun[umbseln]=1.
    endelse

    if pumbsel[0] eq -1 then begin
	;print,"No Penumbra in the FOV"
	str1.comment="No Penumbra in the FOV"
        str1.chk=0
	return,str1
    endif else begin
        mskp=float(cimg*0.)
        mskp[pumbsel]=1.
    endelse
; Label umbrae
    mskunl=label_region(mskun)
    mskupl=label_region(mskup)
    unpix=histogram(mskunl,bin=1,loc=unind)
    uppix=histogram(mskupl,bin=1,loc=upind)
    rankn = reverse(unind[sort(unpix)])
    rankp = reverse(upind[sort(uppix)])
; Only umbrae larger than minusize is used
    szn=max(where(unpix[rankn] ge minusize))
    szp=max(where(uppix[rankp] ge minusize))
       str1.dbstr.up=szp
       str1.dbstr.un=szn

    if szn eq 0 then begin
	;print," No negative umbra with minium required size"
        str1.comment=" No negative umbra with minium size"
        str1.chk=0
	return,str1
    endif
    if szp eq 0 then begin
        str1.comment=" No positive umbra with minium size"
        str1.chk=0
	return,str1
       ;print," No positive umbra with minium required size"
    endif
;Ordering the label as per rank
   mskupord=cimg*0.
   mskunord=cimg*0.
   ss1=size(unind,/dim)
   for i=0,ss1[0]-1 do begin
	mskunord[where(mskunl eq rankn[i])]=i
   endfor
   ss2=size(upind,/dim)
   for i=0,ss2[0]-1 do begin
	mskupord[where(mskupl eq rankp[i])]=i
   endfor
;;============== Condition 1 check ==================

;check largest szn,szp umbrae of both polarity

   deltapos=bytarr(szn,szp)
   distar1=fltarr(szn,szp)
   pfrnar1=fltarr(szn,szp)
   pfrpar1=fltarr(szn,szp)
   pshar1=fltarr(szn,szp)
   dp=fltarr(szp)
   dn=fltarr(szn)
   cenp=fltarr(2,szp)
   cenn=fltarr(2,szn)
   mskdelta=byte(cimg*0.+128)
   ndelta=0
   for ii=1,szn do begin
	mskuns=cimg*0.			;mask umbra positive selected
	tn=where(mskunord eq ii)
	if tn[0] eq -1 then continue
	mskuns(tn)=1.
	cenn1=centroid(mskuns*mimg)
	for jj=1,szp do begin
		mskups=cimg*0.
   		tp=where(mskupord eq jj)
		if tp[0] eq -1 then continue
		mskups(tp)=1.
		cenp1=centroid(mskups*mimg)
		cenp[*,jj-1]=cenp1
		cenn[*,ii-1]=cenn1
		dist1=distcal(lat[cenp1[0],cenp1[1]],lon[cenp1[0],cenp1[1]],lat[cenn1[0],cenn1[1]],lon[cenn1[0],cenn1[1]])
		distar1[ii-1,jj-1]=dist1
		if (dist1 le distdeg) then begin

			deltapos[ii-1,jj-1]=1  ;Condition -1 Satisfied
			mskups1=smooth(mskups,pesz)
			tmp=where(mskups1 gt 0.)
			mskups1(tmp)=1.
			mskuns1=smooth(mskuns,pesz)
			tmp=where(mskuns1 gt 0.)
			mskuns1(tmp)=1.
			pop=where((mskups1*mskp) gt 0.)
			pon=where((mskuns1*mskp) gt 0.)
			pep=where((mskups1-mskups) gt 0.)
			pen=where((mskuns1-mskuns) gt 0.)
			match,pop,pon,subpop,subpon,count=cnt

			pshar1[ii-1,jj-1]=cnt
			pfrp=float(size(pop,/dim))/float(size(pep,/dim))
			pfrn=float(size(pon,/dim))/float(size(pen,/dim))
			pfrnar1[ii-1,jj-1]=pfrn
			pfrpar1[ii-1,jj-1]=pfrp
			if (pfrn gt pfr and pfrp gt pfr and cnt ge pshare) then begin
			    ;NOTE: Modified pfrn and pfrp gt pfr individually, instead
			    ;of (pfrn+pfrp)>2*pfr earlier as per telecon on 21oct

			     ;print,'one',ii,jj,dp[jj-1],dn[ii-1],ndelta

			    if dp[jj-1] eq 0 and dn[ii-1] eq 0 then begin
				ndelta++
				dp[jj-1]=ndelta
				dn[ii-1]=ndelta
				;print,'two',ii,jj,dp[jj-1],dn[ii-1],ndelta
	    		    endif else begin
				mndlt=min([dp[jj-1],dn[ii-1]])
				mxdlt=max([dp[jj-1],dn[ii-1]])

				if dp[jj-1] eq 0. or dn[ii-1] eq 0 then begin
				    dp[jj-1]=mxdlt
			            dn[ii-1]=mxdlt
				endif else begin
				    if dp[jj-1] ne dn[ii-1] then begin
					ndelta=ndelta-1

				    	dp[jj-1]=mndlt
				    	dn[ii-1]=mndlt
				    	tmp=where(dp eq mxdlt)
			            	if tmp[0] ne -1 then dp[tmp]=mndlt
				    	tmp=where(dn eq mxdlt)
			            	if tmp[0] ne -1 then dn[tmp]=mndlt
					tmp=where(dp gt ndelta)
					if tmp[0] ne -1 then dp[tmp]=dp[tmp]-1
					tmp=where(dn gt ndelta)
					if tmp[0] ne -1 then dn[tmp]=dn[tmp]-1
				    endif

				endelse
				;print,'thre',ii,jj,dp[jj-1],dn[ii-1],ndelta
	    		    endelse
				;print,'four',ii,jj,dp[jj-1],dn[ii-1],ndelta
			    deltapos[ii-1,jj-1]=2  ;Condition 2 satisfied
			;    mskdelta(tp)=128+dp[jj-1]
			;   mskdelta(pop)=150.
			 ;   mskdelta(tn)=128-dn[ii-1]
			;   mskdelta(pon)=100.

			endif

		endif
   endfor & endfor

   if szn lt 200 and szp lt 200 then begin
       str1.dbstr.dltpos[0:szn-1,0:szp-1]=deltapos[*,*]
       str1.dbstr.uppos[*,0:szp-1]=cenp[*,*]
       str1.dbstr.unpos[*,0:szn-1]=cenn[*,*]
       str1.dbstr.distar[0:szn-1,0:szp-1]=distar1[*,*]
       str1.dbstr.pfrnar[0:szn-1,0:szp-1]=pfrnar1[*,*]
       str1.dbstr.pfrpar[0:szn-1,0:szp-1]=pfrpar1[*,*]
       str1.dbstr.pshar[0:szn-1,0:szp-1]=pshar1[*,*]
   endif

 if ndelta ge 1 then begin
;Flux of delta forming umbrae
   str1.ndelta=ndelta
;stop
   for ii=0,ndelta-1 do begin

  	dp1=where(dp eq ii+1)
  	dn1=where(dn eq ii+1)
	dltcenx=mean([reform(cenp[0,dp1]),reform(cenn[0,dn1])])
	dltceny=mean([reform(cenp[1,dp1]),reform(cenn[1,dn1])])
	str1.dltcenpx[0,ii]=dltcenx & str1.dltcenpx[1,ii]=dltceny
	str1.dltcen[0,ii]=cmap.xc+(dltcenx-imgcenpx[0])*cmap.dx	;arc seconds
	str1.dltcen[1,ii]=cmap.yc+(dltceny-imgcenpx[1])*cmap.dy ;arc seconds
	sdn1=size(dn1,/dim)
	for kk=0,sdn1[0]-1 do begin
		tn=where(mskunord eq (dn1[kk]+1))
		mskdelta(tn)=128-ii-1
	    ;   mskdelta(pon)=100.
	endfor
	sdp1=size(dp1,/dim)
	for kk=0,sdp1[0]-1 do begin
		tp=where(mskupord eq (dp1[kk]+1))
		mskdelta(tp)=128+ii+1
	    ;   mskdelta(pop)=150.
	endfor
	tn=where(mskdelta eq 128-(ii+1))
	tp=where(mskdelta eq 128+(ii+1))
	;if tn[0] eq -1 then continue
	str1.dltunflx[ii]=total(mimg(tn))*pxcmsq
	str1.dltupflx[ii]=total(mimg(tp))*pxcmsq
   endfor
   str1.dltmap.data=mskdelta
 endif
   str1.unbmax=max(abs(mimg(umbseln)))  ; Max Negative umbrae
   str1.unbmin=min(abs(mimg(umbseln)))
   str1.unbmean=mean(abs(mimg(umbseln)))
   str1.upbmax=max(abs(mimg(umbselp)))  ; Max of Positive umbrae
   str1.upbmin=min(abs(mimg(umbselp)))
   str1.upbmean=mean(abs(mimg(umbselp)))
   str1.tnegflx=total(mimg<0.)*pxcmsq
   str1.tposflx=total(mimg>0.)*pxcmsq
   str1.unegflx=total(mimg(umbseln))*pxcmsq
   str1.uposflx=total(mimg(umbselp))*pxcmsq
   str1.chk=1
   str1.comment='Successful: '+string(ndelta)+' deltas found'

outstr=str1
print,systim()
return,outstr
end

