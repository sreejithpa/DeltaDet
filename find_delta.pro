function distcal,x1,y1,x2,y2

dist=sqrt((x2-x1)^2+(y2-y1)^2)

return,dist
end

;Find all delta spots and output a structure
;Name   : find_delta
;syntax : outstr=find_delta(cfname,mfname,roi=roi)
;INPUTS : cfname - Continuum fits file name
;	: mfname - Magnetic fits file name
;KEYWORDS: ROI - Region of interest in [xcen,ycen,dx,dy]
;OUTPUT : A Structure containing following 29 elements
;	- CMAP 		: Map of continuum image
;	- MMAP 		: Map of Magnetic image
;	- DLTMAP	: Map of delta forming region umbra and penumbra
;	- CINDEX	: Complete index from continuum fits file
;	- MINDEX	: Complete index from magnetic fits file
;	- wcs		: WCS index (World coordinate system)
;	- CFNAME	: Continuum fits filename - input
;	- MFNAME	: Magnetic fits filename  - input
;	- UMBSELP       : Array giving pixel position of positive umbra
;	- UMBSELN       : Array giving pixel position of negative umbra
;	- PUMBSEL  	: Array giving pixel position of penumbra
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
;	- DISTDEG       : Distance between delta-forming umbrae in degrees
;	- NDELTA	: Number of delta spots found
;	- DLTPPOS       : Order numbers (on size) of positive umbrae forming delta
;	- DLTNPOS 	: Order numbers (on size) of negative umbrae forming delta
;	- DLTUPCEN      : Delta forming positive umbrae centroids
;	- DLTUNCEN	: Delta forming negative umbrae centroids
;	- DLTUNFLX  	: Negative umbral flux of delta formation
;	- DLTUPFLX   	: Positive umbral flux of delta formation

;EXAMPLES:
;	str=find_delta(cfname,mfname)
;	plot_map,str.cmap
;	plot_map,str.dltmap,/cont,/over

function find_delta,cfname,mfname,roi=inroi
print,systim()
;Get the input ROI else find ROI to be used

   if n_elements(inroi) ne 0 then begin
      roi=inroi
      xcen=roi[0] & ycen=roi[1] & dx = roi[2] & dy = roi[3]
; Read Continuum and Magnetic image
       read_sdo,cfname,cindex,cimg,/uncomp_delete
       xyr = [ cindex.crpix1, cindex.crpix2, cindex.rsun_obs/cindex.CDELT1 ]
       darklimb_correct, cimg, cimg1, lambda = cindex.WAVELNTH, limbxyr = xyr
       read_sdo,cfname,cindex,cimg,xcen-round(dx/2.),ycen-round(dy/2.),dx,dy,/uncomp_delete
       cimg=cimg1[xcen-round(dx/2.)-1:xcen-round(dx/2.)-1+dx-1,ycen-round(dy/2.)-1:ycen-round(dy/2.)-1+dy-1]
       read_sdo,mfname,mindex,mimg,xcen-round(dx/2.),ycen-round(dy/2.),dx,dy,/uncomp_delete
   endif else begin
       read_sdo,cfname,cindex,cimg,/uncomp_delete
       xyr = [ cindex.crpix1, cindex.crpix2, cindex.rsun_obs/cindex.CDELT1 ]
       darklimb_correct, cimg, cimg1, lambda = cindex.WAVELNTH, limbxyr = xyr
       cimg=cimg1
       read_sdo,mfname,mindex,mimg,/uncomp_delete
   endelse

   if (anytim2tai(cindex.date_obs)-anytim2tai(mindex.date_obs) gt 30 ) then begin

       print,"ERROR:Continuum and Magnetic images are not simultaneous"
       return,0
   endif


   if cindex.crota2 ge 170. then begin
        cimg=rotate(cimg,2)
        cindex.crpix1 = cindex.naxis1 - cindex.crpix1 + 1
        cindex.crpix2 = cindex.naxis2 - cindex.crpix2 + 1
        cindex.crota2 = cindex.crota2 - 180
   endif

   if mindex.crota2 ge 170. then begin
        mimg=rotate(mimg,2)
        mindex.crpix1 = mindex.naxis1 - mindex.crpix1 + 1
        mindex.crpix2 = mindex.naxis2 - mindex.crpix2 + 1
        mindex.crota2 = mindex.crota2 - 180
    endif

;Find the Quiet Sun peak from the data
   binsize=max(cimg>0)/100.
   tmp=max(histogram(cimg,min=2*binsize,bin=binsize),maxpos)
   cqsun=(maxpos+2)*binsize
; Normalise Continuum and  umbra selection
   cimgn=cimg/cqsun
   umbselp=where(mimg ge 500 and cimgn le 0.7)
   if umbselp[0] eq -1 then begin
	print,"No Positive polarity umbra in the FOV"
	return,0
   endif else begin
   mskup=cimg*0.
   mskup[umbselp]=1.
   endelse
   umbseln=where(mimg le -500 and cimgn le 0.7)
   if umbseln[0] eq -1 then begin
	print,"No Negative polarity umbra in the FOV"
	return,0
   endif else begin
   mskun=cimg*0.
   mskun[umbseln]=1.
   endelse

;Penumbra selection

   pumbsel=where(cimgn le 0.9 and cimgn ge 0.7 and abs(mimg) ge 50 )
   if pumbsel[0] eq -1 then begin
	print,"No Penumbra in the FOV"
	return,0
   endif else begin
        mskp=cimg*0.
        mskp[pumbsel]=1.
   endelse

; Label umbrae
   mskunl=label_region(mskun)
   mskupl=label_region(mskup)
   unpix=histogram(mskunl,bin=1,loc=unind)
   uppix=histogram(mskupl,bin=1,loc=upind)

   mskupord=cimg*0.
   mskunord=cimg*0.

   ss1=size(unind,/dim)
   for i=0,ss1[0]-1 do begin
	rankn = reverse(unind[sort(unpix)])
	mskunord[where(mskunl eq rankn[i])]=i

   endfor
   ss2=size(upind,/dim)
   for i=0,ss2[0]-1 do begin
	rankp = reverse(upind[sort(uppix)])
	mskupord[where(mskupl eq rankp[i])]=i

   endfor

;;============== Condition 1 check ==================

;check largest 10 umbrae of both polarity
   if max(rankn) ge 30 then sz1=30 else sz1=max(rankn)
   if max(rankp) ge 30 then sz2=30 else sz2=max(rankp)

   wcs=fitshead2wcs(cindex)
   coord=wcs_get_coord(wcs)
   wcs_convert_from_coord,wcs,coord,'hg',lon,lat
   distdeg=2.0
   deltapos=fltarr(sz1,sz2)
   cenp=fltarr(2,sz1,sz2)
   cenn=fltarr(2,sz1,sz2)
;   dist=fltarr(sz1,sz2)
		mskdelta=cimg*0.
   for ii=1,sz1 do begin
	for jj=1,sz2 do begin
		mskuns=cimg*0.			;mask umbra positive selected
		mskups=cimg*0.
   		tp=where(mskupord eq jj)
		if tp[0] eq -1 then continue
		mskups(tp)=1.
		tn=where(mskunord eq ii)
		if tn[0] eq -1 then continue
		mskuns(tn)=1.
		cenp1=centroid(mskups*mimg)
		cenn1=centroid(mskuns*mimg)

		dist1=distcal(lat[cenp1[0],cenp1[1]],lon[cenp1[0],cenp1[1]],lat[cenn1[0],cenn1[1]],lon[cenn1[0],cenn1[1]])
		if (dist1 le distdeg) then begin

			mskups1=smooth(mskups,10)
			tmp=where(mskups1 gt 0.)
			mskups1(tmp)=1.
			mskuns1=smooth(mskuns,10)
			tmp=where(mskuns1 gt 0.)
			mskuns1(tmp)=1.
			pop=where((mskups1*mskp) gt 0.)
			pon=where((mskuns1*mskp) gt 0.)
			pep=where((mskups1-mskups) gt 0.)
			pen=where((mskuns1-mskuns) gt 0.)
			match,pop,pon,subpop,subpon,count=cnt

			pfrp=float(size(pop,/dim))/float(size(pep,/dim))
			pfrn=float(size(pon,/dim))/float(size(pen,/dim))
			;print,'pfrn == ', pfrn,'PFRP ==',pfrp
			if (pfrn+pfrp) gt 1.1 and cnt ge 5. then begin
				deltapos[ii-1,jj-1]=1.
				cenp[*,ii-1,jj-1]=cenp1 &cenn[*,ii-1,jj-1]=cenn1
				mskdelta(tp)=2.
				mskdelta(pop)=1.
				mskdelta(tn)=-2.
				mskdelta(pon)=-1.

			endif

		endif
   endfor & endfor

   ds=where(deltapos eq 1.)
   ;number of deltas detected
   nds=(size(ds,/dim))[0]
;position of positive and negative umbra forming the delta
   ppos=round(ds/sz1)& npos= ds-ppos*sz1


;centroid of delta forming spots in pixels
   dltcenpx=fltarr(2,nds)
   dltcennx=fltarr(2,nds)
;centroid of delta forming spots in HG lat-long
;   cenpdeg=fltarr(2,nds)
 ;  cenndeg=fltarr(2,nds)
;Flux of delta forming umbrae
   dltunflx=fltarr(nds)
   dltupflx=fltarr(nds)

;px to sqcm conversion
   pxcmsq=cindex.cdelt1*cindex.cdelt2*700e5*700e5

   for ii=0,nds-1 do begin
	dltcenpx[*,ii]=[cenp[0,npos[ii],ppos[ii]],cenp[1,npos[ii],ppos[ii]]]
	dltcennx[*,ii]=[cenn[0,npos[ii],ppos[ii]],cenn[1,npos[ii],ppos[ii]]]
;	cenpdeg[*,ii]=[lat[cenp[0,npos[ii],ppos[ii]],cenp[1,npos[ii],ppos[ii]]],lon[cenp[0,npos[ii],ppos[ii]],cenp[1,npos[ii],ppos[ii]]]]
;	cenndeg[*,ii]=[lat[cenn[0,npos[ii],ppos[ii]],cenn[1,npos[ii],ppos[ii]]],lon[cenn[0,npos[ii],ppos[ii]],cenn[1,npos[ii],ppos[ii]]]]
        tp=where(mskupord eq ppos[ii]+1)
	;if tp[0] eq -1 then continue
	tn=where(mskunord eq npos[ii]+1)
	;if tn[0] eq -1 then continue
	dltunflx[ii]=total(mimg(tn))*pxcmsq
	dltupflx[ii]=total(mimg(tp))*pxcmsq
   endfor
   index2map,cindex,cimg,cmap
   index2map,mindex,mimg,mmap
   index2map,cindex,mskdelta,deltamap
   ;Output structure
    str1={unbmax:0d, unbmin:0d, unbmean:0d,upbmax:0d, upbmin:0d, upbmean:0d, $
    tnegflx:0d,tposflx:0d,unegflx:0d, uposflx:0d,umbselp:umbselp,$
    umbseln:umbseln,pumbsel:pumbsel,mindex:mindex,cindex:cindex,wcs:wcs,mfname:mfname,$
    cfname:cfname,distdeg:distdeg,ndelta:nds,dltppos:ppos,dltnpos:npos,dltupcen:dltcenpx,$
    dltuncen:dltcennx,dltunflx:dltunflx,dltupflx:dltupflx,cmap:cmap,mmap:mmap,dltmap:deltamap}
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

outstr=str1
print,systim()
return,outstr
end

