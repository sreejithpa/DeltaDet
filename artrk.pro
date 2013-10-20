; IDL procedure by Sreejith

; Start		: 25 Jun 2013 12:24
; Last Mod 	: 11 Oct 2013 02:35

;-------------------  Details of the program --------------------------;
;PRO delta
;+
; NAME:
;      delta
;
; PURPOSE:
;       This program detectes delta formation, make movie sequence files, create
;	flux plots etc.
;
; CALLING SEQUENCE:
;       delta,indirc,indirm,xcen1,ycen1,dx1,xy1,[/movie,/plot,/noreadhmi]


pro artrk,indirc,indirm,ostrarr,$
    	MKMOVIE=mkmovie,movdir=movdir,MKPLOT=mkplot,roi=inroi

   if n_elements(inroi) ne 0 then begin
       roi=inroi
       xcen=roi[0] & ycen=roi[1] & dx = roi[2] & dy = roi[3]
   endif

   if strlen(indirc) ne 0. then begin
       spawn,'ls '+indirC+'/*.fits',cfnames
       ss=sizeof(cfnames)
       read_sdo,cfnames,cindex,/nodata,/uncomp_delete
       ctime=anytim2tai(cindex.date_obs)
   endif
   if strlen(indirm) ne 0. then begin
       spawn,'ls '+indirM+'/*.fits',mfnames
       ss1=sizeof(mfnames)
       read_sdo,mfnames,mindex,/nodata,/uncomp_delete
       mtime=anytim2tai(mindex.date_obs)
   endif
   x1=xcen-round(dx/2.) & y1=ycen-round(dy/2.)

   xcena1 = (xcen - cindex[0].crpix1 + 1)*cindex[0].cdelt1
   ycena1 = (ycen - cindex[0].crpix1 + 1)*cindex[0].cdelt1
   tmp=min(abs(mtime-ctime[0]),minpos)
   ostr=find_delta(cfnames[0],mfnames[minpos],roi=[xcen,ycen,dx,dy])
   ostrarr=replicate(ostr,ss)
   ostrarr[0]=ostr
   for ii=1,ss-1 do begin

	npos1 = rot_xy(xcena1, ycena1, tstart=cindex[0].date_obs,$
				tend=cindex[ii].date_obs)
	xcenp1 = npos1(0)/cindex[ii].cdelt1 + cindex[ii].crpix1 - 1
	ycenp1 = npos1(1)/cindex[ii].cdelt2 + cindex[ii].crpix2 - 1
	if cindex[ii].crota2 ge 170. then begin
           xcenp1 = round(2.*xcen-xcenp1)
          ycenp1 = round(2.*ycen-ycenp1)
        endif
	;else begin
	 ;       xlo1 = round(xcenp1 - xsize1/2.0)
	 ;       ylo1 = round(ycenp1 - ysize1/2.0)
	  ;   endelse

	tmp=min(abs(mtime-ctime[ii]),minpos)
	ostr=find_delta(cfnames[ii],mfnames[minpos],roi=[xcenp1,ycenp1,dx,dy])
	ostrarr[ii]=ostr


	if (keyword_set(mkmovie)) then begin
	    if (keyword_set(movdir)) then dirname=movdir else dirname='DELTMOV'
	    if (dx/dy le 1.5) then artrk_mkmov,ostrarr,dirname,/hor $
		else artrk_mkmov,ostrarr,dirname
	endif

   endfor

if (keyword_set(mkplot)) then begin

endif

END

