; IDL procedure by Sreejith

; Start		: 25 Jun 2013 12:24
; Last Mod 	: 24 Jun 2014 11:43

;-------------------  Details of the program --------------------------;
;PRO delta
;+
; NAME:
;      deltadet
;
; PURPOSE:
;       This program uses SMART to pick up ARs automatically and uses find_delta
;	on those selected ROI.
;
; CALLING SEQUENCE:
;       deltadet,indirc,indirm,[/mkmovie,/moviedir,/plotmap,/savestr


pro deltadet,indirc,indirm,$
    	MKMOVIE=mkmovie,movdir=movdir,MKPLOT=mkplot,plotmap=plotmap,listfname=listfname,savestr=savestr


    poslimit=700  ;Consider ARs position less than poslimit; in arcesec

    ;   if n_elements(inroi) ne 0 then begin
 ;      roi=inroi
  ;     xcen=roi[0] & ycen=roi[1] & dx = roi[2] & dy = roi[3]
  ; endif

   if strlen(indirc) ne 0. then begin
       spawn,'ls '+indirC+'/*continuum.fits',cfnames
       ss=sizeof(cfnames)
       read_sdo,cfnames,cindex,/nodata,/uncomp_delete
       ctime=anytim2tai(cindex.date_obs)
   endif
   if strlen(indirm) ne 0. then begin
       spawn,'ls '+indirM+'/*magnetogram.fits',mfnames
       ss1=sizeof(mfnames)
       read_sdo,mfnames,mindex,/nodata,/uncomp_delete
       mtime=anytim2tai(mindex.date_obs)
   endif
	if (keyword_set(listfname)) then listfname=listfname else listfname='arlist.txt'
   openw,out,listfname,/get_lun
   printf,out,format='(%"\t%s\t\t%s\t\t%s\t\t%s\n")','xcen','ycen','delta','date'
   for ii=0,ss-1 do begin
;   for ii=206,206 do begin
	tmp=min(abs(mtime-ctime[ii]),minpos)
	ar_extract,mfnames[minpos],pospropstr
	if data_type(pospropstr) ne 8 then continue
        roin=size(pospropstr,/dim)
	for kk=0,roin[0]-1 do begin

	    cent=[pospropstr[kk].hcxbnd,pospropstr[kk].hcybnd]
	    if abs(cent[0]) le poslimit then begin
	    ostr=find_delta(cfnames[ii],mfnames[minpos],center=cent,fov=5)
	if data_type(ostr) ne 8 then continue
	    cent=[pospropstr[kk].hcxbnd,pospropstr[kk].hcybnd]
	printf,out,format='(%"\t%f\t%f\t%d\t%s\n")',cent[0],cent[1],ostr.ndelta,ostr.cmap.time

	if (keyword_set(plotmap)) then deltadet_plotmap,ostr

	if(keyword_set(savestr)) then begin
	    dirname='OUTSTR'
	    fname1=strcompress(dirname+"/"+ostr.cmap.time+"_"+string(kk,FORMAT='(I02)')+".sav",/remove_all)
	    spawn,'mkdir -p '+dirname
	    save,ostr,filename=fname1
	endif

	if (keyword_set(mkmovie)) then begin
	    if (keyword_set(movdir)) then dirname=movdir else dirname='DELTMOV'
 	    fname=strcompress(dirname+"/"+ostr.cmap.time+"_"+string(kk,FORMAT='(I02)')+".eps",/remove_all)
	    spawn,'mkdir -p '+dirname

	    deltadet_mkmov,ostr,fname,/hor ;$
		;else artrk_mkmov,ostrarr,dirname
	endif

       endif
       endfor
   endfor
   free_lun,out
END

PRO deltadet_plotmap,ostr

		device,decomposed=0
	        set_plot,'x'
	        !p.multi=[0,2,1]
	        !p.thick=2.5
	        loadct,0
	        plot_map,ostr.cmap
	        tvlct,250,0,0,10
	        tvlct,0,220,0,20
	        tvlct,0,0,255,30
	        if ostr.ndelta ne 0 then begin
		    for jj=0,ostr.ndelta-1 do begin
		        cumap=ostr.dltmap
		        cumap.data=cumap.data*0b
		        circu=cir_mask(cumap.data,ostr.dltcenpx[0,jj],ostr.dltcenpx[1,jj],70)
		        cumap.data(circu)=byte(1)
		        plot_map,cumap,/over,/cont,c_level=[1],c_color=[20],c_thick=2.5
		    endfor
		    plot_map,ostr.dltmap,/over,/cont,levels=[126,127,128,129,130],c_color=[30,30,30,10,10],c_thick=2.0
	        endif

	        loadct,0
	        plot_map,ostr.mmap,dmin=-1000,dmax=1000
	        tvlct,250,0,0,10
	        tvlct,0,220,0,20
	        tvlct,0,0,255,30
	        if ostr.ndelta ne 0 then begin
		    for jj=0,ostr.ndelta-1 do begin
		        cumap=ostr.dltmap
		        cumap.data=cumap.data*0b
		        circu=cir_mask(cumap.data,ostr.dltcenpx[0,jj],ostr.dltcenpx[1,jj],70)
		        cumap.data(circu)=byte(1)
		        plot_map,cumap,/over,/cont,c_level=[1],c_color=[20],c_thick=2.5
		    endfor
		    plot_map,ostr.dltmap,/over,/cont,levels=[126,127,128,129,130],c_color=[30,30,30,10,10],c_thick=2.0
	        endif

end


; IDL procedure by Sreejith

; Start		: 11 Oct 2013 00:44
; Last Mod 	: 28 Nov 2013 20:36

;-------------------  Details of the program --------------------------;
PRO deltadet_mkmov,strar,fname,hor=hor,plotmap=plotmap,yr=yr
;+
; NAME:
;      Function
;
; PURPOSE:
;       This function writes
;
; CALLING SEQUENCE:
;       Function, input,output
device,decomposed=0.
tvlct,250,0,0,10
tvlct,0,220,0,20
tvlct,0,0,255,30
tvlct,150,0,255,40
tvlct,0,150,255,50
tvlct,50,90,255,60
tvlct,255,255,255,1
tvlct,200,200,200,2


ss=size(strar,/dim)
ss1=size(strar.cmap.data,/dim)
for ii=0,ss[0]-1 do begin
;for ii=206,206 do begin

   ; fname = strcompress(dirname+"/"+string(ii,FORMAT='(I05)')+".eps",$
;	/remove_all)
    xl=ss1[0] & yl=ss1[1]
    if keyword_set(hor) then xl=2.*xl else yl=2.*yl
    set_plot,'x'
    set_plot,'ps'
    device,bits_per_pixel = 8,/portrait,filename = fname,/color,encapsulated=0,$
	YSIZE=yl/15.,xsize=xl/15.
    device,decomposed=0.
    !p.charthick = 2.5
    !p.charsize = 2.0
    if keyword_set(hor) then !p.multi=[0,2,1] else !p.multi=[0,1,2]
    loadct,0
    plot_map,strar[ii].cmap,dmin=75000,dmax=20000;,yrange=[-400,-250]
    tvlct,250,0,0,10
    tvlct,0,220,0,20
    tvlct,0,0,255,30
    tvlct,255,0,255,40
    if strar[ii].ndelta ne 0 then begin
	for jj=0,strar[ii].ndelta-1 do begin
	    cumap=strar[ii].dltmap
	    cumap.data=cumap.data*0b
	    circu=cir_mask(cumap.data,strar[ii].dltcenpx[0,jj],strar[ii].dltcenpx[1,jj],70)
	    cumap.data(circu)=byte(1)
	    plot_map,cumap,/over,/cont,c_level=[1],c_color=[40],c_thick=3.5
	endfor
;	plot_map,strar[ii].mskmap,/over,/cont,levels=[60,150],c_color=[30,20],c_thick=7.0,max_val=160
;	plot_map,strar[ii].mskmap,/over,/cont,levels=[byte(60),byte(200)],c_color=[30,10],c_thick=7.0
	plot_map,strar[ii].dltmap,/over,/cont,levels=[126,127,128,129,130],c_color=[30,30,30,10,10],c_thick=3.0
    endif
    loadct,0
    plot_map,strar[ii].mmap,dmin=-1000,dmax=1000;,yrange=[-150,-300]
tvlct,250,0,0,10
tvlct,0,220,0,20
tvlct,0,0,255,30
tvlct,255,0,255,40
    if strar[ii].ndelta ne 0 then begin
	for jj=0,strar[ii].ndelta-1 do begin
	    cumap=strar[ii].dltmap
	    cumap.data=cumap.data*0b
	    circu=cir_mask(cumap.data,strar[ii].dltcenpx[0,jj],strar[ii].dltcenpx[1,jj],70)
	    cumap.data(circu)=byte(1)
	    plot_map,cumap,/over,/cont,c_level=[1],c_color=[40],c_thick=5.5

	endfor
	;plot_map,strar[ii].mskmap,/over,/cont,levels=[60,150],c_color=[30,20],c_thick=7.0,max_val=160
	;plot_map,strar[ii].mskmap,/over,/cont,levels=[byte(60),byte(200)],c_color=[30,10],c_thick=7.0
	plot_map,strar[ii].dltmap,/over,/cont,levels=[126,127,128,129,130],c_color=[30,30,30,10,10],c_thick=4.0
    endif
    loadct,0
    device,/close
    set_plot,'x'
    if (keyword_set(plotmap)) then begin
	loadct,0
	plot_map,strar[ii].cmap
tvlct,250,0,0,10
tvlct,0,220,0,20
tvlct,0,0,255,30
    if strar[ii].ndelta ne 0 then begin
	for jj=0,strar[ii].ndelta-1 do begin
	    cumap=strar[ii].dltmap
	    cumap.data=cumap.data*0b
	    circu=cir_mask(cumap.data,strar[ii].dltcenpx[0,jj],strar[ii].dltcenpx[1,jj],70)
	    cumap.data(circu)=byte(1)
	    plot_map,cumap,/over,/cont,c_level=[1],c_color=[20],c_thick=2.5

	endfor
	plot_map,strar[ii].dltmap,/over,/cont,levels=[126,127,128,129,130],c_color=[30,30,30,10,10],c_thick=1.5
    endif
    loadct,0
    plot_map,strar[ii].mmap,dmin=-1000,dmax=1000
tvlct,250,0,0,10
tvlct,0,220,0,20
tvlct,0,0,255,30
    if strar[ii].ndelta ne 0 then begin
	for jj=0,strar[ii].ndelta-1 do begin
	    cumap=strar[ii].dltmap
	    cumap.data=cumap.data*0b
	    circu=cir_mask(cumap.data,strar[ii].dltcenpx[0,jj],strar[ii].dltcenpx[1,jj],70)
	    cumap.data(circu)=byte(1)
	    plot_map,cumap,/over,/cont,c_level=[1],c_color=[20],c_thick=2.5

	endfor
	plot_map,strar[ii].dltmap,/over,/cont,levels=[126,127,128,129,130],c_color=[30,30,30,10,10],c_thick=1.5
	wait,1
    endif
    loadct,0
endif
    !p.multi=0.


endfor
END

