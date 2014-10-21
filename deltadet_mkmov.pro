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
;device,decomposed=0.
;tvlct,250,0,0,10
;tvlct,0,220,0,20
;tvlct,0,0,255,30
;tvlct,150,0,255,40
;tvlct,0,150,255,50
;tvlct,50,90,255,60
;tvlct,255,255,255,1
;tvlct,200,200,200,2


ss=size(strar,/dim)
ss1=size(strar.cmap.data,/dim)
for ii=0,ss[0]-1 do begin
;for ii=206,206 do begin

   ; fname = strcompress(dirname+"/"+string(ii,FORMAT='(I05)')+".eps",$
;	/remove_all)
    xl=ss1[0] & yl=ss1[1]
    if keyword_set(hor) then xl=2.*xl else yl=2.*yl
    ;set_plot,'x'
    set_plot,'ps'
    device,bits_per_pixel = 8,/portrait,filename = fname,/color,encapsulated=0,$
	YSIZE=yl/15.,xsize=xl/15.
    device,decomposed=0.
    !p.charthick = xl/200
    !p.charsize = yl/200
    if keyword_set(hor) then !p.multi=[0,2,1] else !p.multi=[0,1,2]
    loadct,0
    plot_map,strar[ii].cmap;,yrange=[-400,-250]
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
    if (keyword_set(plotmap)) then begin
    set_plot,'x'
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

