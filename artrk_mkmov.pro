; IDL procedure by Sreejith

; Start		: 11 Oct 2013 00:44
; Last Mod 	: 11 Oct 2013 13:43

;-------------------  Details of the program --------------------------;
PRO artrk_mkmov,strar,movdir,hor=hor
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

    dirname=movdir
    spawn,'mkdir -p '+dirname
    fname = strcompress(dirname+"/"+string(ii,FORMAT='(I05)')+".eps",$
	/remove_all)
    xl=ss1[0] & yl=ss1[1]
    if keyword_set(hor) then xl=2.*xl else yl=2.*yl
    set_plot,'x'
    set_plot,'ps'
    device,bits_per_pixel = 8,/portrait,filename = fname,/color,encapsulated=0,$
	YSIZE=yl/15.,xsize=xl/15.
device,decomposed=0.
    !p.thick = 2.5
    if keyword_set(hor) then !p.multi=[0,2,1] else !p.multi=[0,1,2]
    loadct,0
    plot_map,strar[ii].cmap
tvlct,250,0,0,10
tvlct,0,220,0,20
tvlct,0,0,255,30
    if strar[ii].ndelta ne 0 then $
	plot_map,strar[ii].dltmap,/over,/cont,level=[1.5,-1.5],c_color=[30,10],c_thick=2.5
    loadct,0
    plot_map,strar[ii].mmap
tvlct,250,0,0,10
tvlct,0,220,0,20
tvlct,0,0,255,30
    if strar[ii].ndelta ne 0 then $
	plot_map,strar[ii].dltmap,/over,/cont,level=[1.5,-1.5],c_color=[30,10],c_thick=1.5

    device,/close
    set_plot,'x'
    !p.multi=0.

endfor
END

