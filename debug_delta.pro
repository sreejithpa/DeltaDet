; IDL procedure by Sreejith

; Start		: 01 Nov 2013 00:35
; Last Mod 	: 01 Nov 2013 02:54
; P.Higgins     : 01 Nov 2013 17:17 - Added option to print to file
;                                     for NRT version.

;-------------------  Details of the program --------------------------;

PRO debug_delta_print, instring, filename=filename
; NAME:
;      DEBUG_DELTA_PRINT
;
; PURPOSE:
;       This routine prints to the terminal or prints to a file
;
; CALLING SEQUENCE:
;       debug_delta_print, 'blah',filename='blah.txt'
if n_elements(filename) eq 1 then begin
   openw, lun, filename,/get_lun,/append
   PRINTF, lun, instring 
   close,/all
endif else print,instring

end

;----------------------------------------------------------------------;

PRO debug_delta,istr,detail=detail, filename=filename
;+
; NAME:
;      Function
;
; PURPOSE:
;       This function writes
;
; CALLING SEQUENCE:
;       Function, input,output
if tag_exist(istr,'dbstr') then begin
    c1=where(istr.dbstr.dltpos eq 1)
    c1n=size(c1,/dim)
    c2=where(istr.dbstr.dltpos eq 2)
    c2n=size(c2,/dim)
	s= size(istr.dbstr.dltpos)
	y = DOUBLE( FIX( DOUBLE( c1 ) / s[1] ) )
	x = DOUBLE( c1 - ( y * s[1] ) )
    debug_delta_print,'Number of Delta region found: ' +string(istr.ndelta), filename=filename
    debug_delta_print,'Number of pairs satisfied Condition1: '+string(c1n[0]), filename=filename
    debug_delta_print,'Number of pairs satisfied Condition 1 & 2: '+string(c2n[0]), filename=filename
   if keyword_set(detail) then begin
    for ii=0,c1n[0]-1 do begin
	debug_delta_print,'Pair '+string(ii+1)+' :', filename=filename
	debug_delta_print,'Dist = '+string(istr.dbstr.distar[c1[ii]]), filename=filename
	if tag_exist(istr.dbstr,'pfrpar') then begin
	    debug_delta_print,'Penumbra Fraction (P, N) : '$
		+string(istr.dbstr.pfrpar[c1[ii]]) + ' , '+$
	         string(istr.dbstr.pfrnar[c1[ii]]), filename=filename
	     if tag_exist(istr.dbstr,'pshar') then $
		 debug_delta_print,'Penumbra share between P-N (pixels) : '+$
		 string(istr.dbstr.pshar[c1[ii]]), filename=filename
	endif else begin
	    if tag_exist(istr.dbstr,'pfrar') then $
		debug_delta_print,' Penumbra Fraction [(P+N)/2] : ' $
		   +string(istr.dbstr.pfrar[c1[ii]]), filename=filename
	endelse

	debug_delta_print,'P-umbra centroid position ([x,y] in pixels): ['$
	    +string(istr.dbstr.uppos[0,y[ii]])+' , '+$
	    string(istr.dbstr.uppos[1,y[ii]]) +']', filename=filename
	debug_delta_print,'N-umbra centroid position ([x,y] in pixels): ['$
	    + string(istr.dbstr.unpos[0,x[ii]]) +' , '$
	    +string(istr.dbstr.unpos[1,x[ii]])+']', filename=filename
    endfor
endif
   if c1n[0] eq 0 then begin
       debug_delta_print,'closest Umbral pair distance = '$
	   +string(min(istr.dbstr.distar)), filename=filename
   endif
endif else begin
    debug_delta_print,'Tag dbstr does not exist in this structure', filename=filename
endelse
END

