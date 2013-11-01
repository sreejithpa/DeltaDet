; IDL procedure by Sreejith

; Start		: 01 Nov 2013 00:35
; Last Mod 	: 01 Nov 2013 02:54

;-------------------  Details of the program --------------------------;
PRO debug_delta,istr,detail=detail
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
    print,'Number of Delta region found: ' +string(istr.ndelta)
    print,'Number of pairs satisfied Condition1: '+string(c1n[0])
    print,'Number of pairs satisfied Condition 1 & 2: '+string(c2n[0])
   if keyword_set(detail) then begin
    for ii=0,c1n[0]-1 do begin
	print,'Pair '+string(ii+1)+' :'
	print,'Dist = '+string(istr.dbstr.distar[c1[ii]])
	if tag_exist(istr.dbstr,'pfrpar') then begin
	    print,'Penumbra Fraction (P, N) : '$
		+string(istr.dbstr.pfrpar[c1[ii]]) + ' , '+$
	         string(istr.dbstr.pfrnar[c1[ii]])
	     if tag_exist(istr.dbstr,'pshar') then $
		 print,'Penumbra share between P-N (pixels) : '+$
		 string(istr.dbstr.pshar[c1[ii]])
	endif else begin
	    if tag_exist(istr.dbstr,'pfrar') then $
		print,' Penumbra Fraction [(P+N)/2] : ' $
		   +string(istr.dbstr.pfrar[c1[ii]])
	endelse

	print,'P-umbra centroid position ([x,y] in pixels): ['$
	    +string(istr.dbstr.uppos[0,y[ii]])+' , '+$
	    string(istr.dbstr.uppos[1,y[ii]]) +']'
	print,'N-umbra centroid position ([x,y] in pixels): ['$
	    + string(istr.dbstr.unpos[0,x[ii]]) +' , '$
	    +string(istr.dbstr.unpos[1,x[ii]])+']'
    endfor
endif
   if c1n[0] eq 0 then begin
       print,'closest Umbral pair distance = '$
	   +string(min(istr.dbstr.distar))
   endif
endif else begin
    print,'Tag dbstr does not exist in this structure'
endelse
END

