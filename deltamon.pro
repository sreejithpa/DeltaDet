; IDL procedure by Sreejith

; Start		: 07 Aug 2014 19:35
; Last Mod 	: 15 Aug 2014 22:23

;-------------------  Details of the program --------------------------;
PRO deltamon
;+
; NAME:
;      Function
;
; PURPOSE:
;       This function writes
;
; CALLING SEQUENCE:
;       Function, input,output
id="/Users/sreejith/Solar/Delta/NRT/Data/Ic"
md="/Users/sreejith/Solar/Delta/NRT/Data/Mag"
spawn, "mkdir -p "+id
spawn, "mkdir -p "+md

get_hmi_latest,md,mfname,err=merr
spawn, "rm -rf "+md+"/SUM*"
get_chmi_latest,id,cfname,err=cerr
spawn, "rm -rf "+id+"/SUM*"
if cerr ne -1 and merr ne -1 then begin
	deltadet1,cfname,mfname,fdostr,/mkmovie,movdir='/Users/sreejith/Solar/Delta/NRT/ARImages'$
	    ,listfname='/Users/sreejith/Solar/Delta/NRT/tmparlist.txt'
	if fdostr.ndelta ge 1 then begin
	    delposstr=' '
	    for ii=0,fdostr.ndelta -1 do begin

	    delposstr=delposstr+"["+string(fdostr.dltcen[0,ii])+","+string(fdostr.dltcen[1,ii])+"],"
	    endfor
	    mailstr="SMART-DF live detected "+string(fdostr.ndelta,FORMAT='(I3)')+$
		" deltas at positions \([x,y] in arcsec\) "+ delposstr+" on "+$
		fdostr.cmap.time+". Visit http://sungaze.org/smart-df-live for more details"
	    subjstr="'SMART-DF detects a Delta-spot'"
	    ;toadstr=' padinhas@tcd.ie scullie@tcd.ie bloomfis@tcd.ie gallagpt@tcd.ie '
	    toadstr=' padinhas@tcd.ie' 
	    spawn, "echo "+mailstr+"|Mail -s "+subjstr+toadstr
	endif
endif
spawn, "cat /Users/sreejith/Solar/Delta/NRT/tmparlist.txt >> /Users/sreejith/Solar/Delta/NRT/arlist.txt"
END
