C ambp.inc
C
C fichier temporaire contenant les commandes de manipulation des amibes 

      elseif (command.eq.'AMBPK') then
        if (dim.ne.2) call gifaout("Command available in 2D only!")
	call message("F1 coordinate of pixel?")
  	call getint2(yy,error)
 	if (yy.gt.si1im) goto 108
	call message("F2 coordinate of pixel?")
  	call getint2(xx,error)
 	if (xx.gt.si2im) goto 108
        zz = getmaskno(xx,yy)
	call gifaouti("Peak number :",zz)

      elseif (command.eq.'AMBPUT') then
        if (dim.ne.2) call gifaout("Command available in 2D only!")
	call message("Peak number ?")
  	call getint2(zz,error)
 	if (zz.gt.nbpic2d) goto 147
	call message("F1 coordinate of pixel?")
  	call getint2(yy,error)
 	if (yy.gt.si1im) goto 108
	call message("F2 coordinate of pixel?")
  	call getint2(xx,error)
 	if (xx.gt.si2im) goto 108
        call setpixmask(xx,yy,zz,error)
	if (error.ne.0) goto 90

      elseif (command.eq.'AMBREM') then
        if (dim.ne.2) call gifaout("Command available in 2D only!")
	call message("Peak number ?")
  	call getint2(zz,error)
 	if (zz.gt.nbpic2d) goto 147
	call message("F1 coordinate of pixel?")
  	call getint2(yy,error)
 	if (yy.gt.si1im) goto 108
	call message("F2 coordinate of pixel?")
  	call getint2(xx,error)
 	if (xx.gt.si2im) goto 108
        call rempixmask(xx,yy,zz,error)
	if (error.ne.0) goto 90
	 
      elseif (command.eq.'AMBDEL') then
	call message("Peak number ?")
  	call getint2(zz,error)
 	if (zz.gt.nbpic2d) goto 147
        call delmask(zz)

      elseif (command.eq.'AMBINT') then
	call message("Peak number ?")
  	call getint2(zz,error)
 	if (zz.gt.nbpic2d) goto 147
        call calcmask(zz,plane2d,si1im,si2im,noise,shift,signpic)
	if (error.ne.0) goto 90

      elseif (command.eq.'AMBWRITE') then
        call message( 'Enter file-name')
        call getstring2(name,error)
        call storemask(name,error)
	if (error.ne.0) goto 90

      elseif (command.eq.'AMBREAD') then
        call message( 'Enter file-name')
        call getstring2(name,error)
        call readmask(name,plane2d,si1im,si2im,error)
	if (error.ne.0) goto 90


