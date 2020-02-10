C This file is a part of the GIFA program .
C     This software has been developped by the NMR group in GIF/Yvette
C
C     Authors : F.Billois,M.A.Delsuc
C               NMR Laboratory,
C               ICSN-CNRS
C               91190 GIF/YVETTE   FRANCE
C
C
C     This software cannot be used unless the user has been fully 
C licensed to do so form by the above laboratory.
C     This file cannot be copied,duplicated or used in any other program,
C without written permission from the authors.

	     subroutine delta(nerr)
c OUT	: nerr
c SIDE	: ...
c
c part of the DELTA command




	implicit none


        character*256 line
        character*256 datadec,copy	
	character*4 atome,ato
	character*3 res,residu
	character*1 rep
	integer nres,flag,nresidu,nerr
	real d,l,depchim,largeur
	logical ext

        call message('File name')
	call getstring2(datadec,nerr)
	inquire(file=datadec,exist=ext)

 5	open(10,file=datadec,form='formatted',
     *       iostat=nerr,err=200)
        open(15,form='formatted',status='scratch')
		flag=0

		ato='HN'
                call message('atom')
		call getstring2(ato,nerr)
		res='GLY'
		call message('residu')
		call getstring2(res,nerr)
		nres=1
		call message('number of the residu ')
		call getint2(nres,nerr)
		d=0.0
		call message('Chemical shift')
		call getreal2(d,nerr)
		l=5.0
		call message('Width')
		call getreal2(l,nerr)

	   write(15,110)'    ATOME          DEP.C     L'

	if (ext)then 
            read(10,160)line  
	endif
	    ext=.true.


 10     continue
        read(10,120,end=15,err=200)atome,residu,nresidu,
     $  depchim,largeur 
	if (nres.eq.nresidu.and.flag.eq.0)then
		write(15,120)ato,res,nres,d,l
	        flag=1
	endif
	write(15,120)atome,residu,nresidu,depchim,largeur
	goto 10

 15     continue
	if (flag.eq.0)then
		write(15,120)ato,res,nres,d,l
	endif
	rewind(10)
        rewind(15)
 20     continue
           read(15,160,end=30)copy
           write(10,160)copy
        goto 20

 30	close(10)
        close(15)

        call message(' Another data ?(y/n)')
	rep='y'
        call getstring2(rep,nerr)
        if (rep.eq.'n')then
         return
        endif

	goto 5

 110    format(a)
 120    format(a4,1x,a3,1x,i3,3x,f6.2,3x,f6.1)
 140    format(a3)
 130    format(a4)
 160    format(a256)
 150    format (a1)
 200    return
	end
