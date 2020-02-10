C This file is a part of the GIFA program
C     This software has been developped by the NMR group in GIF/Yvette.
C 
C     Authors : F.Billois,M.A.Delsuc
C
C

        subroutine lect(natomfen,ntachefen,intens,tabindex,
     $                   borne,depchim,largeur,s1,s2,nerr)
C
C           subroutine de saisie des donnees
C
C       variables:
C                 y          dimension des tableaux parcourant        
C		             les taches
C                 z          dimension des tableaux parcourant    
C                            les atomes
C                 depchim    deplacement chimique
C                 largeur    largeur des raies a demi hauteur
C                 freq       frequence du spectro
C                 inten      intensite d une tache
C                 natome     nombre d atome
C                 tabindex   tableau contenant le rang des atomes
C                            de la tache k
C                 ntache     nombre de taches
C                 natomfen   nombre d'atome dans la fenetre
C                 datadec    nom du fichier de deplacement chimique
C                 dataint    nom du fichier corma
C                 datacoor   nom du fichier des coordonnees des 
C                            taches
C                 numresidu  numero du residu
C                 residu     nom du residu
C                 atomnres   atome et numero du residu
C                 s1         taille dimension 1 du fid
C                 s2         taille dimension 2 du fid
C                 ndiag      nombre de tache sur la diagonale
C
C ****************************************************************            

	implicit none

#include "sizeparam.inc"
	real deuxpi
	integer y,z,s1,s2,ndiag

	parameter (z=2000,y=20000)

        parameter (deuxpi=6.283185)

	real borne(4),depchim(z),largeur(z),temp 
	reaL inten(y),freq,borneppm(4),mult,intens(y)

	integer i,j,k,l,nerr,natome,tabindex(y,2),err
	integer aindex(2),natomfen,ntachefen,n

	character*256 dataint,datadec,datacoor
	character*256 line
	character*4 atome
	character*3 numresidu,residu
	character*7 atomnres(z),atom1,atom2

	logical l1,l2,l3,l4,lo(z,2)
	save borneppm,dataint,datadec,datacoor
#f2c        save atomnres,lo

	nerr=0


C *********** entree des bornes et des nom de fichiers *********************
C
C
C              2  <--------- 0
C                            |
C                            | 
C                            |
C                            V 1
C


	call message( 'Name of chemical shift file')
        call getstring2(datadec,err)
        call message( 'Name of Corma file')
	call getstring2(dataint,err)
	call message( 'Name of save file')
	call getstring2(datacoor,err)
        if (err.ne.0) goto 860

 2      call message( 'window limits in PPM')
	call message( 'Chemical shift min dim 1=')
	call getreal2(borneppm(3),err)
	call message( 'Chemical shift min dim 2=')
	call getreal2(borneppm(4),err)
	call message( 'Chemical shift max dim 1=')
	call getreal2(borneppm(1),err)
	call message( 'Chemical shift max dim 2=')
	call getreal2(borneppm(2),err)
        if (err.ne.0) goto 860

	if (borneppm(4).ge.borneppm(2))then
	   call gifaout( 'Error : LL DIM 1 > UR DIM 1 ')
           goto 890
	endif

	if (borneppm(3).ge.borneppm(1))then
           call gifaout( 'Error : LL DIM 2 > UR DIM 2 ')
           goto 890
	endif

C ********* taille du fid ***************************************

 30	call message( 'Data size in dim 1')
	call getint2(s1,err)
	call message( 'Data size in dim 2')
	call getint2(s2,err)
        if (err.ne.0) goto 860

	if (s1.lt.0.or.s2.lt.0)then
	   call gifaout( 'error : size must be =>0')
	   goto 890
	elseif (s1*s2.gt.smxbig)then
	   call gifaout( 'error : size too big')
	   goto 890
	endif 

C ******* Suppression de la diagonale *************************

 40	mult=1.0
        call message( 'Multiplication factor of the diagonal')
	call getreal2(mult,err)
        if (err.ne.0) goto 860

	if (mult.lt.0)then
	   call gifaout( 'error : factor must be >0')
	   goto 890
	endif

	ndiag=0

C ***** lecture du fichier de deplacement chimique *****************

	open(10,file=datadec,status='old',
#vms     *    readonly,
     *       err=800,iostat=nerr)

	natomfen=0
	j=1

	read(10,1040)line  !lit le titre

 5      continue
        read(10,1040,end=10,err=810)line
	read(line,1010,err=810)atome,residu,
     $  numresidu,depchim(j),largeur(j)
      l1=depchim(j).le.borneppm(1).and.depchim(j).ge.borneppm(3)
      l2=depchim(j).le.borneppm(2).and.depchim(j).ge.borneppm(4) 
	if (l1.or.l2)then
	   natomfen=natomfen+1
	   atomnres(natomfen)=atome//numresidu
	   depchim(natomfen)=depchim(j)
	   largeur(natomfen)=largeur(j)
	   lo(natomfen,1)=l1
	   lo(natomfen,2)=l2
C	   write(*,6000)atomnres(natomfen)
	endif
	j=j+1
	goto 5

 10	natome=j-1
C       write(*,*)'Nombre d atomes',natome
        close(10)

C ***** lecture du fichier CORMA *******************************

	open(unit=10,file=dataint,status='old',
#vms     *    readonly,
     *     err=820,iostat=nerr)
        open(12,file=datacoor,status='new',err=840,iostat=nerr)
        write(12,*)' Atom1     chem sh    width     Atom2
     $      chem sh   width        intensity'

	do i=1,3           ! lit les 3 premieres lignes    
	   read(10,1000,err=830)line
	enddo
	read(10,1100,err=830)freq
	read(10,1000,err=830)line   ! lit 1 ligne
	
	k=0
	ntachefen=0

 15     continue
	k=k+1
	l3=.false.
	l4=.false.
	read(10,1000,end=20,err=830)line
	read(line,1030,err=830)atom1,atom2,temp,inten(k)
	do l=1,natomfen !parcours le tableau red de depchim & 
	   if (atom1.eq.atomnres(l))then       ! largueur
	      l3=.true.
	      aindex(1)=l
	   endif
	   if (atom2.eq.atomnres(l))then
	      l4=.true.
	      aindex(2)=l
	   endif
	   if (l3.and.l4)then
	    if (lo(aindex(1),1).and.lo(aindex(2),2))then
                 ntachefen=ntachefen+1
                 intens(ntachefen)=inten(k)
	         tabindex(ntachefen,1)=aindex(1)
	         tabindex(ntachefen,2)=aindex(2)
C	         write(*,4000)atom1,depchim(aindex(1))
C	         write(*,5000)atom2,depchim(aindex(2))
		 write(12,7000,err=850)atom1,depchim(aindex(1)),
     $          largeur(aindex(1)),atom2,depchim(aindex(2))
     $          ,largeur(aindex(2)),intens(ntachefen)
	    endif
              if (lo(aindex(2),1).and.lo(aindex(1),2))then 
                if (atom1.eq.atom2)then ! diagonal
		 intens(ntachefen)=intens(ntachefen)*mult
	        else  
	         ntachefen=ntachefen+1
C	         write(*,4000)atom2,depchim(aindex(2))
C	         write(*,5000)atom1,depchim(aindex(1))
		 write(12,7000,err=850)atom2,depchim(aindex(2)),
     $           largeur(aindex(2)),atom1,depchim(aindex(1))
     $           ,largeur(aindex(1)),inten(k)
		 tabindex(ntachefen,1)=aindex(2)
	         tabindex(ntachefen,2)=aindex(1)
	         intens(ntachefen)=inten(k)
	       endif
	      endif
	       goto 15
          endif
	enddo
	goto 15

C       write(*,1070)' End of file :  ',dataint
 20     write(12,*)'Number of atoms                    :',natome
        write(12,*)'Number of atoms in spectral window :',natomfen
        write(12,*)'Number of peaks in file            :',ntachefen
	close(12)
	close(10)

C ***************** transformation PPM --->Hz *************
        
	do n=1,natomfen
	   depchim(n)=freq*depchim(n)        ! ppm -->Hz
	enddo

	do i=1,4
	   borne(i)=freq*borneppm(i)  !ppm --> Hz
	enddo

	return

C *********** Messages d erreur **********************************

 800   call gifaout('Error in opening file '//datadec)
       goto 890
 810   call gifaout('Error in reading file '//datadec)
       goto 890
 820   call gifaout('Error in opening file '//dataint)
       goto 890
 830   call gifaout('Error in reading file '//dataint)
       goto 890
 840   call gifaout('Error in opening file '//datacoor)
       goto 890
 850   call gifaout('Error in reading file '//datacoor)
       goto 890
 860   call gifaout('Error in reading')
       goto 890
 890   nerr=1 
       return

C ***************** formats ***************************************

 1100	format(' FREQUENCE UTILISEE :          ',f3.0,'MHZ')
 1000   format(a)
 1030   format(a7,x,a7,3x,f7.3,f10.5)
 1040   format(a256)
 1060   format(f7.2,3x,f7.2)
 1010   format(a4,1x,a3,1x,a3,3x,f6.2,3x,f6.1)
 2000   format(a8,3x,a7,3x,a7,3x,i,3x,i)
 1070   format(x,a20,$)
 4000   format(x,a7,3x,f6.2,$)
 5000   format(4X,a7,3x,f6.2)
 6000   format(1x,a)
 7000   format(x,2(a7,2x,2(2x,f6.2,3x)),2x,f10.5)

C ******************************************************************


	end
