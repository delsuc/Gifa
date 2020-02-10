C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in CBS/Montpellier
C
C     Authors :       M.A.Delsuc
C                     C.B.S. Faculte de Pharmacie
C                     34060 Montpellier   FRANCE
C
C     This software cannot be used unless the user have been fully
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.

C This file holds all the utilities needed for 2D line-fitting,
C Line-fitting itself, is done by calling the generic 
C Levenberg-Maquardt minimiser

      subroutine prepprm2d(tofit,nbp,tdx,start2,start1,end2,end1,
     *           verbose,actif,err)
c IN	: tofit,start2,start1,end2,end1,verbose
c OUT	: tdx,actif
c INOUT	: nbp,err
c
c Select pics which are chosed, as found in peak()
c Select parameters,between start2,start1,end2,end1 which will be fitted
c Number of peaks found is nbp
c as found in peak()
c

      implicit none
#include "sizeparam.inc"
#include "peaksize.inc"
#include "fitv.inc"

      integer  start2,start1,end2,end1,nbp,verbose,err,tdx(4*nbp)
      character*256 tofit,st

      integer  i,actif,buff
      
c Test si le nb de pics est sup. a une valeur max.
      if (nbp.gt.tlmxpk2) then
        write(st,*) 'Peak table truncated to',tlmxpk2,' peaks'
        call gifaout(st) 
        nbp = tlmxpk2
      endif

      if (nbp.le.0) then
         call gifaerr('Peak-Table should not be empty !')
         err=1
         return
      endif
      actif = 0
      buff = 0
c Boucle permettant de choisir les pics a optimiser
c   et leurs parametres 
      do i=1,nbp
         if (peak2d(i,1).ge.start2 .and. peak2d(i,1).le.end2
     *      .and. peak2d(i,4).ge.start1 .and. peak2d(i,4).le.end1) then
              if (tofit.eq.'GAUSS') then 
                  peak2d(i,7) = 2.0
               elseif (tofit.eq.'LORENTZ') then
                  peak2d(i,7) = 1.0 
               else
                  err = 1
                  return
               endif
               buff = buff+1
               tdx(actif+1) = 6*(i-1)+1
               tdx(actif+2) = 6*(i-1)+2
               tdx(actif+3) = 6*(i-1)+3
               tdx(actif+4) = 6*(i-1)+4
               tdx(actif+5) = 6*(i-1)+5
               actif = actif + 5
               if (verbose.eq.1) then
                  write(st,*) i,
     *               peak2d(i,1),peak2d(i,2),peak2d(i,3),peak2d(i,4),
     *               peak2d(i,5)
                  call message('Initial values :')
                  call message(st)
               endif
         endif
      enddo

      if (verbose.eq.1)
     * call gifaouti('Number of Peaks to be fitted :      ',buff)
      if (verbose.eq.1)
     * call gifaouti('Number of parameters to be fitted : ',actif)
      return
      end
      

C**********************************************************************      
      subroutine selectprm2d(nbp,tdx,start2,start1,end2,end1,
     *          verbose,actif,err)
c IN	: start2,start1,end2,end1,verbose
c OUT	: tdx,actif
c INOUT	: nbp,err
c
c Select pics which are chosed, as found in peak()
c Select parameters,between start2,start1,end2,end1, which will be fitted
c Number of peaks found is nbp
c
c

      implicit none
#include "sizeparam.inc"
#include "peaksize.inc"
#include "fitv.inc"

      integer  start2,start1,end2,end1,nbp,verbose,err,tdx(4*nbp)
 
      integer  i,actif,buff
      character*256 st,rep
      
c Test si le nb de pics est sup. a une valeur max.
      if (nbp.gt.tlmxpk2) then
        write(st,*) 'Peak table truncated to',tlmxpk2,' peaks'
        call gifaout(st) 
        nbp = tlmxpk2
      endif

      if (nbp.le.0) then
         call gifaerr('Peak-Table should not be empty !')
         err=1
         return
      endif
      rep = 'yes'
      actif = 0
      buff = 0
c Boucle permettant de choisir les pics a optimiser
c   et leurs parametres 
      i = 0
c goto next peak in window => promt for peaks in window
 100  dowhile (i.le.nbp .and.
     *      .not.(peak2d(i,1).ge.start2 .and. peak2d(i,1).le.end2
     *         .and. peak2d(i,4).ge.start1 .and. peak2d(i,4).le.end1))
          i = i+1
      enddo
      if (i.gt.nbp) i = 0
      call message ('Enter peak number to fit, finish with 0')
      call getint2(i,err)
      if (err.ne.0) return
      if (i.lt.0 .or. i.gt.nbp) then
         err = 1
         return
      endif
c preset peaks to Lorentz...
      if (i.ne.0) then
    	       buff = buff + 1
    	       if (peak2d(i,7).eq.2.0) then
                  rep = 'Gauss'
               else
                  rep = 'Lorentz'
               endif
c Choix de la fonction cible pour le pic
               call message('    Type of line-shape : Gauss / Lorentz')
               call getstring2(rep,err)
               if (err.ne.0) return 
               call uppercase(rep,80)
               if (rep.eq.'GAUSS') then 
                  peak2d(i,7) = 2.0
               elseif (rep.eq.'LORENTZ') then
                  peak2d(i,7) = 1.0 
               else
                  err = 1
                  return
               endif
               rep = 'yes'
c Optimiser ou non l'amplitude du pic
               call message('    Fit Amplitude (yes / no) ')
	       call getbool2(rep,'toto',err)
	       if (err.ne.0) return
               if (rep.eq.'yes') then
                  actif        = actif + 1
                  tdx(actif) = 6*(i-1)+1
               endif
c Optimiser ou non la frequence du pic 
               call message('    Fit F1 Frequency (yes / no) ')
	       call getbool2(rep,'toto',err)
	       if (err.ne.0) return
               if (rep.eq.'yes') then
                  actif        = actif + 1
                  tdx(actif) = 6*(i-1)+2
               endif 
c Optimiser ou non la largeur du pic
               call message('    Fit F1 Width (yes / no) ')
	       call getbool2(rep,'toto',err)
	       if (err.ne.0) return
               if (rep.eq.'yes') then
                  actif        = actif + 1
                  tdx(actif) = 6*(i-1)+3
               endif 
c Optimiser ou non la frequence du pic 
               call message('    Fit F2 Frequency (yes / no) ')
	       call getbool2(rep,'toto',err)
	       if (err.ne.0) return
               if (rep.eq.'yes') then
                  actif        = actif + 1
                  tdx(actif) = 6*(i-1)+4
               endif 
c Optimiser ou non la largeur du pic
               call message('    Fit F2 Width (yes / no) ')
	       call getbool2(rep,'toto',err)
	       if (err.ne.0) return
               if (rep.eq.'yes') then
                  actif        = actif + 1
                  tdx(actif) = 6*(i-1)+5
               endif 
               if (verbose.eq.1) then
                  write(st,*) i,
     *               peak2d(i,1),peak2d(i,2),peak2d(i,3),peak2d(i,4),
     *               peak2d(i,5)
                  call message('Initial values :')
                  call message(st)
               endif
               i = i + 1
               goto 100     ! and loop
      endif

      if (verbose.eq.1)
     * call gifaouti('Number of Peaks to be fitted :      ',buff)
      if (verbose.eq.1)
     * call gifaouti('Number of parameters to be fitted : ',actif)
      return
      end
      

C**********************************************************************      
      subroutine initprm2d(nbp,code,err)

c IN : code
c INOUT : nbp, err
c
c The following subroutine initializes the 6*nbpic paramaters (prm)
c that will have to be fitted in the 2d spectrum
c
c code is the size (in F2) of the area that will used for the fit
c

      implicit none
#include "sizeparam.inc"
#include "peaksize.inc"
#include "fitv.inc"
 
      integer  nbp,code,err
  
      integer  i
      character*256 st       

      if (nbp.gt.tlmxpk2) then
        write(st,*) 'Peak table truncated to',tlmxpk2,' peaks'
        call gifaout(st) 
        nbp = tlmxpk2
      endif
      if (nbp.le.0) then
         call gifaout('Peak-Table empty !')
         err=1
         return
      endif
c Copie les param. dans un tableau qui sera utilise pour l'optimisation
      do i=1,nbp
         prmr2d(6*(i-1)+1)=peak2d(i,3)
         prmr2d(6*(i-1)+2)=peak2d(i,1)
         prmr2d(6*(i-1)+3)=peak2d(i,2)
         prmr2d(6*(i-1)+4)=peak2d(i,4)
         prmr2d(6*(i-1)+5)=peak2d(i,5)
         if (peak2d(i,7) .eq.0.0) then
            prmr2d(6*(i-1)+6) = 1.0
         else
            prmr2d(6*(i-1)+6)=peak2d(i,7)
         endif
      enddo

c code is needed to fit in 2D, tells how i and j and coded in a single value
      prmr2d(6*nbp+1) = float(code)

      return
      end


C**********************************************************************
      subroutine postprm2d(chi2,nbpt,cnt,cov,tdx,ajus,verbose)
c IN	: cnt,verbose,chi2,nbpt,...
c SIDE	: 1d peak table
C this subroutine copies the working buffer of line fitting
c back to the 1d peak table
c called at the end of the line fitting
c
c

      implicit none
#include "sizeparam.inc"
#include "peaksize.inc"
#include "fitv.inc"

      integer cnt,verbose,ajus,nbpt
      integer tdx(6*cnt)
      real cov(6*cnt+1,6*cnt+1),chi2

      integer i,n,p
      character*256 st
      real z,scale
 
      if (verbose.eq.1) then
        call gifaouti('Number of peak : ',cnt)
        call gifaout('Final values :')
      endif
c Copie le tableau-buffer dans le tableau de param.
      do i=1,cnt
         peak2d(i,3) = prmr2d(6*(i-1)+1)
         peak2d(i,1) = prmr2d(6*(i-1)+2)
         peak2d(i,2) = prmr2d(6*(i-1)+3)
         peak2d(i,4) = prmr2d(6*(i-1)+4)
         peak2d(i,5) = prmr2d(6*(i-1)+5)
         if (verbose.eq.1) then
                  write(st,*) i,
     *               peak2d(i,1),peak2d(i,2),peak2d(i,3),peak2d(i,4),
     *               peak2d(i,5)
            call gifaout(st)
         endif
      enddo
      if (chi2 .lt. nbpt/4 .or. chi2 .gt. 4*nbpt) then
         if (chi2 .lt. nbpt/4) then
            call gifaout
     *    ('Warning, Model is overfitting or Noise is overestimated')
         else
            call gifaout
     *    ('Warning, Model is wrong or Noise is underestimated')
         endif
         call gifaout('   reestimating error bars')
         scale = sqrt(chi2/(nbpt-2))
         if (verbose .ne. 0) then
            write (*,*) 'scaling for error bars:',scale
         endif
      else
         scale = 1.0
      endif
      do i=1,ajus
C search n,p such that tdx(i) = 6*(n-1)+p
         n = tdx(i)/6 + 1
         p = tdx(i) - 6*(n-1)
         z = scale*2.0*sqrt(cov(i,i))
         if (verbose .ne. 0) then
            write (*,*) 'Error for peak #',n,' param',p,':',z
         endif
         if (p .eq. 1) then         ! amp
            peak2d(n,6) = z
         else if (p .eq. 2) then    ! F1 pos
            peak2d(n,10) = z
         else if (p.eq.3) then      ! f1 width
            peak2d(n,11) = z
         else if (p .eq. 4) then    ! F2 pos
            peak2d(n,8) = z
         else if (p.eq.5) then      ! f2 width
            peak2d(n,9) = z
         else
            call gifaout('*** Error in LINEFIT / postprm1d')
         endif
      enddo

      return
      end



C**********************************************************************
C function and derivatives for line fitting
C**********************************************************************
      subroutine flogau2d(size,param,x,y)
c IN	: size,param,x
c OUT	: y
c computes the value of the 2D lorent/gaussian line in x, the parameters are 
c found in param
c
c
c g2d(x) = A g1d(i) g1d(j)
c
c with i = mod(x,code)   j = x/code    i.e. x = j*code + i
c and g1d are 1d lorent/gaussian function with amplitude 1.0
c
c
      implicit none
      integer  size
      real     param(size),y,x

      integer  i
      real ix,jx,p1(4),p2(4),g1,g2
      
      ix = amod(x,param(size))
      jx = float( int(x/param(size)) )
      y = 0.0
      
      p1(1) = 1.0
      p2(1) = 1.0

      do i=1,size-2,6
         p1(2) = param(i+1)	! F2 loc
         p1(3) = param(i+2)	! F2 width
         p1(4) = param(i+5)	! F2 type
         p2(2) = param(i+3)	! idem in F1
         p2(3) = param(i+4)
         p2(4) = param(i+5)
         call flogau(4,p1,ix,g1)
         call flogau(4,p2,jx,g2)
         y = y + param(i)*g1*g2
      enddo
      return
      end


C**********************************************************************
C**********************************************************************
      subroutine dflogau2d(size,param,x,dyda)
c IN	: size,param,x
c OUT	: dyda
c computes the derivative of the 2D lorent/gaussian line in x, the parameters are 
c found in param
c
c dg2d/dx = a [ dg1/dx g2  +   dg2/dx g1]    (only one term is !=0 )
c
      implicit none
      integer  size
      real     param(size),dyda(size),x

      integer  i
      real ix,jx,p1(4),p2(4),g1,g2,d1(4),d2(4)
      
      ix = amod(x,param(size))
      jx = float( int(x/param(size)) )
      p1(1) = 1.0
      p2(1) = 1.0


      do i=1,size-2,6
         p1(2) = param(i+1)
         p1(3) = param(i+2)
         p1(4) = param(I+5)
         p2(2) = param(i+3)
         p2(3) = param(i+4)
         p2(4) = param(I+5)
         call flogau(4,p1,ix,g1)
         call flogau(4,p2,jx,g2)
         call dflogau(4,p1,ix,d1)
         call dflogau(4,p2,jx,d2)
         dyda(i)   = g1*g2
         dyda(i+1) = param(i)*d1(2)*g2
         dyda(i+2) = param(i)*d1(3)*g2
         dyda(i+3) = param(i)*d2(2)*g1
         dyda(i+4) = param(i)*d2(3)*g1
         dyda(i+5) = 0.0
      enddo
      return
      end


