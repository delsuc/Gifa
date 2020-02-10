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

C This file holds all the utilities needed for 1D line-fitting,
C Line-fitting itself, is done by calling the generic 
c Levenberg-Maquardt minimiser


      subroutine prepprm1d(tofit,nbp,tdx,start,end,verbose,actif,err)
c IN	: tofit,start,end,verbose
c OUT	: tdx,actif
c INOUT	: nbp,err
c
c Select pics which are chosed, as found in peak()
c Select parameters,between start and end,which will be fitted
c Number of peaks found is nbp
c
c

      implicit none
#include "sizeparam.inc"
#include "peaksize.inc"
#include "fitv.inc"

      integer  start,end,nbp,verbose,err,tdx(4*nbp)
      character*256 tofit,st

      integer  i,actif,buff
      
c Test si le nb de pics est sup. a une valeur max.
      if (nbp.gt.tlmxpk1) then
        write(st,*) 'Peak table truncated to',tlmxpk1,' peaks'
        call gifaout(st) 
        nbp = tlmxpk1
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
         if (peak1d(i,1).ge.start .and. peak1d(i,1).le.end) then
              if (tofit.eq.'GAUSS') then 
                  peak1d(i,5) = 2.0
               elseif (tofit.eq.'LORENTZ') then
                  peak1d(i,5) = 1.0 
               else
                  err = 1
                  return
               endif
               buff = buff+1
               tdx(actif+1) = 4*(i-1)+1
               tdx(actif+2) = 4*(i-1)+2
               tdx(actif+3) = 4*(i-1)+3
               actif = actif + 3
               if (verbose.eq.1) then
                  write(st,*) i,
     *               peak1d(i,1),peak1d(i,2),peak1d(i,3)
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
      subroutine selectprm1d(nbp,tdx,start,end,verbose,actif,err)
c IN	: start,end,verbose
c OUT	: tdx,actif
c INOUT	: nbp,err
c
c Select pics which are chosed, as found in peak()
c Select parameters,between start and end,which will be fitted
c Number of peaks found is nbp
c
c

      implicit none
#include "sizeparam.inc"
#include "peaksize.inc"
#include "fitv.inc"

      integer  start,end,nbp,verbose,err,tdx(4*nbp)
 
      integer  i,actif,buff
      character*256 st,rep
      
c Test si le nb de pics est sup. a une valeur max.
      if (nbp.gt.tlmxpk1) then
        write(st,*) 'Peak table truncated to',tlmxpk1,' peaks'
        call gifaout(st) 
        nbp = tlmxpk1
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
     *      .not. (peak1d(i,1).ge.start .and. peak1d(i,1).le.end))
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
    	       if (peak1d(i,5).eq.2.0) then
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
                  peak1d(i,5) = 2.0
               elseif (rep.eq.'LORENTZ') then
                  peak1d(i,5) = 1.0 
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
                  tdx(actif) = 4*(i-1)+1
               endif
c Optimiser ou non la frequence du pic 
               call message('    Fit Frequency (yes / no) ')
	       call getbool2(rep,'toto',err)
	       if (err.ne.0) return
               if (rep.eq.'yes') then
                  actif        = actif + 1
                  tdx(actif) = 4*(i-1)+2
               endif 
c Optimiser ou non la largeur du pic
               call message('    Fit Width (yes / no) ')
	       call getbool2(rep,'toto',err)
	       if (err.ne.0) return
               if (rep.eq.'yes') then
                  actif        = actif + 1
                  tdx(actif) = 4*(i-1)+3
               endif 
               if (verbose.eq.1) then
                  write(st,*) i,
     *               peak1d(i,1),peak1d(i,2),peak1d(i,3)
                  call message('Initial values :')
                  call message(st)
               endif
               i = i+1
               goto 100     ! and loop
      endif

      if (verbose.eq.1)
     * call gifaouti('Number of Peaks to be fitted :      ',buff)
      if (verbose.eq.1)
     * call gifaouti('Number of parameters to be fitted : ',actif)
      return
      end
      

C**********************************************************************      
      subroutine initprm1d(nbp,err)

c INOUT : nbp, err
c
c The following subroutine initializes the 4*nbpic paramaters (prm)
c that will have to be fitted in the 1d spectrum
c
c

      implicit none
#include "sizeparam.inc"
#include "peaksize.inc"
#include "fitv.inc"
 
      integer  nbp,err
  
      integer  i
      character*256 st       

      if (nbp.gt.tlmxpk1) then
        write(st,*) 'Peak table truncated to',tlmxpk1,' peaks'
        call gifaout(st) 
        nbp = tlmxpk1
      endif
      if (nbp.le.0) then
         call gifaout('Peak-Table empty !')
         err=1
         return
      endif
c Copie les param. dans un tableau qui sera utilise pour l'optimisation
      do i=1,nbp
         prmr1d(4*(i-1)+1)=peak1d(i,3)
         prmr1d(4*(i-1)+2)=peak1d(i,1)
         prmr1d(4*(i-1)+3)=peak1d(i,2)
         if (peak1d(i,5) .eq.0.0) then
            prmr1d(4*(i-1)+4) = 1.0
         else
            prmr1d(4*(i-1)+4)=peak1d(i,5)
         endif
      enddo
      return
      end


C**********************************************************************
      subroutine postprm1d(chi2,nbpt,cnt,cov,tdx,ajus,verbose)
c IN	: cnt,verbose,cov,tdx,ajus,chi2,nbpt
c SIDE	: 1d peak table
C this subroutine copies the working buffer of line fitting
c back to the 1d peak table
C
C as well as extracts the errors from the covariance matrix
c computed at the end of the line fitting
c
c

      implicit none
#include "sizeparam.inc"
#include "peaksize.inc"
#include "fitv.inc"

      integer cnt,verbose,ajus,nbpt
      integer tdx(4*cnt)
      real cov(4*cnt,4*cnt),chi2

      integer i,n,p
      real z,scale
      character*256 st
 
      if (verbose.eq.1) then
        call gifaouti('Number of peak : ',cnt)
        call gifaout('Final values :')
      endif
c Copie le tableau-buffer dans le tableau de param.
      do i=1,cnt
         peak1d(i,3) = prmr1d(4*(i-1)+1)
         peak1d(i,1) = prmr1d(4*(i-1)+2)
         peak1d(i,2) = prmr1d(4*(i-1)+3)
         if (verbose.eq.1) then
            write(st,*) i,
     *             peak1d(i,1),peak1d(i,2),peak1d(i,3)
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
C search n,p such that tdx(i) = 4*(n-1)+p
         n = tdx(i)/4 + 1
         p = tdx(i) - 4*(n-1)
         z = scale*2.0*sqrt(cov(i,i))
         if (verbose .ne. 0) then
            write (*,*) 'Error for peak #',n,' param',p,':',z
         endif
         if (p .eq. 1) then         ! amp
            peak1d(n,8) = z
         else if (p .eq. 2) then    ! pos
            peak1d(n,6) = z
         else if (p.eq.3) then      ! width
            peak1d(n,7) = z
         else
            call gifaout('*** Error in LINEFIT / postprm1d')
         endif
      enddo
      return
      end


C**********************************************************************
C function and derivatives for line fitting
C**********************************************************************
 
      subroutine fgauss(size,param,x,y) 
c IN	: size,param,x
c OUT	: y
c Computes the value of the gaussian line in x, the parameters are 
c found in param
c                   x-xo
c g = A exp( -( a -------)^2 )   a = 2sqrt(log(2))
c                    w
c
      implicit none

      integer  size
      real     param(size),y,x

      real     tampon,argument
      integer  i

      y=0.0
      do i=1,size-1,3
         param(i+2) = abs(param(i+2))
         argument = 2.0 * sqrt(log(2.0)) * (x-param(i+1))/param(i+2)
         tampon   = exp(-argument**2)
         y        = y+param(i)*tampon
      enddo
      return
      end


C**********************************************************************
      subroutine florenz(size,param,x,y)
c IN	: size,param,x
c OUT	: y
c Computes the value of the lorentzian line in x, the parameters are 
c found in param
c
c            A
c f = -------------------
c      1 + (2(x-xo)/w)^2
c
c will also reset negative widthes
c
c

      implicit none

      integer  size
      real     param(size),x,y

      real     arg,denum
      integer  i

      y=0.0
      do i=1,size-1,3
         param(i+2) = abs(param(i+2))
         arg       = 2.0*(x-param(i+1))/param(i+2)
         denum     = 1.0 + (arg**2)
         y         = y + param(i)/denum
      enddo
      return
      end


C**********************************************************************
      subroutine dfgauss(size,param,x,dyda) 
c IN	: size,param,x
c OUT	: dyda
c Computes the derivative of the gaussian line in x, the parameters are 
c found in param
c
c

      implicit none

      integer  size
      real     param(size),dyda(size),x

      real     a,b,c
      integer  i

      do i=1,size-1,3
         a        = 2.0 * sqrt(log(2.0)) * (x-param(i+1))/param(i+2)
         b        = exp(-a**2)
         c        = param(i) * b       ! gaussien function
         c        = c * 2.0 * a / param(i+2)
         dyda(i)  = b
         dyda(i+1)  = 2.0 * sqrt(log(2.0)) * c
         dyda(i+2)  = a * c
      enddo
      return
      end


C**********************************************************************
      subroutine dflorenz(size,param,x,dyda)
c IN	: size,param,x
c OUT	: dyda
c computes the derivative of the lorentzian line in x, the parameters are 
c found in param
      implicit none
      integer  size
      real     param(size),dyda(size),x
      real     a,b,c
      integer  i
      do i=1,size-1,3
         a         = 2.0*(x-param(i+1))/param(i+2)
         b         = 1.0 / (1.0 + (a**2))
         dyda(i)   = b
         c         = b * param(i)         ! lorentz function
         c         = c * b * a * 2.0 / param(i+2)
         dyda(i+1) = 2.0 * c
         dyda(i+2) = a * c
      enddo
      return
      end


C**********************************************************************
      subroutine flogau(size,param,x,y)
c IN    : size,x
c OUT   : y
c INOUT : param (changed if error)
c Computes the value of the lorentzian or gaussian line in x, the 
c parameters are found in param
c
c
      implicit none

      integer size
      real param(size),x,y

      integer i
      real parambuff(3),ybuff 

      y = 0.0
c Suivant le choix de l'utilisateur FGAUSS ou FLORENZ est appelee
      do i=1,size-1,4
         param(i+2) = abs(param(i+2))
         parambuff(1) = param(i) 
         parambuff(2) = param(i+1) 
         parambuff(3) = param(i+2)
         if (param(i+3).eq.2.0) then
            call fgauss(3,parambuff,x,ybuff) 
         elseif (param(i+3).eq.1.0) then              
            call florenz(3,parambuff,x,ybuff)
         else
            call gifaout('ERROR in sub. FLOGAU')
            param(i+3) = 1.0
            return
         endif
         y = y + ybuff
      enddo
      return
      end


C**********************************************************************
      subroutine dflogau(size,param,x,dyda)
c IN    : size,param,x
c OUT   : dyda
c Computes the derivative of the lorentzian or gaussiean line in x, the 
c parameters are found in param
c
c 

      implicit none

      integer  size
      real     param(size),dyda(size),x

      integer i
      real parambuff(3),dydabuff(3) 

c Suivant le choix de l'utilisateur DFGAUSS ou DFLORENZ est appelee
      do i=1,size-1,4
         parambuff(1) = param(i)
         parambuff(2) = param(i+1)
         parambuff(3) = param(i+2)
         if (param(i+3).eq.2.0) then
            call dfgauss(3,parambuff,x,dydabuff)
         elseif (param(i+3).eq.1.0) then
            call dflorenz(3,parambuff,x,dydabuff)
         else
            call gifaout('ERROR in sub. DFLOGAU')
            param(i+3) = 1.0
            return
         endif
         dyda(i)   = dydabuff(1)
         dyda(i+1) = dydabuff(2)
         dyda(i+2) = dydabuff(3)
         dyda(i+3) = 0.0              ! should be useless
      enddo
             
      return
      end

