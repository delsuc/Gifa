C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in GIF/Yvette.
C
C     Author :       A.Rouh
C                    NMR Laboratory,
C                    ICSN - CNRS
C                    91190 GIF/YVETTE   FRANCE
C
C modified by M.A.Delsuc 12 may `93
C
C     This software cannot be used unless the user have been fully 
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.
C

      subroutine linecor2(debug,verbose,yy)
c IN	: debug,verbose,yy
c SIDE	: depends on DIM
c
c performs polynomial baseline correction
c yy is axis to correct
c yy : 1 or 2 in 2D   / 1, 2, or 3 in 3D
      implicit none

      integer debug,verbose,yy

#include "sizeparam.inc"
#include "linecor2v.inc"         
#include "paramfilev.inc"
#include "constant.inc"
#include "transform.inc"
#include "controlc.inc"

      integer controle,indice(bcmax),k,i,j,
     *        portion(2*maxportion),nbpt(3),nbrep,err
      real y00,y01,y02,y10,y11,y12,y20,y21,y22,
     *     rsize,min,max,rep(2,3)

      rsize = sizeimage1D

c     First debug mode to test modules

      if (debug.eq.1) then
         if (dim.ne.1) then
            call gifaout('DEBUG mode only in 1D')
            return
         endif
         call message('   0: Smooth')
         call message('   1: Derivative')
         call message('   2: Segmentation')
         call message('   3: Baseline estimation')
         call message('   4: Correction')
         call message('   6: image+data->image')
         controle = 1
         call getint2(controle,err)
         if (controle.eq.0) then 
            call copvect(source0,column,sizeimage1d)
            call centre(source0,sizeimage1d)
            if (lisse.eq.0) then
               call copvect(source1,source0,sizeimage1d)
            else 
               if (lisse/10 .eq. 1) then
                  call hyste(source1,source0,sizeimage1d,seuilhyste)
               else
                  call copvect(source1,source0,sizeimage1d)
               endif
               if (lisse - (lisse/10)*10 .eq. 1) then
                  call copvect(corr,source1,sizeimage1d)
                  call movave(source1,corr,sizeimage1d,fenma,blocbase)
               endif
            endif
            call copvect(column,source1,sizeimage1d)
         else if (controle.eq.1) then 
            call copvect(source,column,sizeimage1d)
            call copvect(source0,column,sizeimage1d)
            call centre(source0,sizeimage1d)
            if (lisse.eq.0) then
               call copvect(source1,source0,sizeimage1d)
            else 
               if (lisse/10 .eq. 1) then
                  call hyste(source1,source0,sizeimage1d,seuilhyste)
               else
                  call copvect(source1,source0,sizeimage1d)
               endif
               if (lisse - (lisse/10)*10 .eq. 1) then
                  call copvect(corr,source1,sizeimage1d)
                  call movave(source1,corr,sizeimage1d,fenma,blocbase)
               endif
            endif
            if (dim.eq.1) then
                  call deriv(debug,3,sizeimage1d)
            else
                  call deriv(debug,1,si2im)
            endif
         else if (controle.eq.2) then
            call copvect(source,column,sizeimage1d)
            call copvect(source0,column,sizeimage1d)
            call centre(source0,sizeimage1d)
            if (lisse.eq.0) then
               call copvect(source1,source0,sizeimage1d)
            else 
               if (lisse/10 .eq. 1) then
                  call hyste(source1,source0,sizeimage1d,seuilhyste)
               else
                  call copvect(source1,source0,sizeimage1d)
               endif
               if (lisse - (lisse/10)*10 .eq. 1) then
                  call copvect(corr,source1,sizeimage1d)
                  call movave(source1,corr,sizeimage1d,fenma,blocbase)
               endif
            endif
            if (segm1.eq.0) then
               call prpvect(segm,1.0,sizeimage1d)
            else if (segm1.eq.1 .or. segm1.eq.11) then
               call stddev(debug,sizeimage1d)
            else if (segm1.eq.2 .or. segm1.eq.12) then
               call deriv(0,1,sizeimage1d)
               call stddev(debug,sizeimage1d)
            else if (segm1.eq.3 .or. segm1.eq.13) then
               call deriv(0,3,sizeimage1d)
               call seuil(debug,sizeimage1d)
            else if (segm1.eq.4 .or. segm1.eq.14) then
            k = nueealgo / 100
            i = (nueealgo - k*100) / 10
            j = nueealgo - i*10 - k*100
            nbrep = 1
            if (j.eq.0) then
               if (i.eq.0) then
                  call deriv(0,2,sizeimage1d)
                  call initclassif(verbose,source2,stockage,
     *sizeimage1d,segm,2,rep,nbrep,nbpt) 
               else
                  if (k.ne.0) then
                     call deriv(0,3,sizeimage1d)
                     nbrep = nbrep + 1
                     call initclassif(verbose,source1,stockage,
     *sizeimage1d,segm,2,rep,nbrep,nbpt)
                  else
                     call deriv(0,1,sizeimage1d)
                     call initclassif(verbose,source1,stockage,
     *sizeimage1d,segm,2,rep,nbrep,nbpt)
                  endif
               endif
            else
               if (i.eq.0) then
                  if (k.ne.0) then
                     call deriv(0,2,sizeimage1d)
                     call copvect(source1,source2,sizeimage1d)
                     nbrep = nbrep + 1
                     call initclassif(verbose,source0,stockage,
     *sizeimage1d,segm,2,rep,nbrep,nbpt)
                  else
                     call initclassif(verbose,source0,stockage,
     *sizeimage1d,segm,2,rep,nbrep,nbpt)
                  endif
               else
                  nbrep = nbrep + 1
                  if (k.ne.0) then
                  call deriv(0,3,sizeimage1d)
                  nbrep = nbrep + 1
                  call initclassif(verbose,source0,stockage,
     *sizeimage1d,segm,2,rep,nbrep,nbpt)
                  else
                  call deriv(0,1,sizeimage1d)
                  call initclassif(verbose,source0,stockage,
     *sizeimage1d,segm,2,rep,nbrep,nbpt)
                  endif
               endif
            endif
            call classif(verbose,stockage,sizeimage1d,segm,2,
     *rep,nbrep,nbpt)
            endif
            if ((segm1/10).eq.1) 
     *call morpho(sizeimage1d,morphob,morphos)
            if (segm2.eq.1) then
               call segm2_inter(sizeimage1d,portion)
            else if (segm2.eq.2) then
              call segm2_auto(portion,sizeimage1d,seuilsegm2)
            else if (segm2.eq.0) then
               nbportion = 1
               portion(1) = 1
               portion(2) = sizeimage1d
            endif
            if (nbportion.gt.0) then
               portion(1) = 1
               portion(2*nbportion) = sizeimage1d
            endif
            call apply_segm(sizeimage1d)
            call copvect(column,segm,sizeimage1d)
            call mnxvect(min,max,i,j,column,sizeimage1d)
            do 5 k=2,nbportion
               i = portion(2*(k-1))
               j = portion(2*(k-1)+1) 
               call prpvect(column(i),max,j-i+1)
5           continue
C ICI
         else if (controle.eq.3 .or. controle.eq.4) then
            call init_approx(bcmax)
            call copvect(source,column,sizeimage1d)
            call copvect(source0,column,sizeimage1d)
            call centre(source0,sizeimage1d)
            if (lisse.eq.0) then
               call copvect(source1,source0,sizeimage1d)
            else 
               if (lisse/10 .eq. 1) then 
                  call hyste(source1,source0,sizeimage1d,seuilhyste)
               else
                  call copvect(source1,source0,sizeimage1d)
               endif
               if (lisse - (lisse/10)*10 .eq. 1) then
                  call copvect(corr,source1,sizeimage1d)
                  call movave(source1,corr,sizeimage1d,fenma,blocbase)
               endif
            endif
            if (segm1.eq.0) then
               call prpvect(segm,1.0,sizeimage1d)
            else if (segm1.eq.1 .or. segm1.eq.11) then
               call stddev(debug,sizeimage1d)
            else if (segm1.eq.2 .or. segm1.eq.12) then
               call deriv(0,1,sizeimage1d)
               call stddev(debug,sizeimage1d)
            else if (segm1.eq.3 .or. segm1.eq.13) then
               call deriv(0,3,sizeimage1d)
               call seuil(debug,sizeimage1d)
            else if (segm1.eq.4 .or. segm1.eq.14) then
            k = nueealgo / 100
            i = (nueealgo - k*100) / 10
            j = nueealgo - i*10 - k*100
            nbrep = 1
            if (j.eq.0) then
               if (i.eq.0) then
                  call deriv(0,2,sizeimage1d)
                  call initclassif(verbose,source2,stockage,
     *sizeimage1d,segm,2,rep,nbrep,nbpt) 
               else
                  if (k.ne.0) then
                     call deriv(0,3,sizeimage1d)
                     nbrep = nbrep + 1
                     call initclassif(verbose,source1,stockage,
     *sizeimage1d,segm,2,rep,nbrep,nbpt)
                  else
                     call deriv(0,1,sizeimage1d)
                     call initclassif(verbose,source1,stockage,
     *sizeimage1d,segm,2,rep,nbrep,nbpt)
                  endif
               endif
            else
               if (i.eq.0) then
                  if (k.ne.0) then
                     call deriv(0,2,sizeimage1d)
                     call copvect(source1,source2,sizeimage1d)
                     nbrep = nbrep + 1
                     call initclassif(verbose,source0,stockage,
     *sizeimage1d,segm,2,rep,nbrep,nbpt)
                  else
                     call initclassif(verbose,source0,stockage,
     *sizeimage1d,segm,2,rep,nbrep,nbpt)
                  endif
               else
                  nbrep = nbrep + 1
                  if (k.ne.0) then
                  call deriv(0,3,sizeimage1d)
                  nbrep = nbrep + 1
                  call initclassif(verbose,source0,stockage,
     *sizeimage1d,segm,2,rep,nbrep,nbpt)
                  else
                  call deriv(0,1,sizeimage1d)
                  call initclassif(verbose,source0,stockage,
     *sizeimage1d,segm,2,rep,nbrep,nbpt)
                  endif
               endif
            endif
            call classif(verbose,stockage,sizeimage1d,segm,2,
     *rep,nbrep,nbpt)
            endif
            if ((segm1/10).eq.1) 
     *call morpho(sizeimage1d,morphob,morphos)
            if (segm2.eq.1) then
               call segm2_inter(sizeimage1d,portion)
            else if (segm2.eq.2) then
               call segm2_auto(portion,sizeimage1d,seuilsegm2)
            else if (segm2.eq.0) then
               nbportion = 1
               portion(1) = 1
               portion(2) = sizeimage1d
            endif
            if (nbportion.gt.0) then
               portion(1) = 1
               portion(2*nbportion) = sizeimage1d
            endif
            call apply_segm(sizeimage1d)
            i = approx / 10
            k = approx - 10 * i
            if (k.eq.0) then
               if (approx/100 .eq. 0) then
                  call movave2(corr,segm,sizeimage1d,
     *fenma2,blocbase,iterma2)
               else
                  call movave3(corr,segm,sizeimage1d,
     *fenma2,blocbase,iterma2)
               endif
            else
               if (nbportion.gt.0) then
                  call approxim(indice,portion(1),bcmax,
     *portion(2)-portion(1)+1,y10,y11,y12,y20,y21,y22)

c     Si la premiere portion ne commence pas a gauche, prolongation de 
c la correction par continuite c2 au premier point de ligne de base.

                  if (portion(1).ne.1) 
     *call prolonge(verbose,sizeimage1d,
     *0,0.0,0.0,portion(1),y10,y11,y12)


c     Memorisation des caracteristiques du dernier point de l approximation
c precedente.

                  y00 = y20
                  y01 = y21
                  y02 = y22

c     Si autre portion de ligne de base a corriger :

                  if (nbportion.gt.1) then
C     Approximation et prolongation a gauche sur les autres portions de
C ligne de base
                     do 6 k=2,nbportion
                        call approxim(indice,portion((k-1)*2+1),
     *bcmax,
     *portion(k*2)-portion((k-1)*2+1)+1,y10,y11,y12,y20,y21,y22)
                        call prolonge(verbose,sizeimage1d,
     *portion((k-1)*2),y00,y01,portion((k-1)*2+1),y10,y11,0.0)
                        y00 = y20
                        y01 = y21
                        y02 = y22
6                   continue 
                  endif

C     Prolongation a droite si la derniere portion de ligne de base 
C n arrive pas au bout

                  if (portion(2*nbportion).lt.sizeimage1d) 
     *call prolonge(verbose,sizeimage1d,
     *portion(nbportion*2),y00,y01,0,0.0,0.0,y02)
               endif
            endif
            if (controle.eq.3) then
               call copvect(column,corr,sizeimage1d)
            else
               call addvect(column,source,corr,-1.0,sizeimage1d)
            endif
         elseif (controle.eq.6) then
            call addvect(column,column,datab,1.0,sizeimage1d)
         endif   
      
    
c   If debug .....

      else 
         if (approx-10*(approx/10).eq.1) call init_approx(bcmax)

      if (dim.eq.1) then
         call copvect(source,column,sizeimage1d)
         call linecor21d(verbose,sizeimage1d,indice,portion)
         call copvect(column,source,sizeimage1d)
      elseif (dim.eq.2) then
        if (yy.eq.1) then                  ! in F1
          call initinprog(si2im)
          do i=1,si2im
            call inprog(i)
            if (control.eq.1) goto 100
            if (segm2.ne.1 .or. window(i).ne.0.0) then
               call getcol(source,plane2d,i,si1im,si2im)
               call linecor21d(verbose,si1im,indice,portion)
               call putcol(source,plane2d,i,si1im,si2im)
            endif
          enddo
        else
          call initinprog(si1im)
          do i=1,si1im                    ! in F2
            call inprog(i)
            if (control.eq.1) goto 100
            if (segm2.ne.1 .or. window(si2im+i).ne.0.0) then
               call copvect(source,plane2d(si2im*(i-1)+1),si2im)
               call linecor21d(verbose,si2im,indice,portion)
               call copvect(plane2d(si2im*(i-1)+1),source,si2im)
            endif
          enddo
        endif
      else                  ! dim == 3
        if (yy.eq.1) then                  ! in F1
          call initinprog(si3d2*si3d3)
          do i=1,si3d2
           do j=1,si3d3
            k = j+(i-1)*si3d2
            if (segm2.ne.1 .or. (window(i+si3d3)*window(j)).ne.0.0) then
               call getcol(source,image,k,si3d1,si3d3*si3d2)
               call linecor21d(verbose,si3d1,indice,portion)
               call putcol(source,image,k,si3d1,si3d3*si3d2)
            endif
           enddo
           if (control.eq.1) goto 100
           call inprog(k)
          enddo
        elseif (yy.eq.2) then                  ! in F2
          call initinprog(si3d1*si3d3)
          do i=1,si3d1
           do j=1,si3d3
            k = j+(i-1)*si3d1
            if (segm2.ne.1 .or.
     *          (window(i+si3d3+si3d2)*window(j)).ne.0.0) then
               call getcol(source,image((i-1)*si3d3*si3d2+1),
     *                     j,si3d2,si3d3)
               call linecor21d(verbose,si3d2,indice,portion)
               call putcol(source,image((i-1)*si3d3*si3d2+1),
     *                     j,si3d2,si3d3)
            endif
           enddo
           if (control.eq.1) goto 100
           call inprog(k)
          enddo
        else                              ! in F3
          call initinprog(si3d2*si3d1)
          do i=1,si3d1
           do j=1,si3d2
            k = j+(i-1)*si3d1
            if (segm2.ne.1 .or. 
     *          (window(i+si3d3+si3d2)*window(j+si3d3)).ne.0.0) then
               call copvect(source,image(si3d3*(k-1)+1),si3d3)
               call linecor21d(verbose,si3d3,indice,portion)
               call copvect(image(si3d3*(k-1)+1),source,si3d3)
            endif
           enddo
           if (control.eq.1) goto 100
           call inprog(k)
          enddo
        endif   ! yy
      endif     ! dim
      endif     ! debug
      return

C This is used by control_c
100   continue

      return
      end




      subroutine linecor21d(verbose,size,indice,portion)
c IN	: verbose,size,indice,portion
c realizes the 1D polynomial baseline correction
c
c called by linecor2
      implicit none

#include "sizeparam.inc"
#include "linecor2v.inc"         
c#include "paramfilev.inc"
#include "constant.inc"

      integer indice(bcmax),size,
     *         portion(2*maxportion),verbose
      integer iter,k,i,j,nbpt(3),controle
      real y00,y01,y02,y10,y11,y12,y20,y21,y22,devcor,
     *     rsize,devspectre,rep(2,3)

      rsize = size

      iter = 0
10    iter = iter +1

c     lissage eventuel des donnees et transfert de origine vers source

c     calcul de deviation standard de la partie de spectre a corriger

      call dotvect(devspectre,source,source,size)
      devspectre = sqrt(devspectre/rsize)

      call copvect(source0,source,size)
      call centre(source0,size)


      if (verbose.eq.1) call gifaout('Smooth data')
      if (lisse.eq.0) then
         call copvect(source1,source0,size)
      else 
        if (lisse/10 .eq. 1) then
           call hyste(source1,source0,size,seuilhyste)
        else
           call copvect(source1,source0,size)
        endif
        if (lisse - (lisse/10)*10 .eq. 1) then
           call copvect(corr,source1,size)
           call movave(source1,corr,size,fenma,blocbase)
        endif
      endif

c     segmentation primaire

      if (verbose.eq.1) call gifaout('First segmentation')
      if (segm1.eq.0) then
         call prpvect(segm,1.0,size)
      else if (segm1.eq.1 .or. segm1.eq.11) then
         call stddev(0,size)
      else if (segm1.eq.2 .or. segm1.eq.12) then
         call deriv(0,1,size)
         call stddev(0,size)
      else if (segm1.eq.3 .or. segm1.eq.13) then
         call deriv(0,3,size)
         call seuil(0,size)
      else if (segm1.eq.4 .or. segm1.eq.14) then
         k = nueealgo / 100
         i = (nueealgo - k*100) / 10
         j = nueealgo - i*10 - k*100
         controle = 1
         if (j.eq.0) then
            if (i.eq.0) then
               call deriv(0,2,size)
               call initclassif(verbose,source2,stockage,
     *size,segm,2,rep,controle,nbpt) 
            else
               if (k.ne.0) then
                  call deriv(0,3,size)
                  controle = controle + 1
                  call initclassif(verbose,source1,stockage,
     *size,segm,2,rep,controle,nbpt)
               else
                  call deriv(0,1,size)
                  call initclassif(verbose,source1,stockage,
     *size,segm,2,rep,controle,nbpt)
               endif
            endif
         else
            if (i.eq.0) then
               if (k.ne.0) then
                  call deriv(0,2,size)
                  call copvect(source1,source2,size)
                  controle = controle + 1
                  call initclassif(verbose,source0,stockage,
     *size,segm,2,rep,controle,nbpt)
               else
                  call initclassif(verbose,source0,stockage,
     *size,segm,2,rep,controle,nbpt)
               endif
            else
               controle = controle + 1
               if (k.ne.0) then
               call deriv(0,3,size)
               controle = controle + 1
               call initclassif(verbose,source0,stockage,
     *size,segm,2,rep,controle,nbpt)
               else
               call deriv(0,1,size)
               call initclassif(verbose,source0,stockage,
     *size,segm,2,rep,controle,nbpt)
               endif
            endif
         endif
         call classif(verbose,stockage,size,segm,2,
     *rep,controle,nbpt)
      endif
      if ((segm1/10).eq.1) call morpho(size,morphob,morphos)
 
c     segmentation secondaire

      if (verbose.eq.1) call gifaout('Second segmentation')
      if (segm2.eq.1) then
         call segm2_inter(size,portion)
      else if (segm2.eq.2) then
         call segm2_auto(portion,size,seuilsegm2)
      else if (segm2.eq.0) then
          nbportion = 1
          portion(1) = 1
          portion(2) = size
      endif
      if (nbportion.gt.0) then
          portion(1) = 1
          portion(2*nbportion) = size
      endif


c     estimation de ligne de base
      if (verbose.eq.1) call gifaout('Base line estimation')


      call apply_segm(size)
      if (approx.eq.0 .or. approx.eq.10) then
         call movave2(corr,segm,size,fenma2,blocbase,iterma2)      
      else if (approx.eq.100 .or. approx.eq.110) then
         call movave3(corr,segm,size,fenma2,blocbase,iterma2)
      else
         if (nbportion.gt.0) then
            call approxim(indice,portion(1),bcmax,
     *portion(2)-portion(1)+1,y10,y11,y12,y20,y21,y22)

c     Si la premiere portion ne commence pas a gauche, prolongation de 
c la correction par continuite c2 au premier point de ligne de base.

         if (portion(1).ne.1) call prolonge(verbose,size,
     *0,0.0,0.0,portion(1),y10,y11,y12)


c     Memorisation des caracteristiques du dernier point de l approximation
c precedente.

         y00 = y20
         y01 = y21
         y02 = y22

c     Si autre portion de ligne de base a corriger :

         if (nbportion.gt.1) then
C     Approximation et prolongation a gauche sur les autres portions de
C ligne de base
            do 30 k=2,nbportion
               call approxim(indice,portion((k-1)*2+1),bcmax,
     *portion(k*2)-portion((k-1)*2+1)+1,y10,y11,y12,y20,y21,y22)
               call prolonge(verbose,size,
     *portion((k-1)*2),y00,y01,portion((k-1)*2+1),y10,y11,0.0)
               y00 = y20
               y01 = y21
               y02 = y22
30          continue 
         endif

C     Prolongation a droite si la derniere portion de ligne de base 
C n arrive pas au bout

         if (portion(2*nbportion).lt.size) 
     *call prolonge(verbose,size,
     *portion(nbportion*2),y00,y01,0,0.0,0.0,y02)

         else
            call prpvect(corr,0.0,size)
         endif
      endif


c     correction de ligne de base


      call addvect(source,source,corr,-1.0,size)
c     calcul du critere d iteration

      call dotvect(devcor,corr,corr,size)
      devcor = sqrt(devcor/rsize)
      if (verbose.eq.1) then
         call gifaoutr('dev. spectre = ' , devspectre)
         call gifaoutr('dev. corr = ' , devcor)
         call gifaoutr('ldbw = ' , ldbw)
         call gifaoutr('ldbw*devspectre = ' , ldbw*devspectre)
         call gifaouti('blciter = ' , blciter)
         call gifaouti('iter = ' , iter)
      endif
      if (devcor .gt. ldbw*devspectre .and.
     *    iter.lt.blciter) then
            goto 10
      endif

      return
      end
			



