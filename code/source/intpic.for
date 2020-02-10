C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in GIF/Yvette.
C
C     Authors :       M.A.Delsuc, V.Stoven, M.Robin
C                     NMR Laboratory,
C                     ICSN - CNRS
C                     91190 GIF/YVETTE   FRANCE
C
C     This software cannot be used unless the user have been fully 
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.
C
       subroutine intpic(tab,masktab,num,vol,poinpic,rap,mon,stop,
     *      taille1,taille2,ligb,ligh,limg,limd,com,signpic,zq)
c IN	: tab,num,poinpic,rap,mon,stop,taille1,taille2,com,signpic,zq
c INOUT	: masktab
c OUT	: vol,ligb,ligh,limg,limd
c
c realizes the intregration of peak "num" as found in peak2d table
c and returns the volume "vol", the extention, and update the amoeba
c "masktab"
       implicit none

#include "sizebase.inc"
#include "peaksize.inc"

       integer num,poinpic,colg,cold,ligh,ligb,limg,limd, 
     *         taille1,taille2,yy,somcol,xx,i,com,col,lig,
     *         zq,signpic
       real    vol,rap,mon,stop,tab(taille2,taille1),
     *         masktab(taille2,taille1),temp

       vol=0.0
       poinpic=0 
       col=peak2d(num,1)
       lig=peak2d(num,4)
c determine extremum of the amoeba
       call bornelig(tab,taille1,taille2,num,rap,mon,stop,
     *               colg,cold,signpic,zq)
       call bornecol(tab,taille1,taille2,num,rap,mon,stop,
     *               ligb,ligh,signpic,zq)
c then determine the amoeba and sum up
       do yy=ligb,ligh
         call maxlig(tab,taille1,taille2,yy,colg,cold,somcol,signpic,zq)
         call limlig(tab,taille1,taille2,num,rap,mon,stop,
     *               yy,somcol,colg,cold,limg,limd,signpic,zq)  

         if (peak2d(num,5).gt.0.0) then
           temp=num
         else
           temp=abs(peak2d(num,5))
         endif 

         if (masktab(1,1) .ne.-1.0) then
           do i=limg,limd
             masktab(i,yy)=temp
           enddo
         endif

         do xx=limg,limd
           vol=vol+tab(xx,yy)
           poinpic=poinpic+1
          enddo

       enddo
       return
       end
