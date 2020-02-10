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
       subroutine intpic1d(tab,num,vol,poinpic,rap,mon,stop,
     *                   taille,colg,cold,signpic,zq)
c IN	: tab,num,poinpic,rap,mon,stop,taille,signpic,zq
c OUT	: vol,colg,cold
c realizes the intregration of peak "num" as found in peak1d table
c and returns the volume "vol", the extention

       implicit none


       integer num,poinpic,colg,cold, 
     *         taille,xx,signpic,zq
       real    vol,rap,mon,stop,val,tab(taille)
                                                         

       vol=0.0
       poinpic=0                                
       call borne1d(tab,taille,num,rap,mon,stop,
     *               colg,cold,signpic,zq)

         do xx=colg,cold
              val=tab(xx)
              poinpic=poinpic+1
              vol=vol+val
         enddo

       return
       end
