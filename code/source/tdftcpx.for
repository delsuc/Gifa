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
      subroutine tdftcpx(matrix,si1sp,si2sp,sign,mode)
c IN	: si1sp,si2sp,sign,mode
c INOUT	: matrix
c
c      calcule la tf complexe d'un vecteur.selon la valeur de mode,
c      on fait xf1,xf2,xfb ou rien.le chargement d'une ligne est
c      immediat,et dans le cas d'une colonne on va chercher et 
c      redeposer les points un a un.      
   
       implicit none

#include "sizeparam.inc"
#include "controlc.inc"

       integer  mode
       integer si1sp,si2sp,i,sign
       real   matrix(si2sp,si1sp)

      
      if (mod(mode,2).eq.1) then  !calculates xf2 if mode=1 or 3 
         do 2 i=1,si1sp
             call fftcpx(matrix(1,i),si2sp/2,sign)
             if (control.eq.1) return
2        continue 
       endif

       if (mode/2.eq.1) then   !calcule xf1 si mode=2 ou 3
           do 4 i=1,si2sp
             call getcol(work1d,matrix,i,si1sp,si2sp)
             call fftcpx(work1d,si1sp/2,sign)
             call putcol(work1d,matrix,i,si1sp,si2sp)
             if (control.eq.1) return
4          continue            
       endif
       return
       end
              
