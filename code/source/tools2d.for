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
      subroutine zerofill(tablo,fill01,fill02,init1,init2)
c IN	: fill01,fill02,init1,init2
c INOUT	: tablo
c
c     permit to change the dimensions of a 2D matrix. init1 and init2
c     are numbers of real points before change, fill01 and 
c     fill02 are numbers of real points after change
c If size is increasing, zeros are added

      implicit none

      integer  i,fill01,fill02,init1,init2
      real     tablo(*)    

      if (fill02.gt.init2) then 
          do i=min(fill01,init1), 1, -1
            call cprvect(tablo((i-1)*fill02+1),
     *                   tablo((i-1)*init2+1),fill02,init2)
          enddo     
      elseif (fill02.lt.init2) then
          do i=2, min(init1,fill01)
            call copvect(tablo((i-1)*fill02+1),
     *                   tablo((i-1)*init2+1),fill02)
          enddo
      endif

      if (fill01.gt.init1) then
        call  prpvect( tablo( fill02*init1+1 ),
     *                   0.0,fill02*(fill01-init1))
      endif

      return
      end

C******************************************************************
       subroutine getcol(col,mat,number,size1,size2)
c IN	: mat,number,size1,size2
c OUT	: col
c
c      read-in a column from matrix "mat" at index "number"
c and put it in  "col"

       implicit  none
C the direct variable tries to help a Virtual Memory system using the
C Least Recently Used Algo.
       integer direct
       common/getputcol/direct

       integer  i,number,size1,size2
       real  col(size1),mat(size2,size1) 
     
       if (mod(direct,2).eq.0) then
         do i=1,size1
           col(i)=mat(number,i)
         enddo
       else
         do i=size1,1,-1
           col(i)=mat(number,i)
         enddo
       endif
       direct = direct+1
       return
       end

c***************************************************************
       subroutine putcol(col,mat,number,size1,size2)
c IN	: col,number,size1,size2
c INOUT	: mat
c
c  put back "co"l, as a column into matrix "mat" at index "number"

       implicit none

       integer direct
       common/getputcol/direct

       integer  i,number,size1,size2
       real  col(size1),mat(size2,size1)

       if (mod(direct,2).eq.0) then
         do i=1,size1
           mat(number,i)=col(i)
         enddo
       else
         do i=size1,1,-1
           mat(number,i)=col(i)
         enddo
       endif
       direct = direct+1
       return
       end

