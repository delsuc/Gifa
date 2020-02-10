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
      subroutine extr2d(smx,si1,si2,col1,col2,lig1,lig2)
c IN	: col1,col2,lig1,lig2
c INOUT	: smx,si1,si2
c
c extract a submatrix from the matrix smx
c 1 is for second index, 2 is for first index

      implicit none                         
                            
      integer lig1,lig2,col1,col2,si1,si2
      real    smx(si1*si2)
      integer nbcol,nblig,i
                                               
      nbcol=col2-col1+1
      nblig=lig2-lig1+1
      do i=lig1,lig2 
        call copvect(smx((i-lig1)*nbcol+1),smx(col1 + si2*(i-1)),nbcol)
      enddo

      si1=nblig
      si2=nbcol
      
      return
      end
