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
      subroutine extr3d(smx,si1,si2,si3,pil1,pil2,
     *                              col1,col2,lig1,lig2)
c IN	: pil1,pil2,col1,col2,lig1,lig2
c INOUT	: smx,si1,si2,si3
c
c extract a subcube from the smx cube
c 3 is for 1st index, 2 for second, 1 for third

      implicit none                         
               
      integer pil1,pil2,lig1,lig2,col1,col2,si1,si2,si3
      real    smx(si1*si2*si3)
      integer nbcol,nblig,nbpil,i,j

      nbcol=col2-col1+1
      nblig=lig2-lig1+1
      nbpil=pil2-pil1+1
      do i=pil1,pil2 
         do j=col1,col2 
            call copvect(smx(1 + nblig*(j-col1) +
     *     (i-pil1)*nbcol*nblig),
     *                     smx(lig1 + si3*(j-1) + 
     *     si2*si3*(i-1)),nblig)
         enddo
      enddo

      si1=nbpil
      si2=nbcol
      si3=nblig
      
      return
      end
