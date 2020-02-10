C This file is a part of the GIFA program
C     This software has been developped by the NMR Group in GIF/Yvette.
C
C     Authors :       M.A.Delsuc, M.Robin
C                     NMR Laboratory,
C                     ICSN - CNRS
C                     91190 GIF/YVETTE   FRANCE
C
C     This software cannot be used unless the user have been fully
C licensed to do so form the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.
C
C*************************************************************
      subroutine transpose(array,size1,size2,size3,zz)
c IN	: size1,size2,size3,zz
c INOUT	: array
c
C TRANSPOSE(A,L,M,N,I)
c    Transposes the 3D matrix A of size LxMxN. 
C                      If I = 23 transposition of the second
C                      and third dimensions,
C                      If I = 13 transposition of the first
C                      and third dimensions,
C                      If I = 12 transposition of the first
C                      and second dimensions.
C   Can also be used to transpose a 2d matrix
C                      A of size MxN by calling
C                      transpose(A,1,M,N,23)

      implicit none

      integer zz,off1,off2,sz,sz1,sz2,offm,jm,
     *        size1,size2,size3,n,m,n1,n2,i,j
      real array(size1*size2*size3)

C First check for the transposition which has to be computed
C LxMxN == size1,size2,size3
C jm : size of the chunk
C offm :
C sz1 x sz2 : matrix to be transposed
c
      if (zz.eq.12) then    !A is a LxM matrix of vectors of length N
         sz1 = size1
         sz2 = size2
         jm = size3
         offm = size3
      elseif (zz.eq.13) then   !A is a Lx(MxN) matrix
         sz1 = size1
         sz2 = size3
         jm = size2
         offm = 1
      elseif (zz.eq.23) then   !A is made of L MxN matrices
         sz1 = size2
         sz2 = size3
         jm = 1
         offm = 1
      else
         return
      endif

      if ((sz1.lt.2).or.(sz2.lt.2)) return

      n1 = 1
      sz = sz1
1       sz = sz/2
        n1 = n1+1
        if (sz.gt.2) goto 1
      if (2**n1.ne.sz1) return       ! sz1 must be power of two

      n2 = 1
      sz = sz2
2       sz = sz/2
        n2 = n2+1
        if (sz.gt.2) goto 2
      if (2**n2.ne.sz2) return       ! sz2 must be power of two

      sz = size1*size2*size3

      if (n1.eq.n2) then
         off1 = sz1/2
         do n=1,n1
           do i=1,sz,2*sz1*off1*jm
             do j=off1*offm,jm*sz1*off1-1,2*off1*offm
               call excvect(array(i+j+(sz1-1)*off1),
     *                                      array(i+j),off1*offm)
             enddo
           enddo
          off1 = off1/2  
         enddo

      elseif (sz1.lt.sz2) then
         off2 = sz2
         do n=1,n2-n1
           off1 = sz1/2    
           do m=1,n1
            do i=1,sz,2*off2*off1*jm
             do j=offm*off2/2,jm*off1*off2-1,off2*offm
               call excvect(array(i+j+(2*off1-1)*off2/2),
     *                                      array(i+j),offm*off2/2)
             enddo
            enddo
            off1 = off1/2  
           enddo
           off2 = off2/2  
         enddo

         off1 = sz1/2
         do n=1,n1
           do i=1,sz,2*sz1*off1*jm
             do j=offm*off1,jm*sz1*off1-1,2*offm*off1
               call excvect(array(i+j+(sz1-1)*off1),
     *                                      array(i+j),offm*off1)
             enddo
           enddo
          off1 = off1/2  
         enddo

      else
C first, sz2 by sz2 transpostition of the sz1/sz2 submatrices
         off2 = 1
         do n=1,n2
           do i=1,sz,2*sz2*off2*jm
             do j=offm*off2,jm*sz2*off2-1,2*offm*off2
               call excvect(array(i+j+(sz2-1)*off2),
     *                                      array(i+j),offm*off2)
             enddo
           enddo
          off2 = off2*2  
         enddo
C then conversion to a sz2 by sz1 matrix
         off2 = 2*sz2
         do n=1,n1-n2
           off1 = 1    
           do m=1,n2
            do i=1,sz,2*off2*off1*jm
             do j=offm*off2/2,jm*off1*off2-1,offm*off2
               call excvect(array(i+j+(2*off1-1)*off2/2),
     *                                      array(i+j),offm*off2/2)
             enddo
            enddo
            off1 = off1*2  
           enddo
           off2 = off2*2  
         enddo

      endif

      return
      end
