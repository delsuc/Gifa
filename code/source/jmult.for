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
      subroutine jmult(array,size,j,type,itype)
c IN	: size,j,type,itype
c INOUT	: array
c
c multiply the data "array" by a sinusoid of freq j
c itype 0 or 1 : real or complex
c type A or P : sine or cosine

      implicit none
                   
      integer size,itype
      real array(size),j
      character*1   type

      integer i
      real zz,z

#vms      Call Lib$flt_under(0)

      z = j/4000.0
      if (size.gt.1) then    
        if (type.eq.'A') then
          if (itype.eq.0) then
           do i=1,size
              zz = sin(z*(i-1))
              array(i) = array(i)*zz
           enddo
          else
           do i=1,size,2
              zz = sin(z*(i-1))
              array(i) = array(i)*zz
              array(i+1) = array(i+1)*zz
           enddo
          endif
        else 
           if (type.eq.'P') then
             if (itype.eq.0) then
              do i=1,size-1
                 zz = cos(z*(i-1))
                 array(i) = array(i)*zz
              enddo
             else
              do i=1,size-1,2
                 zz = cos(z*(i-1))
                 array(i) = array(i)*zz
                 array(i+1) = array(i+1)*zz
              enddo
             endif
           endif
         endif 
      endif

      return
      end
