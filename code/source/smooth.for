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
      subroutine smooth1d(fdata,size,nsm)
c IN	: size,nsm
c INOUT	: fdata
c
C    This subr. performs a Moving Average smoothing of the data, fdata(size)
C length of M.A. is nsm,

      Implicit None
      integer size,nsm
      real fdata(size)

      integer i,j
      real sum

      If (nsm.lt.2 .or. nsm.gt.size-2) then
         write(*,*) 'Error in parameters'
         return
      endif

      do i=1,size-nsm
        sum = 0.0
        do j=0,nsm-1
           sum = sum + fdata(i+j)
        enddo
        fdata(i) = sum/nsm
      enddo

      j = nsm/2
      do i=size-nsm,1,-1
        fdata(i+j) = fdata(i)
      enddo
      do i=1,j
        fdata(i) = 0.0
        fdata(size+1-i) = 0.0
      enddo

      return
      end

      subroutine smooth2d(fdata,size1,size2,nsm1,nsm2)
c IN	: size1,size2,nsm1,nsm2
c INOUT	: fdata
c
C    This subr. performs a Moving Average smoothing of the data, 
C fdata(size2,size1) is real
C length of M.A. is nsm1 in dim 1 (vertical) and nsm2 in dim 2
C pad with zero lost points.
      Implicit None
      integer size1,nsm1,size2,nsm2
      real fdata(size2,size1)

      integer i1,j1,i2,j2
      real sum,dd

      If (nsm1.lt.2 .or. nsm1.gt.size1-2 .or.
     *    nsm2.lt.2 .or. nsm2.gt.size2-2) then
         write(*,*) 'Error in parameters'
         return
      endif

C first compute M.A.
      dd = 1.0/(nsm1*nsm2)
      do i1=1,size1-nsm1
        do i2=1,size2-nsm2
          sum = 0.0
          do j1=0,nsm1-1
            do j2=0,nsm2-1
              sum = sum + fdata(i2+j2,i1+j1)
            enddo
          enddo
          fdata(i2,i1) = sum*dd
        enddo
      enddo
C then shift
      j1 = nsm1/2
      j2 = nsm2/2
      do i1=size1-nsm1,1,-1
        do i2=size2-nsm2,1,-1
          fdata(i2+j2,i1+j1) = fdata(i2,i1)
        enddo
      enddo
C then pad
      do i1=1,j1
        do i2=1,j2
          fdata(i1,i2) = 0.0
          fdata(size2+1-i2,size1+1-i1) = 0.0
        enddo
      enddo
      return
      end


      subroutine smooth3d(fdata,size1,size2,size3,nsm1,nsm2,nsm3)
c IN	: size1,size2,size3,nsm1,nsm2,nsm3
c INOUT	: fdata
c
C    This subr. performs a Moving Average smoothing of the data, 
C fdata(size3,size2,size1) is real
C length of M.A. is nsm1 in dim 1 (vertical), nsm2 in dim 2 and
C nsm3 in dim 3
C pad with zero lost points.
      Implicit None
      integer size1,nsm1,size2,nsm2,size3,nsm3
      real fdata(size3,size2,size1)

      integer i1,j1,i2,j2,i3,j3
      real sum,dd

      If (nsm1.lt.1 .or. nsm1.gt.size1-2 .or.
     *    nsm2.lt.1 .or. nsm2.gt.size2-2 .or.
     *    nsm3.lt.1 .or. nsm3.gt.size3-2 .or.
     *    nsm1*nsm2*nsm3.le.1 ) then
         write(*,*) 'Error in parameters'
         return
      endif

C first compute M.A.
      dd = 1.0/(nsm1*nsm2*nsm3)
      do i1=1,size1-nsm1
        do i2=1,size2-nsm2
          do i3=1,size3-nsm3
            sum = 0.0
            do j1=0,nsm1-1
              do j2=0,nsm2-1
                do j3=0,nsm3-1
                  sum = sum + fdata(i3+j3,i2+j2,i1+j1)
                enddo
              enddo
            enddo
            fdata(i3,i2,i1) = sum*dd
          enddo
        enddo
      enddo
C then shift
      j1 = nsm1/2
      j2 = nsm2/2
      j3 = nsm3/2
      if ((j1+j2+j3).gt.0) then
        do i1=size1-nsm1,1,-1
          do i2=size2-nsm2,1,-1
            do i3=size3-nsm3,1,-1 
             fdata(i3+j3,i2+j2,i1+j1) = fdata(i3,i2,i1)
            enddo
          enddo
        enddo
C then pad
        do i1=1,max(1,j1)
          do i2=1,max(1,j2)
            do i3=1,max(1,j3) 
              fdata(i3,i2,i1) = 0.0
              fdata(size3+1-i3,size2+1-i2,size1+1-i1) = 0.0
            enddo
          enddo
        enddo
      endif
      return
      end
