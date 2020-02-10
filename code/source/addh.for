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
      subroutine addh1d(work,image,size,name,error)
c IN	: size,name
c OUT	: error
c INOUT	: image
c WORK	: work
C adds the contents of image(size) with the 1D file called 'name'
c
C work is a work area given by the caller. Should be at least size long

      implicit none

      integer size,error
      real work(*),image(size)
      character*(*) name

      real z
      integer zz

      call lhare(work,zz,name,z,error)
      if (error.ne.0) return
      if (zz.ne.size) then
          write(*,*) 'Unequal sizes'
          error=1
          return
      endif
      call addvect(image,image,work,1.0,size)
      error=0
      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine addh2d(work,image,si1,si2,name,error)
c IN	: si1,si2,name
c OUT	: work, error
c INOUT	: image
C adds the contents of image(si1,si2) with the 2D file called 'name'
C work is a work area given by the caller.

      implicit none

      integer si1,si2,error
      real work(*),image(si2,si1)
      character*(*) name

      integer zz2,i,j

       open(unit=10,file=name,iostat=error,
#vms     *     readonly,
     *     status='old',form='unformatted')
       if (error.ne.0) return

       j=1
30     read(10,iostat=error,end=40,err=50)
     *    zz2,(work(i),i=1,zz2*2)
          if (2*zz2.ne.si2) then
             write(*,*) 'Unequal sizes'
             error=1
             return
          else
            call addvect(image(1,j),image(1,j),work,1.0,si2)
          endif
          if (mod(j,16).eq.0)write(*,100) J,si2
100       format ('+',2i6)
          if (j.ge.si1)  goto 40
          j=j+1
       go to 30

40     continue
       close(10)
       error=0
       return

50     continue
       close(10)
       error=1
       return
       end
