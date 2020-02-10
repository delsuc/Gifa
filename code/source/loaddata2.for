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
      subroutine loaddata(tdata,size,noise)
c OUT	: tdata,size,noise
c SIDE	: spectral parameters
c
c simulates a 1D fid form values entered by the user
c
      implicit none
#include "sizeparam.inc"
#include "constant.inc"
#include "paramfilev.inc"

      integer i,seed,j,nline,size,err
      complex tdata(*)
      character*256 st
      real  noise,zz,gasdev,z,w,
     *      phase(20),freq(20),t2(20),amp(20),t,f,p
      save  zz,nline,phase,freq,t2,amp,seed

C init tdata                    

#vms      Call Lib$flt_under(0)
10     call message ('Spectral width :')
      call getreal2(sp1d,err)
      if (sp1d.le.0.0) goto 10
      of1d = 0.0
1     call message( 'data size')
      call getint2( size,err)
      if (2*size.gt.smxmax.or.size.lt.1) goto 1
      call message( 'number of line')
2     call getint2( nline,err)
      if (nline.gt.20 .or. nline.lt.0) goto 2
      do j=1,nline
        write(st,*)  j,'th line'
        call message(st)
        call message( '   amplitude')
        call getreal2( amp(j),err)

        call message( '   frequence')
        call getreal2( freq(j),err)

61      call message( '   t2 (in sec)')
        if (t2(j).eq.0.0) t2(j)=1.0
        call getreal2( t2(j)  ,err)
        if (t2(j).le.0.0) then
           write(*,*) 't2 Should not be 0'
           goto 61
        endif

        call message( '   phase')
        call getreal2( phase(j),err)
      enddo
      call message( 'amount of noise')
      call getreal2( zz,err)
      noise=zz
      if (noise.ne.0.0) then
         call message( 'enter seed')
         call getint2( seed,err)
      endif

      call prpvect(tdata,0.0,size)
      do j=1,nline
      f = (freq(j)*(2*pi)/sp1d) -pi
      p = phase(j)*pi/180.0
      t = t2(j)*sp1d 
      do i=1,size/2
          z = amp(j)*exp(-float(i-1)/t)
          w = f*float(i-1)+p
          tdata(i) = tdata(i) + z*cmplx(cos(w),sin(w))
      enddo
      enddo

      if (noise.ne.0.0) then
         do i=1,size/2
            tdata(i) = tdata(i) + 
     *            cmplx(noise*gasdev(seed), noise*gasdev(seed))
         enddo
      endif
      call bruvect(tdata,tdata,size)

      return
      end
