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
      subroutine simu2d(tdata,size1,size2,noise,it)
c OUT	: tdata,size1,size2,noise,it
c
c simulates a 2D FID
c
      implicit none
#include "sizeparam.inc"
#include "constant.inc"
#include "controlc.inc"
#include "paramfilev.inc"

      integer i,k,seed,j,nline,size1,size2,it,qtype,err
      character*256 st
      real    tdata(*)
      real  noise,zz,gasdev,z,w1,w2,
     *      phase1(10),phase2(10),freqq1(10),freqq2(10),
     *      t2(10),t1(10),amp(10),cw,sw,e1,f1,f2,p1,p2,tt1,tt2
      save nline,phase1,phase2,freqq1,freqq2,
     *      t2,t1,amp,zz,qtype,seed

C init tdata                     

#vms      Call Lib$flt_under(0)
10    call message(' spectral width in F1')
      call getreal2(specw1,err)
      call message(' spectral width in F2')
      call getreal2(specw2,err)
      if (specw1.le.0.0 .or. specw2.le.0.0 ) goto 10
      offset1 = 0.0
      offset1 = 0.0
1     call message( 'data size in dim1:')
      call getint2(size1,err)                   !size1/2 complex points
      call message('data size in dim2:'  )
      call getint2( size2,err)                   !size2/2 complex points
      if (size1*size2.gt.smxbig) goto 1              
      call message( 'phase(0) or amplitude(1) modulation')
      call getint2(qtype,err)
2     call message( 'number of lines:')
      call getint2( nline,err)
      if (nline.gt.10 .or. nline.lt.0) goto 2
      do j=1,nline
        write(st,*) j,'th line'
        call message(st)
        call message( '   amplitude')
        call getreal2( amp(j),err)
             
        call message( '   frequency in dim2:')
        call getreal2( freqq2(j),err)

        call message( '   frequency in dim1:')
        call getreal2( freqq1(j),err)

61      call message( '   t2 in dim2')
        if (t2(j).eq.0.0) t2(j)=1.0
        call getreal2( t2(j)  ,err)
        if (t2(j).le.0.0) then
           write  (*,*) 't2 Should not be 0'
           goto 61
        endif

 62     call message( '   t2 in dim1')
        if (t1(j).eq.0.0) t1(j)=1.0
        call getreal2( t1(j),err)
        if (t1(j).le.0.0) then
           write(*,*) 't2 Should not be 0'
           goto 62
        endif

        call message( '   phase in dim2:')
        call getreal2( phase2(j),err)

        call message('    phase in dim1:')
        call getreal2( phase1(j),err)
      enddo
      call message( 'amount of noise')
      call getreal2( zz,err)
      noise=zz
      if (noise.ne.0.0) then
         call message( 'enter seed')
         call getint2( seed,err)
      endif

      if ( qtype .eq. 1 ) then
      it = 3
      call prpvect(tdata,0.0,size1*size2)
      do j=1,nline
      f1 = (freqq1(j)*(2*pi)/specw1) -pi
      p1 = phase1(j)*pi/180.0
      tt1 = t1(j)*specw1
      f2 = (freqq2(j)*(2*pi)/specw2) -pi
      p2 = phase2(j)*pi/180.0
      tt2 = t2(j)*specw2
      do i=1,size1,2
        w1 = f1*float((i+1)/2-1)+p1 
C il faudrait diviser i-1 par 2!
        cw = cos(w1)
        sw = sin(w1)
        e1 = exp(-float((i+1)/2-1)/tt1)
        do k=1,size2,2
          z = e1*amp(j)*exp(-float((k+1)/2-1)/tt2)
          w2 = f2*float((k+1)/2-1)+p2
          tdata(k+size2*(i-1)) = tdata(k+size2*(i-1)) +
     *               z*cos(w2)*cw
          tdata(k+1+size2*(i-1))=tdata(k+1+size2*(i-1))+
     *               z*sin(w2)*cw
          tdata(k+size2*i)=tdata(k+size2*i) +
     *               z*cos(w2)*sw 
          tdata(k+1+size2*i)=tdata(k+1+size2*i) +
     *               z*sin(w2)*sw
        enddo 
        if (control.eq.1) return
      enddo
      enddo
C  divide first complexes points by 2 to remove ridges
      call mltvect(tdata,tdata,0.5,size2)
      call mltvect(tdata(size2+1),tdata(size2+1),0.5,size2)
      do i=1,size1
        tdata(size2*(i-1)+1) = tdata(size2*(i-1)+1)*0.5
        tdata(size2*(i-1)+2) = tdata(size2*(i-1)+2)*0.5
      enddo

      elseif (qtype .eq.0 ) then
      it = 1
      call prpvect(tdata,0.0,size1*size2)
      do j=1,nline
C      f1 = (freqq1(j)*(2*pi)/specw1) -pi
      f1 = (freqq1(j)*(2*pi)/specw1) 
      p1 = phase1(j)*pi/180.0
      tt1 = t1(j)*specw1
      f2 = (freqq2(j)*(2*pi)/specw2) -pi
      p2 = phase2(j)*pi/180.0
      tt2 = t2(j)*specw2
      do i=1,size1,1
        w1 = f1*float(i-1)+p1 
        e1 = exp(-float((i+1)/2-1)/tt1)
        do k=1,size2,2
          z = e1*amp(j)*exp(-float((k+1)/2-1)/tt2)
          w2 = f2*float((k+1)/2-1)+p2
          tdata(k+size2*(i-1)) = tdata(k+size2*(i-1)) +
     *               z*cos(w2+w1)
          tdata(k+1+size2*(i-1))=tdata(k+1+size2*(i-1)) +
     *               z*sin(w2+w1)
        enddo 
        if (control.eq.1) return
      enddo
      enddo
C  divide first complexes points by 2 to remove ridges
      call mltvect(tdata,tdata,0.5,size2)
      do i=1,size1
        tdata(size2*(i-1)+1) = tdata(size2*(i-1)+1)*0.5
        tdata(size2*(i-1)+2) = tdata(size2*(i-1)+2)*0.5
      enddo
      endif

      if (noise.ne.0.0) then      
        do i=1,size1*size2
           tdata(i) = tdata(i) + noise*gasdev(seed)
        enddo
      endif

      call bruvect(tdata,tdata,size1*size2)
      if (qtype.eq.1) then
        do i = 2, size1,4
                call mltvect( tdata(size2*(i)+1),
     *                tdata(size2*(i)+1),-1.0,2*size2)
        enddo
      endif
 
      return
      end
