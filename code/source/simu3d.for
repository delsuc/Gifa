      subroutine simu3d(tdata,size1,size2,size3,noise,itype,err)
c OUT	: tdata,size1,size2,size3,noise,err
c
c simulates a 3D FID
c
      implicit none   

#include "sizeparam.inc"

      integer i,seed,j,nline,size1,size2,size3,size12,
     *        i1,i2,i3,cpx,err,itype
      real    tdata(*)
      real  noise,zz,z12,gasdev,z,w1,w2,w3,
     *      phase1(10),phase2(10),phase3(10),
     *      freq1(10),freq2(10),freq3(10),
     *      t2(10),t1(10),t3(10),amp(10),
     *      cw1,sw1,cw2,sw2,cw3,sw3,e1,e2,
     *      rr,ri,ir,ii
      character*(40) line
      save

C init tdata                     

#vms       call Lib$flt_under(0)
2     call message( 'HyperComplex(1) or Real(0)')
      call getint2(cpx,err)
      if (cpx.ne.0 .and. cpx.ne.1) then
          err = 1
          return
      endif
1     call message( 'data size in dim1:')
      call getint2( size1,err)                   !size1/2 complex points
      call message('data size in dim2:'  )
      call getint2( size2,err)                    !size2/2 complex points
      call message('data size in dim3:'  )
      call getint2( size3,err)                    !size3/2 complex points
      if (size1*size2*size3.gt.smxbig) goto 1
      call message( 'number of lines:')
      call getint2( nline,err) 
      do j=1,nline
        write(line,*)  j,'th line'
        call message(line)
        call message( '   amplitude')
        call getreal2( amp(j),err) 
        call message( '   frequency in dim3:')
        call getreal2( freq3(j),err) 
        call message( '   frequency in dim2:')
        call getreal2( freq2(j),err) 
        call message( '   frequency in dim1:')
        call getreal2( freq1(j),err) 
        call message( '   t2 in dim3')
        call getreal2( t3(j)  ,err) 
        call message( '   t2 in dim2')
        call getreal2( t2(j) ,err)  
        call message( '   t2 in dim1')
        call getreal2( t1(j),err) 
        call message( '   phase in dim3:')
        call getreal2( phase3(j),err) 
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
      if (err.ne.0) return

      call prpvect(tdata,0.0,size1*size2*size3)
      size12 = size3*size2
      do j=1,nline
      if (cpx.eq.1) then
      do i1=1,size1,2
        w1 = freq1(j)*float(i1-1)+phase1(j) 
        cw1 = cos(w1)
        sw1 = sin(w1)
        e1 = exp(-float(i1-1)/t1(j))
        do i2=1,size2,2
           w2 = freq2(j)*float(i2-1)+phase2(j) 
           cw2 = cos(w2)
           sw2 = sin(w2)
           e2 = exp(-float(i2-1)/t2(j))
           z12 = e2*e1*amp(j)
           do i3=1,size3,2
             z = z12*exp(-float(i3-1)/t3(j))
             w3 = freq3(j)*float(i3-1)+phase3(j)
             cw3 = cos(w3)
             sw3 = sin(w3)
             rr = z*cw2*cw3
             ri = z*cw2*sw3
             ir = z*sw2*cw3
             ii = z*sw2*sw3
             tdata(i3+size3*(i2-1)+size12*(i1-1) ) =
     *       tdata(i3+size3*(i2-1)+size12*(i1-1) ) + rr*cw1
             tdata(i3+size3*(i2-1)+size12*(i1) ) =
     *       tdata(i3+size3*(i2-1)+size12*(i1) ) + rr*sw1

             tdata(i3+size3*(i2)+size12*(i1-1) ) =
     *       tdata(i3+size3*(i2)+size12*(i1-1) ) + ir*cw1
             tdata(i3+size3*(i2)+size12*(i1) ) =
     *       tdata(i3+size3*(i2)+size12*(i1) ) + ir*sw1

             tdata(i3+1+size3*(i2-1)+size12*(i1-1) ) =
     *       tdata(i3+1+size3*(i2-1)+size12*(i1-1) ) + ri*cw1
             tdata(i3+1+size3*(i2-1)+size12*(i1) ) =
     *       tdata(i3+1+size3*(i2-1)+size12*(i1) ) + ri*sw1

             tdata(i3+1+size3*(i2)+size12*(i1-1) ) =
     *       tdata(i3+1+size3*(i2)+size12*(i1-1) ) + ii*cw1
             tdata(i3+1+size3*(i2)+size12*(i1) ) =
     *       tdata(i3+1+size3*(i2)+size12*(i1) ) + ii*sw1

           enddo 
         enddo
      enddo
      else    ! (if cpx.eq.0)   (real case)
      do i1=1,size1
        w1 = freq1(j)*float(i1-1)+phase1(j) 
        cw1 = cos(w1)
        e1 = exp(-float(i1-1)/t1(j))
        do i2=1,size2
           w2 = freq2(j)*float(i2-1)+phase2(j) 
           cw2 = cos(w2)
           e2 = exp(-float(i2-1)/t2(j))
           z12 = e2*e1*amp(j)
           do i3=1,size3
             z = z12*exp(-float(i3-1)/t3(j))
             w3 = freq3(j)*float(i3-1)+phase3(j)
             cw3 = cos(w3)
             rr = z*cw2*cw3
             tdata(i3+size3*(i2-1)+size12*(i1-1) ) =
     *       tdata(i3+size3*(i2-1)+size12*(i1-1) ) + rr*cw1

           enddo 
         enddo
      enddo
      endif
      enddo

C  divide first complexes points by 2 to remove ridges
c f23
      call mltvect(tdata,tdata,0.5,size3*size2)
c f13
      do i=2,size1
        call mltvect(tdata(size2*size3*(i-1)+1),
     *               tdata(size2*size3*(i-1)+1),0.5,size3)
      enddo
c f12
      do i1=1,size1
        do i2=1,size2
          tdata(1+size3*(i2-1)+size12*(i1-1) ) =
     *         tdata(1+size3*(i2-1)+size12*(i1-1) )*0.5
        enddo
      enddo

      if (noise.ne.0.0) then      
        do i=1,size1*size2*size3
           tdata(i) = tdata(i) + noise*gasdev(seed)
        enddo
      endif

      if (cpx.eq.1) then
        itype = 7
      else
        itype = 0
      endif
      return
      end
