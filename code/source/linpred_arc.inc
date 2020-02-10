C     This file is part of the GIFA program
C     This software has been developped by the NMR Group in GIF/Yvette.
C
C     Authors :       M.A.Delsuc, T.Malliavin
C                     NMR Laboratory,
C                     ICSN - CNRS
C                     91190 GIF/YVETTE   FRANCE
C
C     This software cannot be used unless the user have been fully
C licensed to do so from the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.
C

c      elseif (command.eq.'WRITEC') then
c         call message( 'Enter file name:  ')
c         call getstring2(name,error)
c         if(order.lt.1) goto 122
c         call gifaouti('order of the AR evaluation : ',order)
c    if (dim.eq.1) then
c       xx = 1
c       yy = sizeimage
c         else
c            xx = si1im
c            yy = si2im
c    endif
c         if (yy.gt.smxmax) goto 103
c         if(order.gt.yy/2 .or. order.gt.2*ldmax) goto 122
c         call writec (name,image,xx,yy,specw1,
c     &     specw2,offset1,offset2,it1d,rate,order)
c         write(st,270) rate
c    call gifaout(st)
c
c      elseif (command.eq.'READC') then
c         call message( 'Enter file name:  ')
c         call getstring2(name,error)
c         call readc (name,image,xx,yy,specw1,
c     &     specw2,offset1,offset2,it1d) 
c         if(xx.eq.1 ) then
c              if ( dim.ne.1) call gifaout('reading a 1D file')
c              dim = 1
c              sizeimage = yy
c         elseif (xx.ne.1) then
c              if ( dim.eq.1) call gifaout('reading a 2D file')
c              dim = 2
c              si1im = xx
c              si2im = yy
c         endif
c         max0=0.0
c         refresh = 1

      elseif (command.eq.'DT->AR') then
         if (it1d.ne.1) goto 104
         if (dim.ne.1) goto 99
         if (sizeimage1d.gt.sizemax) goto 103
         if (order.lt.1) goto 121
         if (order.gt.sizeimage1d/2 .or. order.gt.2*ldmax) goto 121
         if (input.eq.20)
     *      call gifaouti('order of the AR evaluation : ',order)
c        call copvect(datab,image,sizeimage1d)
c        sizedata = sizeimage1d
         call burgc(column,sizeimage1d/2,order,power,
     *             af,work,work2,error)
         if (error.ne.0) goto 90
         if (af(order).eq.0.d0) then
            call ddcopvect(ab,af,2*order)
         else
            call backward(ab,af,order)
         endif
         szar = order
         write(*,*)'power',power
c         itdata = it1d
         max0=0.0
         emptydata = .false.

      elseif (command.eq.'BURG') then
         if (it1d.ne.1) goto 104
         if (dim.ne.1) goto 99
         if(order.lt.1) goto 121
         if (input.eq.20)
     *   call gifaouti('order of the AR evaluation : ',order)
         if (dim.eq.1) then
           zz = 2*sizeimage1d
           call message ('Final size?')
           call getint2(zz,error)
           if(order.gt.sizeimage1d/2) goto 121
           if (zz.le.sizeimage1d) goto 93
           if (zz.gt.sizemax) goto 103
           if (mod(zz,2).ne.0) goto 92
           call predloc(column,order,sizeimage1d/2,zz/2,verbose,error)
           if (error.ne.0) goto 90
           sizeimage1d = zz
c         elseif (dim.eq.2) then
c           call message('In which direction')
c           zz = 1
c           call getdim(zz,error)
c           if (error.ne.0) goto 93
c           if (zz.eq.1) then
c              zz =2*si1im
c              call message ('Final size?')
c              call getint2(zz,error)
c              call checkunderflow(zz*si2im,bool1)
c              if (bool1) goto 90
         else
            goto 124
         endif
         szar = order
         max0=0.0
         refresh=1

      elseif (command.eq.'AR->SP') then
        if (it1d.ne.1) goto 104
        if (dim.ne.1) goto 99
        if (sizeimage1d.gt.sizemax) goto 103
        if(szar.lt.1) goto 121
        if(szar.gt.sizeimage1d/2 .or. szar.gt.2*ldmax) goto 121
        call burgspectrum (column,sizeimage1d,power,af,a,work1d,
     *                     szar,sp1d)
        max0=0.0
        refresh=1

      elseif (command.eq.'ARLIST') then
         if(szar.lt.1) goto 121
         if(szar.gt.ldmax) goto 121
         call gifaouti('Number of AR coefficients : ',szar)
         zz = 1
         call message('AR coeff. forward (1) or backward(2) ?')
         call getint2(zz,error)
         xx = 1
         yy = szar
         call message('From which element ?')
         call getint2(xx,error)
         call message('To which element ?')
         call getint2(yy,error)
         if ((xx.lt.1).or.(xx.gt.szar).or.(yy.lt.1).or.
     &       (yy.gt.szar).or.(yy.lt.xx)) goto 98
         if (zz.eq.1) then
           call gifaout('Forward coeff. of PE polynome')
           call listdcpx(af,szar,xx,yy)
         elseif (zz.eq.2 ) then
            call gifaout('Backward coeff. of PE polynome')
            call listdcpx(ab,szar,xx,yy)
         else
            goto 93
         endif

      elseif (command.eq.'SVDLIST') then
         call gifaouti('number of SVD : ',nsvd)
         if(nsvd.lt.1) goto 121
         if(nsvd.gt.ldmax) goto 121
         xx = 1
         yy = nsvd
         call message('From which element ?')
         call getint2(xx,error)
         call message('To which element ?')
         call getint2(yy,error)
         if ((xx.lt.1).or.(xx.gt.nsvd).or.(yy.lt.1).or.
     &       (yy.gt.nsvd).or.(yy.lt.xx)) goto 98 
         call gifaout('Singular values')
         call listdcpx(svd,nsvd,xx,yy)


      elseif (command.eq.'RTLIST') then
         call gifaouti('Number of Root ',szzn)
         if(szzn.lt.1) goto 121
         if(szzn.gt.ldmax) goto 121
         call message('roots forward (1), backward(2) or 
     * "matched"(3)?')
         call getint2(zz,error)
         xx = 1
         yy = szzn
         call message('From which element ?')
         call getint2(xx,error)
         call message('To which element ?')
         call getint2(yy,error)
         if ((xx.lt.1).or.(xx.gt.szzn).or.(yy.lt.1).or.
     &       (yy.gt.szzn).or.(yy.lt.xx)) goto 98
         if (zz.eq.1) then
            call gifaout('forward roots of PE polynome')
            call listdcpx(znf,szzn,xx,yy)
         elseif (zz.eq.2) then
            call gifaout('backward roots of PE polynome')
            call listdcpx(znb,szzn,xx,yy)
         elseif (zz.eq.3) then
            call gifaout('matched roots of PE polynome')
            call listdcpx(zn,szzn,xx,yy)
         else
            goto 93
         endif

      elseif (command.eq.'FREQLIST') then
         freq0 = 0.0
         call message('Frequences centered on ?')
         call getreal2(freq0,error)
         dfreq = 0.1
         call message('Maximum shift of frequence ?')
         call getreal2(dfreq,error)
         zz = 1
         call message('roots forward (1) or backward(2) ?')
         call getint2(zz,error)
         if (zz.eq.1) then
            call gifaout('forward roots of PE polynome')
            call listfreq(znf,szzn,dfreq,freq0,tindex,sp1d)
         elseif (zz.eq.2) then
            call gifaout('backward roots of PE polynome')
            call listfreq(znb,szzn,dfreq,freq0,tindex,sp1d)
         else
            goto 93
         endif

      elseif (command.eq.'AR->DT') then
        if (it1d.ne.1) goto 104
        if (dim.ne.1) goto 99
         if (sizeimage1d.gt.sizemax) goto 103
         if(szar.lt.1) goto 121
         if(szar.gt.sizeimage1d/2 .or. szar.gt.2*ldmax) goto 121
         zz = sizeimage1d
         call message ('Final size?')
         call getint2(zz,error)
         if (zz.lt.sizeimage1d .or. error.ne.0) goto 93
         if (zz.gt.sizemax) goto 103
         if (mod(zz,2).ne.0) goto 92
         yy = 1
         call message('Prediction forward (1) or backward(2)?')
         call getint2(yy,error)
         if (error.ne.0) goto 90
         if (yy.eq.1) then
            call lpforw(column,zz/2,af,szar,column,sizeimage1d/2,error)
         elseif (yy.eq.2) then
            call lpback(column,zz/2,ab,szar,column,sizeimage1d/2,error)
         else
            goto 93
         endif
         if (error.eq.1) call gifaout('Overflow during prediction!')
         sizeimage1d = zz
         max0=0.0
         refresh=1

      elseif (command.eq.'ORDER') then
       zz = order
       call message ('Order for Linear prediction ?')
       call getint2 (zz,error)
       if (zz.gt.2*ldmax) goto 121
       order = zz

      elseif (command.eq.'DT->SVD') then
        if (it1d.ne.1) goto 104
        if (dim.ne.1) goto 99
        if (sizeimage1d.gt.sizemax) goto 103
        if (order.lt.1) goto 121
        if (input.eq.20)
     *      call gifaouti('order of the AR evaluation : ',order)
        szd = sizeimage1d
        call message('Size of data considered ?')
        call getint2(szd,error)
        if (mod(szd,2).ne.0) goto 92
c To get nd1 greater than order
        if (szd.lt.(2 + (order*4))) goto 98
c To get x(nd1,order) not too big
        xx = (szd/2) - order - 1
c the matrix X should enter into (ldmax*ldmax)
        if ((xx*order).gt.(ldmax*ldmax) ) goto 98
c the matrix U should enter into (ldmax*ldmax)       
        if ((order*order).gt.(ldmax*ldmax) ) goto 98
c the matrix V should enter into (ldmax*ldmax)         
        if ((xx*xx).gt.(ldmax*ldmax) ) goto 98
c the matrix svd should enter into (2*ldmax)         
        if (order.gt.(2*ldmax) ) goto 98
        nd1 = xx
        nd2 = order
        call fairemat (column,szd/2,nd2,ccx,nd1)
        call calcsvd (ccx,svd,cu,cv,a,wk,szd/2,nd2,nd1)
        nsvd = nd2

      elseif (command.eq.'SVDCLEAN1') then
        if (it1d.ne.1) goto 104
        if (dim.ne.1) goto 99
        if (sizeimage1d.gt.sizemax) goto 103
        if (nsvd.lt.1) goto 121
        if (nsvd.gt.sizeimage1d/4 .or. nsvd.gt.ldmax) goto 121
        call message (' Number of svd to keep ?')
        yy = nsvd
        call getint2 (yy,error)
        if (yy.gt.nsvd .or. yy.lt.1 .or. error.ne.0) goto 93
        call message(' Do you want to reduce noise power ?')
        call getbool2(st,'svdclean',error)
        if (error.ne.0) goto 93
        if (st.eq.'YES') then 
          zz = 1
        else
          zz = 0
        endif
        call clesvd (svd,nsvd,yy,zz)
        nsvd = yy
        
      elseif (command.eq.'SVDCLEAN2') then
        if (it1d.ne.1) goto 104
        if (dim.ne.1) goto 99
        if (sizeimage1d.gt.sizemax) goto 103
        if (nsvd.lt.1) goto 121
        if (nsvd.gt.sizeimage1d/4 .or. nsvd.gt.ldmax) goto 121
        call message (' Level value ?')
        call getreal2 (y,error)
        if (y.lt.0.0 .or. error.ne.0) goto 93 
        call message(' Do you want to reduce noise power ?')
        call getbool2(st,'svdclean',error)
        if (error.ne.0) goto 93
        if (st.eq.'YES') then 
          zz = 1
        else
          zz = 0
        endif
        call countlevel (svd,nsvd,yy,zz,y)
        nsvd = yy

      elseif (command.eq.'SVD->DT') then
        if (it1d.ne.1) goto 104
        if (dim.ne.1) goto 99
        if (nsvd.lt.1 .or. nd2.lt.1) goto 121
c To get nd1 greater than nd2
        if (szd.lt.(2+nd2*4) ) goto 98
c To get x(nd1,nd2) not too big
        nd1 = (szd/2) - nd2 - 1
c the matrix X should enter into (ldmax*ldmax)
        if ((nd1*nd2).gt.(ldmax*ldmax) ) goto 98
c the matrix U should enter into (ldmax*ldmax)       
        if ((nd1*nd1).gt.(ldmax*ldmax) ) goto 98
c the matrix V should enter into (ldmax*ldmax)         
        if ((nd2*nd2).gt.(ldmax*ldmax) ) goto 98
c the matrix svd should enter into (2*ldmax)         
        if (order.gt.(2*ldmax) ) goto 98
        if (nsvd.gt.nd2) goto 93
        call recalcdat (svd,nsvd,column,szd/2,nd2,
     &      nd1,cu,cv,ccx)
        sizeimage1d = szd
        max0=0.0
        refresh = 1

      elseif (command.eq.'SVD->AR') then
        if (it1d.ne.1) goto 104
        if (dim.ne.1) goto 99
        if (sizeimage1d.gt.sizemax) goto 103
        if (nd2.lt.1) goto 121
c To get nd1 greater than nd2
        if (szd.lt.(2+nd2*4) ) goto 98
c To get x(nd1,nd2) no to big
        nd1 = (szd/2) - nd2 - 1
c the matrix X should enter into (ldmax*ldmax)
        if ((nd1*nd2).gt.(ldmax*ldmax) ) goto 98
c the matrix U should enter into (ldmax*ldmax)       
        if ((nd1*nd1).gt.(ldmax*ldmax) ) goto 98
c the matrix V should enter into (ldmax*ldmax)         
        if ((nd2*nd2).gt.(ldmax*ldmax) ) goto 98
c the matrix svd should enter into (2*ldmax)         
        if (order.gt.(2*ldmax) ) goto 98
        zz = 3
        call message('coeff forward (1) or backward(2) or both(3)?')
        call getint2(zz,error)
        if (zz.lt.1 .or. zz.gt.3) goto 93
        if (zz.eq.1 .or. zz.eq.3) then
c         call message ('Forward AR coeff')
          call polyarf (svd,nsvd,column,szd/2,af,
     *          nd2,nd1,cu,cv,wk,b,a)
        endif
        if (zz.eq.2 .or. zz.eq.3) then
c           call message ('Backward AR coeff')
            call polyarb (svd,nsvd,column,szd/2,ab,
     *          nd2,nd1,cu,cv,wk,a,b)
        endif
        szar = nd2

      elseif (command.eq.'RT->AR') then
        if (it1d.ne.1) goto 104
        if (dim.ne.1) goto 99
        if (sizeimage1d.gt.sizemax) goto 103
        if (szzn.lt.1) goto 121
        if (szzn.gt.sizeimage1d/4 .or. szzn.gt.ldmax) goto 121
        zz = 3
        call message('Roots forward (1) or backward(2) or matched (3)?')
        call getint2(zz,error)
        if (zz.lt.1 .or. zz.gt.3) goto 93
        if (zz.eq.1) then
c           call message ('Forward AR coeff')
            call polynom (znf,a,szzn,af)
        endif
        if (zz.eq.2) then
c           call message ('Backward AR coeff')
            call polynom (znb,a,szzn,ab)
        endif
        if (zz.eq.3) then
c           call message ('Forward and Backward AR coeff')
            call polynom (zn,a,szzn,af)
            call polynom (zn,a,szzn,ab)
        endif
        szar = szzn
     
      elseif (command.eq.'AR->RT') then
        if (it1d.ne.1) goto 104
        if (dim.ne.1) goto 99
        if (sizeimage1d.gt.sizemax) goto 103
        if(szar.lt.1) goto 121
        if(szar.gt.sizeimage1d/4 .or. szar.gt.ldmax) goto 121
        zz = 3
        call message('roots forward (1) or backward(2) or both(3)?')
        call getint2(zz,error)
        if (error.ne.0) goto 90
        if (zz.lt.1 .or. zz.gt.3) goto 93
        if (zz.eq.1 .or. zz.eq.3) then
c         call message ('Forward roots')
          call factorf (af,szar+1,znf,verbose,error)
        endif
        if (zz.eq.2 .or. zz.eq.3) then
c         call message ('Backward roots')
          call factorf (ab,szar+1,znb,verbose,error)
        endif
        szzn = szar

      elseif (command.eq.'AR->RT2') then
        if (it1d.ne.1) goto 104
        if (dim.ne.1) goto 99
        if (sizeimage1d.gt.sizemax) goto 103
        if(szar.lt.1) goto 121
        if(szar.gt.sizeimage1d/4 .or. szar.gt.ldmax) goto 121
        zz = 3
        call message('roots forward (1) or backward(2) or both(3)?')
        call getint2(zz,error)
        if (error.ne.0) goto 90
        if (zz.lt.1 .or. zz.gt.3) goto 93
        if (zz.eq.1 .or. zz.eq.3) then
          call prfacti3e (af,szar,znf,b,wk,a)
        endif
        if (zz.eq.2 .or. zz.eq.3) then
          call prfacti3e (ab,szar,znb,b,wk,a)
        endif
        szzn = szar

      elseif (command.eq.'RTINV') then
        zz = 1
        call message('roots forward (1) or backward(2) ?')
        call getint2(zz,error)
        if (zz.eq.1) then
           call inverse (znb,znf,szzn)
        elseif (zz.eq.2) then 
           call inverse (znf,znb,szzn)
        else
           goto 98
        endif

      elseif (command.eq.'RTCLEAN') then
        zz = 1
        call message('roots forward (1) or backward(2) ?')
        call getint2(zz,error)
       if (zz.eq.1) then
          call clean (znf,szzn,yy)
       elseif (zz.eq.2) then 
          call clean (znb,szzn,yy)
       else
          goto 98
       endif
       szzn = yy
       call gifaouti('new number of Roots : ',szzn)

      elseif (command.eq.'RTREFLECT') then
        zz = 1
        call message('roots forward (1) or backward(2) ?')
        call getint2(zz,error)
        if (zz.eq.1) then
           call reflect (znf,szzn)
        elseif (zz.eq.2) then 
           call reflect (znb,szzn)
        else
           goto 98
        endif

      elseif (command.eq.'RTMATCH') then
        call message (' Number of exponentials to extract ?')
        call getint2 (zz,error)
        if (zz.gt.szzn .or. zz.lt.1) goto 98
        call match (znf,znb,szzn,zz,ccx,zn)
        szzn = zz

      elseif (command.eq.'RT->PK') then
        if (it1d.ne.1) goto 104
        if (dim.ne.1) goto 99
        if (sizeimage1d.gt.sizemax) goto 103
        if (szzn.lt.1) goto 121
        if (szzn.gt.sizeimage1d/4 .or. szzn.gt.ldmax) goto 121
c        szd = sizeimage1d
        call message('Size of data considered ?')
        call getint2(szd,error)
        if (mod(szd,2).ne.0) goto 92
        if (szd.lt.szzn*2 .or. szd.gt.sizeimage1d) goto 98
        zz = 1
        call message('roots forward (1) or "matched"(2)?')
        call getint2(zz,error)
        nbpic1d = szzn
        porig1d = 4
        z = float(sizeimage1d)
        call message('First point delay?')
        call getreal2(y,error)
        if (error.ne.0) goto 93
        y = y*sp1d/z
        if (zz.eq.1) then
          call extpic (znf,peak1d(1,2),peak1d(1,1),peak1d(1,3),
     *      peak1d(1,4),ipvt,nbpic1d,z1,wk,column,szd/2,a,
     *      -z,z,error,stdev,y)
        elseif (zz.eq.2) then
          call extpic (zn,peak1d(1,2),peak1d(1,1),peak1d(1,3),
     *      peak1d(1,4),ipvt,nbpic1d,z1,wk,column,szd/2,a,
     *      -z,z,error,stdev,y)
        else
          goto 93
        endif
        if (error.ne.0) goto 90
        call gifaoutr('Standard deviation is : ',stdev)

      elseif (command.eq.'RTSELECT') then
        write(st,*)'Number of roots : ',szzn
        call message(st)
        i = 1
        yy = 1
        call message('roots forward (1), backward(2) or 
     * "matched"(3)?')
        call getint2(yy,error)
        call message( 'Enter index for each point, enter 0 to end.')
        bool1 = .true.
        do while (bool1)
           zz = tindex(i)
           call getint2 (zz,error)
           if (zz.eq.0) bool1 = .false.
           if (zz.gt.szzn) goto 98
           tindex(i) = zz
           i = i + 1
        enddo
        if (yy.eq.1) then
           call rselect (znf,tindex,i,szzn)
        elseif (yy.eq.2) then
           call rselect(znb,tindex,i,szzn)
        elseif (yy.eq.3) then
           call rselect(zn,tindex,i,szzn)
        else
           goto 98
        endif
        szzn = i-1
        call gifaouti('New number of Roots',szzn)

      elseif (command.eq.'ZHU') then
         if (dim.ne.1) goto 99
         if (it1d.ne.1) goto 104
         if (2*sizeimage1d.gt.sizemax) goto 103
         call powtwo(sizeimage1d,zz)
         if (zz.ne.0) goto 94
         call message('where is the origin of time t=0?') 
         call message('-1: acquisition delayed by 1 dwell time')
         call message(' 0: first complex point')
         call message(' n: nth complex point')
         call getint2(zz,error)   
         zz = 2*zz + 2
         call copvect(work1d,column(zz+1),sizeimage1d-zz)
         call uswvect(work1d,sizeimage1d)
         call revvect(work1d,sizeimage1d/2)
         call revvect(work1d(sizeimage1d/2+1),sizeimage1d/2)
         call mltvect(work1d(sizeimage1d/2+1),
     *                work1d(sizeimage1d/2+1),-1.0,
     *                                    sizeimage1d/2)
         call swavect(work1d,sizeimage1d)
         call copvect(work1d(sizeimage1d+1),column,sizeimage1d)
         call copvect(column,work1d(zz+1),sizeimage1d*2-zz)
         sizeimage1d = 2*sizeimage1d-zz
         refresh = 1

      elseif (command.eq.'ZHUOLD') then
         if (dim.ne.1) goto 99
         if (it1d.ne.1) goto 104
         if (2*sizeimage1d.gt.sizemax) goto 103
         call copvect(work1d,column,sizeimage1d)
         call uswvect(work1d,sizeimage1d)
         call revvect(work1d,sizeimage1d/2)
         call revvect(work1d(sizeimage1d/2+1),sizeimage1d/2)
         z = -1.0
         call mltvect(work1d((sizeimage1d/2)+1),
     &          work1d((sizeimage1d/2)+1),z,sizeimage1d/2)
c         do i=1,sizeimage1d/2
c           work1d(sizeimage1d/2+i) = -work1d(sizeimage1d/2+i)
c         enddo
         call swavect(work1d,sizeimage1d)
         call copvect(work1d(sizeimage1d+1),column,sizeimage1d)
         call copvect(column,work1d,sizeimage1d*2)
         sizeimage1d = 2*sizeimage1d
         refresh = 1

      elseif (command.eq.'RELAXRATE') then
      if (sp1d.eq.0.0) goto 93
      if (nbpic1d.eq.0) goto 111
      call redfield(peak1d(1,4),peak1d(1,2),peak1d(1,3),nbpic1d,
     &          redf,sp1d)
      if (redf.ge.0.) then
           write(st,273)redf
      else
           write(st,274)redf
      endif
      call gifaout(st)

      elseif (command.eq.'RCRYST') then
        if (dim.ne.1) goto 99
        if (it1d.ne.1) goto 104
        if (sp1d.eq.0.) goto 93
c        szd = sizeimage1d
        call message('Size of data considered ?')
        call getint2(szd,error)
        if (mod(szd,2).ne.0) goto 92
        if (szd.gt.sizeimage1d) goto 98
        call rcryst (column,szd/2,sp1d,peak1d(1,4),peak1d(1,2),
     &    peak1d(1,3),peak1d(1,1),nbpic1d,ki2,rfactor)
        call gifaoutr('Value of the "R factor" : ',rfactor)
        call gifaoutr('Value of the ki2 : ',ki2)

      elseif (command.eq.'SLOPE') then      
        if (dim.ne.1) goto 99
        if (it1d.ne.0) goto 104
        call message('How many points do you take for estimate ?')
        zz = sizeimage1d
        call getint2(zz,error)
        if (zz.le.0) goto 93
        if (sp1d.eq.0.) goto 93
        call pente (column,zz,sp1d,redf)
        if (redf.ge.0.) then
           call gifaoutr('Be careful !  negative slope !',redf)
        else
           call gifaoutr('Value of the slope : ',redf)
        endif

      elseif (command.eq.'CALIBDI') then      
      call message ('Calibrating distance ?')
      call getreal2(dcalib,error)
      if (dcalib.le.0) goto 93
      call message ('Calibrating Relaxation rate ?')
      call getreal2(redfcalib,error)
      if (redfcalib.ge.0) goto 93
    
      elseif (command.eq.'METH') then      
      call message ('Hydrogenes pair is: Hyd-hyd(0),hyd-methyl(1)
     &      or methyl-methyl(2) ?')
      meth = 1
      call getint2(meth,error)

      elseif (command.eq.'DIST') then      
      if (nbpic1d.eq.0 .or. redf.eq.0.0) goto 111
      if (redf.gt.0.) then
        write(st,*)'Warning: relaxation rate is positive'
        goto 90
      endif
      if (meth.eq.0) then
        dist = dcalib*((redfcalib/redf)**(1.0/6.0))
        write(st,*)'Value of the distance : ',dist
      elseif (meth.eq.1) then 
        dist = dcalib*((sqrt(3.0)*
     &          redfcalib/redf)**(1.0/6.0))
        write(st,*)'Value of the distance : ',dist
      elseif (meth.eq.2) then 
        dist = dcalib*((3.0*
     &          redfcalib/redf)**(1.0/6.0))
        write(st,*)'Value of the distance : ',dist
      endif
      call gifaout(st)

      elseif (command.eq.'RTPIV') then
        call message('Cone extent?')
        z = 6.0
        call getreal2(z,error)
        call rtfreq (znf,szzn,tindex,z)

270    format (' packing is : ',f6.2,' %.')
273    format (' value of the relaxation rate : positive relaxation 
     &  rate',e)
274    format (' value of the relaxation rate : ',e)






