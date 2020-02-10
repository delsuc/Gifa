      elseif (command.eq.'MINIMIZE') then
        if (miniter.lt.5) then
          call gifaout('Warning, MINITER is less than 5')
          call gifaout('  This algorithm needs larger values')
        endif
        call message
     *     ('expression to minimize (parameters are $Pi, i:1..n ):')
	call getstring2(st,error)
	call message('Number of independent parameters ($Pi):')
	call getint2(zz,error)
        if (error.ne.0) goto 91
	call miniinit(st,prmr1d,zz,error)
	if (error.ne.0) goto 91
	call powell(minimize,minimize1d,zz,prmr1d,itermax,miniter,
     *                    tolerance,zz1,verbose,error)
	if (error.ne.0) goto 91
	call gifaoutr('MINIMIZE Final Value :',zz1)
	call miniend(prmr1d, zz, verbose, error)
	leastsq = zz1
        iterdone = itermax

      elseif (command.eq.'FITGENE2') then
        if (miniter.lt.5) then
          call gifaout('Warning, MINITER is less than 5')
          call gifaout('  This algorithm needs larger values')
        endif
        if (szsamp.ne.sizeimage1d) goto 117
        call message
     *     ('expression to fit (parameters are $Pi, i:1..n ;
     * running var is $X):')
	call getstring2(st,error)
	call message('Number of independent parameters ($Pi):')
	call getint2(zz,error)
        if (error.ne.0) goto 91
	call fitgeneinit(st,prmr1d,zz,error)
	if (error.ne.0) goto 91
	call powell(fitgene,fitgene1d,zz,prmr1d,itermax,miniter,
     *                    tolerance,zz1,verbose,error)
	leastsq = zz1
	if (error.ne.0) goto 91
	call gifaoutr('FITGENE Final Chi2 :',zz1)
	call miniend(prmr1d, zz, verbose, error)
	leastsq = zz1
        iterdone = itermax
           
      elseif (command.eq.'FITGENE') then
        if (szsamp.ne.sizeimage1d) goto 117
        if (noise.eq.0.0) goto 133
        call message
     *     ('expression to fit (parameters are $Pi, i:1..n ;
     * running var is $X):')
	call getstring2(st,error)
	call message('Number of independent parameters ($Pi):')
	call getint2(zz,error)
        if (error.ne.0) goto 91
	call fitgeneinit(st,prmr1d,zz,error)
	if (error.ne.0) goto 91
	do i=1,zz
	   tindex(i)=i
	enddo
	call levenberg(sampling,column,window,sizeimage1d,noise,
     *      prmr1d,zz,
     *      tindex,zz,leastsq,tolerance,itermax,
     *      fitg2,dfitg2,work3,verbose,debug,error)
	if (error.ne.0) goto 91
	call gifaoutr('FITGENE Final Chi2 :',leastsq)
	call fitgend(prmr1d, leastsq, work3, zz,
     *     sizeimage1d,verbose, error)
        iterdone = itermax
           
      elseif (command.eq.'LINEFIT') then
        if (dim.eq.1) then
           if (it1d.ne.0) goto 96
           if (nbpic1d.gt.tlmxpk1) goto 144
        elseif (dim.eq.2) then
           if (itype.ne.0) goto 96
           if (nbpic2d.gt.tlmxpk2) goto 144
        else
            goto 124
        endif
        if (noise.eq.0.0) goto 133
        st = 'Lorentz'
        call message('fit : GAUSS or LORENTZ')
        call getstring2(st,error)
        call uppercase(st,80)
        if (dim.eq.1) then
          if (zoom.eq.0) then
            win1 = 1
            win2 = sizeimage1d
            zz = sizeimage1d
          else
            win1 = zo1dl
            win2 = zo1du
            zz = zo1du-zo1dl+1
          endif
          if (zz.gt.sdtmx) goto 148
          do i=1,zz                !xdata and ydata 
            absx(i)=float(i+win1-1)
            ordy(i)=column(i+win1-1)
          enddo
C set parameters to be optimized
          call prepprm1d(st,nbpic1d,tindex,win1,win2,verbose,i,error)
          if (error.ne.0) goto 93
          if (i.le.0) goto 135
          call initprm1d(nbpic1d,error)
          if (error.ne.0) goto 91
c call to levenberg makes the fit
          call levenberg(absx,ordy,window,zz,noise,prmr1d,
     *         4*nbpic1d,tindex,i,leastsq,tolerance,itermax,
     *         flogau,dflogau,work3,verbose,debug,error)
          if (error.ne.0) goto 90

          call postprm1d(leastsq,zz,nbpic1d,work3,tindex,i,verbose)
          leastsq = leastsq/float(zz)
          
        elseif (dim.eq.2) then
          if (zoom.eq.0) then
             win1=1
             win2=1
             win3=si2im
             win4=si1im
          else
             win1=zolf2
             win2=zolf1
             win3=zour2
             win4=zour1        
          endif
          zz = (win4-win2+1)*(win3-win1+1)
          if (zz.gt.sdtmx) goto 148
          k=0          ! load 1d buffers absx and ordy from 2d data (see fit2d)
          xx = max(si1im+1,si2im+1)      ! this is code for fit2d
          do i=win1,win3
             do j=win2,win4
               k = k+1
               absx(k) = float(i+xx*j)
               ordy(k) = plane2d(i+(j-1)*si2im)
              enddo
          enddo
C set parameters to be optimized
          call prepprm2d(st,nbpic2d,tindex,win1,win2,
     *                   win3,win4,verbose,i,error)
          if (error.ne.0) goto 93
          if (i.le.0) goto 135
          call initprm2d(nbpic2d,xx,error)
          if (error.ne.0) goto 91
c call to levenberg makes the fit
          call levenberg(absx,ordy,window,zz,noise,prmr2d,
     *         6*nbpic2d+1,tindex,i,leastsq,tolerance,itermax,
     *         flogau2d,dflogau2d,work3,verbose,debug,error)
          if (error.ne.0) goto 90

          call postprm2d(leastsq,zz,nbpic2d,work3,tindex,i,verbose)
	  leastsq = leastsq/float(zz)

        endif
        if (error.ne.0) goto 90
        call gifaoutr
     *  ('Value of the final normalized Chi Square : ', leastsq )
        iterdone = itermax


      elseif (command.eq.'LINEFITD') then
        if (dim.eq.1) then
           if (it1d.ne.0) goto 96
           if (nbpic1d.gt.tlmxpk1) goto 144
        elseif (dim.eq.2) then
           if (itype.ne.0) goto 96
           if (nbpic2d.gt.tlmxpk2) goto 144
        else
            goto 124
        endif
        if (noise.eq.0.0) goto 133
        if (dim.eq.1) then
          if (zoom.eq.0) then
            win1 = 1
            win2 = sizeimage1d
            zz = sizeimage1d
          else
            win1 = zo1dl
            win2 = zo1du
            zz = zo1du-zo1dl+1
          endif
          if (zz.gt.sdtmx) goto 148
          do i=1,zz                !xdata and ydata 
            absx(i)=float(i+win1-1)
            ordy(i)=column(i+win1-1)
          enddo
C set parameters to be optimized
          call selectprm1d(nbpic1d,tindex,win1,win2,verbose,i,error)
          if (error.ne.0) goto 93
          if (i.le.0) goto 135
          call initprm1d(nbpic1d,error)
          if (error.ne.0) goto 91
c call to levenberg makes the fit
          call levenberg(absx,ordy,window,zz,noise,prmr1d,
     *         4*nbpic1d,tindex,i,leastsq,tolerance,itermax,
     *         flogau,dflogau,work3,verbose,debug,error)
          if (error.ne.0) goto 90

          call postprm1d(leastsq,zz,nbpic1d,work3,tindex,i,verbose)
          leastsq = leastsq/float(zz)

        elseif (dim.eq.2) then
          if (zoom.eq.0) then
             win1=1
             win2=1
             win3=si2im
             win4=si1im
          else
             win1=zolf2
             win2=zolf1
             win3=zour2
             win4=zour1        
          endif
          zz = (win4-win2+1)*(win3-win1+1)
          if (zz.gt.sdtmx) goto 148
          k=0          ! load 1d buffers absx and ordy from 2d data (see fit2d)
          xx = max(si1im+1,si2im+1)      ! this is code for fit2d
          do i=win1,win3
             do j=win2,win4
               k = k+1
               absx(k) = float(i+xx*j)
               ordy(k) = plane2d(i+(j-1)*si2im)
              enddo
          enddo
C set parameters to be optimized
          call selectprm2d(nbpic2d,tindex,win1,win2,
     *                   win3,win4,verbose,i,error)
          if (error.ne.0) goto 93
          if (i.le.0) goto 135
          call initprm2d(nbpic2d,xx,error)
          if (error.ne.0) goto 91
c call to levenberg makes the fit
          call levenberg(absx,ordy,window,zz,noise,prmr2d,
     *         6*nbpic2d+1,tindex,i,leastsq,tolerance,itermax,
     *         flogau2d,dflogau2d,work3,verbose,debug,error)
          if (error.ne.0) goto 90

          call postprm2d(leastsq,zz,nbpic2d,work3,tindex,i,verbose)
          leastsq = leastsq/float(zz)

        else
          goto 124
        endif
        if (error.ne.0) goto 90
        call gifaoutr
     * ('Value of the final normalized Chi Square : ', leastsq )
        iterdone = itermax


      elseif (command.eq.'SHOWFIT') then
          if (dim.ne.1) goto 99
          if (nbpic1d.lt.1) goto 111
          win3=vd_id(1)
          if (zoom.eq.0) then
             win1=1
             win2=sizeimage1d
          else
             win1=zo1dl
             win2=zo1du
          endif
	  if (win2-win1+1.gt.sizemax) goto 148
          call initprm1d(nbpic1d,error)  !param. init.
          if (error.ne.0) goto 91
          do i=win1,win2
             call flogau(4*nbpic1d,prmr1d,float(i),work1d(i))
          enddo
c Affiche en couleur les courbes de chaque pic et la somme
          call gcolor(win3,2)
          call display1d(win3,win2-win1+1,work1d(win1),max0,
     *     scale,1,vheight,0)
          call gcolor(win3,color)





