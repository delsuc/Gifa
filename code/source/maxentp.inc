C This file contains the MAXENT commands for gifa
C
      elseif (command.eq.'MAXENT') then
	 memmode = 1
         if (dim.eq.3) goto 124
         if (empty.eq.0) goto 95
         if (noise.eq.0.0) goto 133
         if (nchannel.lt.1 .or.
     *       (nchannel.gt.1 .and. filset.ne.2)) goto 107
         iter = 0
         if (dim.eq.1) then          ! case of 1d spectra
           if (it1d.ne.1) goto 104
           xx = sizeimage1d
           if (xx.gt.sizemax) goto 103
           write(st,*) sizeimage1d
           call message ('Current size of data :'//st)
           call message ('Enter size of image :')
           call powtwo(xx,zz)
           yy = xx + zz
           call getint2(yy,error)
           call powtwo(yy,zz)
           if (zz.ne.0) goto 94
           if (yy.lt.sizeimage1d) call gifaout
     *          ('** image smaller than data ! **')
           yy = yy*nchannel
           if (yy.gt.sizemax .or. yy.lt.1) goto 103
           call copvect(datab,column,xx)
           call ttodata(1)
           sizeimage1d = yy
           it1d = 0

C the following is very sloppy, bringing back the old set-up (as in v2.5)
C This comes from the fact that maxent expects that the buffer `image' (in 
C commmon)  holds the current iteration result. 
C the whole maxent (itergul, controlgull etc..) should be rewritten
C in parameter passing mecanism rather than common passing.

           maxtemp = sizeimage
           sizeimage = sizeimage1d
           call excvect(column,image,sizeimage)
           call controlgull(verbose)
           call excvect(column,image,sizeimage)
           sizeimage = maxtemp
         elseif (dim.eq.2) then         ! case of 2d spectra
           if (itype.ne.3) goto 104
           if (si1im*si2im.gt.smxmax) goto 103
C  image still holds the data.
           write(st,*) si1im,si2im
           call message ('Data sizes in dim1, dim2 :'//st)
           call message ('Enter sizes of image in dim1, dim2:')
           call powtwo(si1im,zz)
           xx = si1im + zz
           call powtwo(si2im,zz)
           yy = si2im + zz
           call getint2(xx,error)
           call getint2(yy,error)
           if (xx.lt.si1im .or. yy.lt.si2im) call gifaout
     *          ('** image smaller than data ! **')
           zz = xx*yy*nchannel
           if (zz.gt.smxmax .or. zz.lt.1) goto 103
           call powtwo(xx,i)
           call powtwo(yy,j)
           if ((i+j).ne.0) goto 94
           sizedata=si1im*si2im
           call copvect(datab,plane2d,sizedata)
           call ttodata(2)
           si1im = xx
           si2im = yy
           sizeimage=nchannel*(si1im*si2im)
           itype = 0
           
C see comment above
           call excvect(plane2d,image,sizeimage)
           call controlgull(verbose)
           call excvect(plane2d,image,sizeimage)
           si1im = si1im*nchannel
         endif
         emptydata = .false.
         max0=0.0
         refresh = 1

      elseif (command.eq.'MAXENTCONT') then        !for 1d or 2d spectra
	 if (memmode.ne.1) goto 91
         if (dim.eq.3) goto 124
         if (noise.eq.0.0) goto 133
         if (iter.eq.0)  goto 91
         if (dim.eq.1) then
            if (it1d.ne.0) goto 104
            call powtwo(sizeimage1d/nchannel,zz)
            if (zz.ne.0) goto 94
            if (itdata.ne.1 .or.dimdat.ne.1) goto 104
            maxtemp = sizeimage
            sizeimage = sizeimage1d
            call excvect(column,image,sizeimage)
            call controlgull(verbose)
            call excvect(column,image,sizeimage)
            sizeimage = maxtemp
         elseif (dim.eq.2) then
            si1im = si1im/nchannel
            if (itype.ne.0) goto 104
            call powtwo(si1im,zz)
            call powtwo(si2im,yy)
            if (zz.ne.0 .or. yy.ne.0) goto 94
            if (itdata.ne.3 .or. dimdat.ne.2) goto 91
            call excvect(plane2d,image,sizeimage)
            call controlgull(verbose)
            call excvect(plane2d,image,sizeimage)
            si1im = si1im*nchannel
          endif
         max0=0.0
         refresh = 1

      elseif (command.eq.'RESIDUE') then        !for 1d or 2d spectra
         if ( emptydata ) goto 95
         if (dim.eq.3) goto 124
         if (iter.eq.0)  goto 91
         if (dim.eq.1) then
            if (it1d.ne.0) goto 91
            call powtwo(sizeimage1d/nchannel,zz)
            if (zz.ne.0) goto 94
            if (itdata.ne.1 .or.dimdat.ne.1) goto 104
            call transform(dchi,column,dim,1)
            call addvect(column,dchi,datab,-1.0,sizedata)
            sizeimage1d=sizedata
            it1d = itdata
         elseif (dim.eq.2) then
            if (itype.ne.0) goto 91
            call powtwo(si1im/nchannel,zz)
            call powtwo(si2im,yy)
            if (zz.ne.0 .or. yy.ne.0) goto 94
            if (itdata.ne.3 .or.dimdat.ne.2) goto 91
            si1im = si1dat
            si2im = si2dat
            call transform(dchi,plane2d,dim,1)
            call addvect(plane2d,dchi,datab,-1.0,sizedata)
            sizeimage=sizedata
            itype = itdata
         endif
         max0=0.0
         refresh = 1

      elseif (command.eq.'FILTER') then
         call message( '0: no filter, 1: automatic, 2: experimental. ')
         call getint2(filset,error)

      elseif (command.eq.'NCHANNEL') then
         call message( 'Number of channels for multi-channel MEM? ')
         call getint2(nchannel,error)
         if (nchannel.lt.1) goto 107

      elseif (command.eq.'SUMCONS') then
         call message( 'Sum constraint (0/1)? ')
         xx = sumcons
         call getint2(sumcons,error)
         if (sumcons.eq.1 .and. xx.eq.0) then
            keepsum = imago(imdim(dim))
            imago(imdim(dim)) = 0.0
            call gifaout('First point of data has been set to 0')
         elseif (sumcons.eq.0 .and. xx.eq.1) then
            if (keepsum.ne.0.0) then
               imago(imdim(dim)) = keepsum
               keepsum = 0.0
            endif
         else
            if (sumcons.ne.1 .and. sumcons.ne.0) then
              sumcons = 0
              goto 93
            endif
         endif

      elseif (command.eq.'NDISP') then
         call message
     *('(n) display every n iterations with MaxEnt processing.')
         call getint2(nndisp,error)

      elseif (command.eq.'INCREM') then
         call getreal2(inc,error)

      elseif (command.eq.'OVERIT') then
         call message
     *  ( 'Enable over-iteration (1), Disable over-iteration (0)? ')
         call getint2(overit,error)
 
      elseif (command.eq.'LAMBDA') then
         call getreal2(lambda,error)

      elseif (command.eq.'LAMBSP') then
         call getreal2(lambdasp,error)

      elseif (command.eq.'LAMBCONT') then
         call getint2(lambcont,error)
         if (lambcont.lt.0 .or. lambcont.gt.3) then
            lambcont = 1
            goto 98
         endif

      elseif (command.eq.'IVALUE') then
         call getreal2(ivalue,error)

      elseif (command.eq.'ALGO') then
         call message( '   1: GIFA equation')
         call message( '   2: Steepest descent')
         call message( '   3: Conjugate gradient')
         call message( ' +10: Use Line Minimization')
         call message( ' +20: Use Bracketing and Line Minimization')
         call message( '+100: Use Wu correction')
         call getint2(algo,error)

      elseif (command.eq.'STEP') then
         call getreal2(step,error)

      elseif (command.eq.'ITER') then
         call message( 'Enter number of iterations')
         call message( 'Enter 0 to iterate until converged:  ')
         call getint2(itermax,error)

      elseif (command.eq.'MINITER') then
         call message(
     *'Enter number of iterations for line-minimization:  ')
         call getint2(miniter,error)

      elseif (command.eq.'TOLERANCE') then
         call message(
     *'Enter tolerance on step for stopping line-mininization')
         call getreal2(tolerance,error)

      elseif (command.eq.'STEPMAX') then
         call message('Enter maximum step for line-minimization:  ')
         call getreal2(stepmax,error)

      elseif (command.eq.'OUTPUT') then
         call message( 'Disable numeric output (0),')
         call message(
     *    'Enable numeric output at each MaxEnt iteration (1)? ')
         call getint2(output,error)

      elseif (command.eq.'ESCALE') then
         call message( 'Enter scale for entropy:  ')
         call getreal2(escale,error)
         if (escale.eq.0.0) then
             call gifaout( 'Escale should not be 0.')
             escale = 1.0
         endif
 
      elseif (command.eq.'EXPLIM') then
         call message( 'Enter exponential linear limit:  ')
         call getreal2(aa,error)
         expa=exp(aa)

      elseif (command.eq.'LB') then  !for 1d or 2d spectra
         if (dim.eq.3) goto 124
         if (dim.eq.1) then               !case of 1d spectra
           call message( 'Enter Lorentzian line-brodening  ')
           call getfreq2(lb,sp1d,error)
         else                             !case of 2d spectra
           call message( '2D line-brodening.')
           call message('Enter LB in dim 1 and dim 2 (J1,J2):')
           call getfreq2(lb1,specw1,error)
           call getfreq2(lb,specw2,error)
         endif

      elseif (command.eq.'GB') then  !for 1d or 2d spectra
         if (dim.eq.3) goto 124
         if (dim.eq.1) then               !case of 1d spectra
           call message( 'Enter Gaussian line-brodening:   ')
           call getfreq2(gb,sp1d,error)
         else                             !case of 2d spectra
           call message( '2D gaussian broadening.')
           call message('Enter GB in dim 1 and dim 2 (J1,J2):')
           call getfreq2(gb1,specw1,error)
           call getfreq2(gb,specw2,error)
         endif

      elseif (command.eq.'JCONS') then  !for 1d or 2d spectra
         if (dim.eq.3) goto 124
         if (dim.eq.1) then               !case of 1d spectra
           call message( 'Enter J-coupling constant:  ')
           call getfreq2(j2,sp1d,error)
         else                             !case of 2d spectra
           call message( '2D j-coupling.')
           call message('Enter J in dim 1 and dim 2 (J1,J2):')
           call getfreq2(j1,specw1,error)
           call getfreq2(j2,specw2,error)
         endif
         call message( 'Phase or antiphase deconvolution (P/A)?')
         st = cc
         call getstring2(st,error)
         call uppercase(st,256)
         if (st.ne.'P' .and. st.ne.'A') go to 93
         cc = st(1:1)

      elseif (command.eq.'JMULT') then  !for 1d or 2d spectra
         if (dim.eq.3) goto 124
         if (dim.eq.1) then               !case of 1d spectra
           call message( 'Enter J-coupling constant:  ')
           call getfreq2(j2,sp1d,error)
           call message( 'Phase or antiphase deconvolution (P/A)?')
           st = cc
           call getstring2(st,error)
           call uppercase(st,256)
           if (st.ne.'P' .and. st.ne.'A') go to 93
           cc = st(1:1)
           call jmult(column,sizeimage1d,j2,cc,it1d)
         else                             !case of 2d spectra
           call message( '2D j-coupling')
           call message( 'Enter J in dim 1 and dim 2 (J1,J2):')
           call getfreq2(j1,specw1,error)
           call getfreq2(j2,specw2,error)
           call message( 'Phase or antiphase deconvolution (P/A)?')
           call getstring2(st,error)
           call uppercase(st,256)
           cc = st(1:1)
           if (cc.ne.'P' .and. cc.ne.'A') go to 93
           call j2d(plane2d,si1im,si2im,j1,j2,cc,itype)
         endif
         max0=0.0
         refresh = 1

      elseif (command.eq.'SHIFT') then
          call message(
     * 'Value of the overall baseline shift of the data-set')
          call getreal2(shift,error)

      elseif (command.eq.'NOISE') then
          call message(
     * 'Value of the noise value of the data-set')
         call getreal2(noise,error)
         sigma = noise*noise
         if (sigma.eq.0.0)  then
            sigma = 1.0
            goto 133
         endif

