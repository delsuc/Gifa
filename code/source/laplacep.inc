C This file contains the 'DOSY' (aka Laplace transform) type of processing

      elseif (command.eq.'LAPLACE') then
C LAPLACE
C Performs the Laplace transform of the current data-set.
C on-place in 1D,
C in F1 in 2D  or in 3D

        if (dim.ne.1) goto 124
	if (dmin.ge.dmax) goto 98
       	if (sizeimage1d.lt.2) goto 91
	if (it1d.ne.0) goto 96
	call message("final size:")
	xx = sizeimage1d
	call getint2(xx,error)
        if (xx.lt.2) goto 91
	if (error.ne.0) goto 93
        call laplace(column,sizeimage1d,work1d,xx,
     *		dmin,dmax,(0.5*float(xx-1)/sp1d) )
        max0 = 0.0
        refresh = 1
	call copvect(column,work1d,xx)
	sizeimage1d = xx

      elseif (command.eq.'TLAPLACE') then
C LAPLACE
C Performs the Laplace transform of the current data-set.
C on-place in 1D, for tabulated values
C in F1 in 2D  or in 3D

        if (dim.ne.1) goto 124
	if (dmin.ge.dmax) goto 98
       	if (sizeimage1d.lt.2) goto 91
	if (it1d.ne.0) goto 96
        call tlaplace(column,sizeimage1d,work1d,szsamp,
     *		dmin,dmax,sampling)
        max0 = 0.0
        refresh = 1
	call copvect(column,work1d,szsamp)
	sizeimage1d = szsamp

      elseif (command.eq.'DMIN') then
C DMIN  value
C sets the initial value for the laplace transform
         if (dfactor.le.0.0) dfactor = 1.0
         zz1 = dmin*dfactor
         call getreal2(zz1,error)
         if (error.ne.0) goto 93
         if (zz1 .eq. 0.0) goto 98
         dmin = zz1/dfactor

      elseif (command.eq.'DMAX') then
C DMAX  value
C sets the final value for the laplace transform
         if (dfactor.le.0.0) dfactor = 1.0
         zz1 = dmax*dfactor
         call getreal2(zz1,error)
         if (error.ne.0) goto 93
         if (zz1 .eq. 0.0) goto 98
         dmax = zz1/dfactor

      elseif (command.eq.'DFACTOR') then
C sets the ratio factor for Dampings
         zz1 = dfactor
         call getreal2(zz1,error)
         if (error.ne.0) goto 93
         if (zz1 .eq. 0.0) goto 98
         dfactor = zz1

      elseif (command.eq.'TRANSLAP') then
C Performs the transpose Laplace transform of the data-set in DATA
C on-place in 1D,
C in F1 in 2D  or in 3D
        if (dim.ne.1) goto 124
	if (dmin.ge.dmax) goto 98
       	if (sizeimage1d.lt.2) goto 91
	if (it1d.ne.0) goto 96
	if (error.ne.0) goto 93
	call ttranslap(column,datab)
        max0 = 0.0
        refresh = 1

      elseif (command.eq.'TRANSTLAP') then
C Performs the transpose Laplace transform of the data-set in DATA
C on-place in 1D, for tabulated values
C in F1 in 2D  or in 3D
        if (dim.ne.1) goto 124
	if (dmin.ge.dmax) goto 98
       	if (sizeimage1d.lt.2) goto 91
	if (it1d.ne.0) goto 96
	if (error.ne.0) goto 93
	call ttranstlap(column,datab)
        max0 = 0.0
        refresh = 1


      elseif (command.eq.'INVLAP') then
	memmode = 2
        if (dim.ne.1) goto 99
	if (dmin.ge.dmax) goto 98
       	if (sizeimage1d.lt.2) goto 91
	if (it1d.ne.0) goto 96
	call message("final size:")
	xx = sizeimage1d
	call getint2(xx,error)
        if (xx.lt.2) goto 91
	if (error.ne.0) goto 93
        if (noise.eq.0.0) goto 133
        iter = 0
        call copvect(datab,column,sizeimage1d)
        call ttodata(1)
	sizeimage1d = xx
C copied from MAXENT
        maxtemp = sizeimage
        sizeimage = sizeimage1d
        call excvect(column,image,sizeimage)
        call controlgull(verbose)
        call excvect(column,image,sizeimage)
        sizeimage = maxtemp
        max0=0.0
        emptydata = .false.
        refresh = 1

      elseif (command.eq.'INVLAPCONT') then
	if (memmode.ne.2) goto 91
        if (dim.ne.1) goto 124
	if (dmin.ge.dmax) goto 98
       	if (sizeimage1d.lt.2) goto 91
	if (it1d.ne.0) goto 96
        if (noise.eq.0.0) goto 133
C copied from MAXENT
        maxtemp = sizeimage
        sizeimage = sizeimage1d
        call excvect(column,image,sizeimage)
        call controlgull(verbose)
        call excvect(column,image,sizeimage)
        sizeimage = maxtemp
        max0=0.0
        refresh = 1

      elseif (command.eq.'INVTLAP') then
	memmode = 3
        if (dim.ne.1) goto 99
	if (dmin.ge.dmax) goto 98
       	if (sizeimage1d.lt.2) goto 91
       	if (sizeimage1d.ne.szsamp) goto 117
	if (it1d.ne.0) goto 96
	call message("final size:")
	xx = sizeimage1d
	call getint2(xx,error)
        if (xx.lt.2) goto 91
	if (error.ne.0) goto 93
        if (noise.eq.0.0) goto 133
	if (szsamp.lt.2) goto 109
c	do i=1, szsamp
c	  if(sampling(i).lt.0.0) goto 109
c	enddo
        iter = 0
        call copvect(datab,column,sizeimage1d)
        call ttodata(1)
	sizeimage1d = xx
C copied from MAXENT
        maxtemp = sizeimage
        sizeimage = sizeimage1d
        call excvect(column,image,sizeimage)
        call controlgull(verbose)
        call excvect(column,image,sizeimage)
        sizeimage = maxtemp
        max0=0.0
        emptydata = .false.
        refresh = 1

      elseif (command.eq.'INVTLAPCONT') then
	if (memmode.ne.3) goto 91
        if (dim.ne.1) goto 124
	if (dmin.ge.dmax) goto 98
       	if (sizeimage1d.lt.2) goto 91
	if (it1d.ne.0) goto 96
        if (noise.eq.0.0) goto 133
C copied from MAXENT
        maxtemp = sizeimage
        sizeimage = sizeimage1d
        call excvect(column,image,sizeimage)
        call controlgull(verbose)
        call excvect(column,image,sizeimage)
        sizeimage = maxtemp
        max0=0.0
        refresh = 1

      elseif (command.eq.'PUTTAB') then
	if (dim.ne.1) goto 99
	if (it1d.ne.0) goto 96
	if (sizeimage1d.gt.ldmax) goto 103
	call copvect(sampling,column,sizeimage1d)
	szsamp = sizeimage1d

      elseif (command.eq.'GETTAB') then
	if (dim.ne.1) goto 99
	sizeimage1d = szsamp
	call copvect(column,sampling,szsamp)
	max0 = 0.0
	refresh = 1
