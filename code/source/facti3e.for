C subroutine package for polynomial solving 
C THE SOLVING ALGORITHM CAN BE USED ONLY ON A IEEE COMPUTER !!!!!!!
C
C*****************************************************************************
	subroutine prfacti3e (ar,szar,zn,coeff,rootr,rooti)
c IN	: ar,szar,zn,coeff
c OUT	: rootr,rooti
c
c Prepare the input for subroutine specc, from the AR coefficients
c complex*16 ar(szar): AR coefficients
c conplex*16 zn(szar): roots
c
        implicit none
        
        integer szar,i
        complex*16 ar(szar),zn(szar)
        complex*16 coeff(szar+1)
        real*8 rootr(szar),rooti(szar)
        
c polynomial SIGMA COEFF(I)*Z**(I-1),I=1,szar+1)

        do i = 1,szar
           coeff(i) = ar(szar-i)
        enddo
        coeff(szar+1) = (1.d0,0.d0)
        call specc(coeff,rootr,rooti,szar+1)
        do i = 2,szar+1
           zn(i-1) = dcmplx(rootr(i),rooti(i))
        enddo
        
        return
        end
C*****************************************************************************
	SUBROUTINE SPERC(COEF,ROOTR,ROOTI,NCOF)
C	sperc.for		Version 1 26/4/1990		Patrice Koehl
C
C	New program to solve a polynomial equation with real coefficient
C	( derived from Bj. Svejgaarg, Bit, 7 (1967), 240-246
C
C
C	Input :		- Coef	: coefficients of the equation (with coef(1)
C				  coef of x**0 and coef(n) coef of x**(m)
C			- N	: M+1, with M order of the polynomial
C	Output :	- Rootr	: real part of the solutions
C			- Rooti : imaginary part of the solutions
C
C	Beware : on output, roots are stored in positions 2 to N
C	of arrays Rootr and Rooti
C
	implicit none
        integer ncof, n
	REAL*8		COEF(NCOF)
	REAL*8		ROOTR(NCOF),ROOTI(NCOF)
	REAL*8		U,V,W,K,M,F,FM,FC,XM,YM,XR,YR,XC,YC,DX,DY
C
	INTEGER		I,J,P
C
	N = NCOF
	DO I = 1,N
		ROOTR(I) = COEF(N+1-I)
	ENDDO
C
10	IF(N.LE.1) RETURN
C
	IF(ROOTR(N).EQ.0) THEN
		ROOTI(N) = 0
		N = N-1
		GOTO 10
	ENDIF
C
	P 	= N-1
	XC 	= 0
	YC	= 0
	FM	= ROOTR(N)**2
	FC	= FM
	DX	= ABS(ROOTR(N)/ROOTR(1))**(1./(N-1))
	DY	= 0
C
20	DO I = 1,4
		U = -DY
		DY = DX
		DX = U
		XR = XC + DX
		YR = YC + DY
		U  = 0
		V  = 0
		K  = 2*XR
		M  = XR**2 + YR**2
		DO J = 1,P
			W = ROOTR(J) + K*U - M*V
			V = U
			U = W
		ENDDO
		F = (ROOTR(N) + U*XR - M*V)**2 + (U*YR)**2
		IF(F.LT.FM) THEN
			XM = XR
			YM = YR
			FM = F
		ENDIF
	ENDDO
C
	IF(FM.LT.FC) THEN
		DX = 1.5*DX
		DY = 1.5*DY
		XC = XM
		YC = YM
		FC = FM
	ELSE
		U = .4*DX - .3*DY
		DY = .4*DY + .3*DX
		DX = U
	ENDIF
C
	U = ABS(XC) + ABS(YC)
C
	IF(U + ABS(DX) + ABS(DY).NE.U.AND.FC.NE.0) GOTO 20
C
	U = 0
	V = 0
	K = 2*XC
	M = XC**2
C
	DO J = 1,P
		W = ROOTR(J) + K*U - M*V
		V = U
		U = W
	ENDDO
C
	IF((ROOTR(N) + U*XC - M*V)**2.LE.FC) THEN
		U = 0
		DO J = 1,P
			ROOTR(J) = U*XC + ROOTR(J)
			U = ROOTR(J)
		ENDDO
		ROOTR(N) = XC
		ROOTI(N) = 0
	ELSE
		U = 0
		V = 0
		K = 2*XC
		M = XC**2 + YC**2
		P = N-2
		DO J = 1,P
			ROOTR(J) = ROOTR(J) + K*U-M*V
			W = ROOTR(J)
			V = U
			U = W
		ENDDO
		ROOTR(N-1) = XC
		ROOTR(N) = XC
		ROOTI(N) = YC
		ROOTI(N-1) = -YC
	ENDIF
C
	N = P
	GOTO 10
C
	END
C*****************************************************************************
	subroutine	specc(coef,rootr,rooti,ncoef)
C	Specc.for		Version 1 26/4/1990	Patrice Koehl
C
C	This subroutine evaluates the roots of a polynomial with complex
C	coefficient, using the method described by Svejgaard, in Bit 7 (1967)
C	p240-246
C
C
C	Input :		- COEF	complex coefficient of the polynomial
C			  coef(1) is the coefficient of x**0, and coef(ncoef)
C			  is the coefficient of x**(ncoef-1)
C			- NCOEF	number of coefficient ( i.e. the order of the
C			  	polynomial is NCOEF-1)
C	Output :	- ROOTR	is the real part of the solution
C			- ROOTI is the complex part of the solution
C
C	Beware :
C		1) on output, roots are stored in positions 2 to NCOEF of
C		arrays rootr and rooti
C		2) works in double precision (real*8 and complex*16)
C
	implicit none
	integer         i,j,n,ncoef
	real * 8        u,v,w,f,fm,fc,xm,ym,xr,yr,xc,yc,dx,dy,us,vs,d
	real * 8	rootr(ncoef),rooti(ncoef)	! Attention
C
	complex * 16    coef(ncoef)
C
C
	n = ncoef
	do 100 i = 1,n
#f2c		rootr(i) = dble(coef(n+1-i))
#_f2c		rootr(i) = dreal(coef(n+1-i))
		rooti(i) = dimag(coef(n+1-i))
100	continue
C
	us = rootr(1)
	vs = rooti(1)
	d = dabs(us) + dabs(vs)
C
200	if(n.le.1) return
C
	fc = dabs(rootr(n)) + dabs(rooti(n))
	fm = fc
	if(fm.eq.0.d0) then
		n = n - 1
		goto 200
	endif
C
	xc = 0.d0
	yc = 0.d0
	dx = (fm/d)**(1.d0/(n-1))
	dy = 0.d0
C
300	do 500 i = 1,4
		u = -dy
		dy = dx
		dx = u
		xr = xc + dx
		yr = yc + dy
		u = us
		v = vs
		do 400 j = 2,n
			w = rootr(j) + u*xr - v*yr
			v = rooti(j) + u*yr + v*xr
			u = w
400		continue
		f = dabs(u) + dabs(v)
		if(f.lt.fm) then
			xm = xr
			ym = yr
			fm = f
		endif
500	continue
c
	if(fm.lt.fc) then
		dx = 1.5*dx
		dy = 1.5*dy
		xc = xm
		yc = ym
		fc = fm
	else
		u = 0.4d0 * dx - 0.3d0*dy
		dy = 0.4*dy + 0.3*dx
		dx = u
	endif
c
	u = dabs(xc) + dabs(yc)
c
	if(u+dabs(dx)+dabs(dy).ne.u.and.fc.ne.0) goto 300
c
	u = us
	v = vs
	rootr(n) = xc
	rooti(n) = yc
	n = n - 1
	do 600 j = 2,n
		rootr(j) = rootr(j) + u*xc - v*yc
		w = rootr(j)
		rooti(j) = rooti(j) + u*yc + v*xc
		v = rooti(j)
		u = w
600	continue
c
	goto 200
c
	end
