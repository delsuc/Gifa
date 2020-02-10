C     This file is part of the GIFA program
C     This software has been developped by the NMR Group in GIF/Yvette.
C
C                     NMR Laboratory,
C                     ICSN - CNRS
C                     91190 GIF/YVETTE   FRANCE
C
C     This software cannot be used unless the user have been fully
C licensed to do so from the above laboratory.
C     This file cannot be copied, duplicated or used in any other program,
C without written permission from the authors.
C

      real dfreq,freq0,redf,delay,
     *     redfcalib,dcalib,dist,rfactor,ki2
      integer order,szd,nsvd,meth,
     *        ipvt(ldmax),nd1,nd2,szzn,szar
      real*8 power,rzero
      complex*16 r0

      common /lporder/  power,rzero, r0,dfreq,freq0,redf,delay,
     *     redfcalib,dcalib,dist,rfactor,ki2, order,szd,nsvd,meth,
     *     ipvt,nd1,nd2,szzn,szar

C	Include file to define equivalences
C of the buffers used by LPSVD package.
C NOT INCLUDED in LPSVD math routines....
c Un complexe double precision occupe 4 fois la place d'un reel, donc:
c	ldmax*ldmax -> 4*ldmax*ldmax

	complex*16 cu(ldmax*ldmax), cv(ldmax*ldmax), 
     *		ccx(ldmax*ldmax),z1(ldmax*ldmax)
	equivalence( dent(1), cu(1))
	equivalence( dchi(1), cv(1))
	equivalence( work(1), z1(1))
	equivalence( work2(1), ccx(1))

	complex*16 svd(2*ldmax),b(2*ldmax),wk(2*ldmax),
     *		a(2*ldmax),af(2*ldmax),ab(2*ldmax)
	complex*16 zn(2*ldmax),znf(2*ldmax) ,znb(2*ldmax),
     *		zn1(2*ldmax) ,zn2(2*ldmax)
	equivalence( work3(1), svd(1))
	equivalence( work3(1+(8*ldmax)), b(1))
	equivalence( work3(1+(16*ldmax)), wk(1))
	equivalence( work3(1+(24*ldmax)), a(1))
	equivalence( work3(1+(32*ldmax)), zn1(1))
	equivalence( work3(1+(40*ldmax)), zn2(1))
	equivalence( work3(1+(88*ldmax)), af(1))
	equivalence( work3(1+(96*ldmax)), ab(1))
	equivalence( work3(1+(104*ldmax)), zn(1))
	equivalence( work3(1+(112*ldmax)), znf(1))
	equivalence( work3(1+(120*ldmax)), znb(1))
c z1,a,b,wk work areas
c szd : size of the data used by the last DT->SVD command
c nd2 set to order by the last DT->SVD command
c nd1 : szd/2-nd2-1 in the following notation
c ccx(1..nd1,1..nd2)      : data matrix
c cu(1..nd1,1..nd1)       : left singular vector
c cv(1..nd2,1..nd2)       : rigth singular vector
c svd(1..nsvd)            : singular values
c af(1..szar) ab(1..szar) : forward, backward coeff
c zn(1..szzn) znf(1..szzn) znb(1..szzn) : matched, forward, backward roots
