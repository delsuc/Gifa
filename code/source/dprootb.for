      SUBROUTINE DPROOTB (MM,A,ZN,KERR,KPRINT)
c IN	: MM,A,KPRINT
c OUT	: KERR,ZN
C     MM DEGREE OF POLYNOMIAL
C     A=COEFFICIENT ARRAY, LOWEST TO HIGHEST DEGREE
C     KPRINT=1 FOR FULL PRINTING
C     KERR=0 IS NORMAL RETURN
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMPLEX*16 B(1024),C(1024),P,PP,Z,W,BB(1024),CC(1024),
     &A(*),SAVE(1024),ZN(*)
      REAL*8 PARTR,PARTI
#f2c      complex*16 zdum
#f2c      real*8 dreal
      COMMON /COMM/B,C,BB,CC,SAVE
C
#vms      Call Lib$flt_under(0)
#f2c      dreal(zdum) = dble(zdum)
      MMP=MM+1
      M=MM
      MP=MMP
      DO 700 I=1,MP
700    SAVE(I)=A(I)
C     KOUNT IS NUMBER OF ITERATIONS SO FAR
      KOUNT=0
C    KMAX IS MAXIMUM TOTAL NUMBER OF ITERATIONS ALLOWED
      KMAX=20*M
C NEWST IS NUMBER OF RE-STARTS
      NEWST=0
C     KTRYM IS NUMBER OF ATTEMPED ITERATIONS BEFORE RESTARTING
      KTRYM=20
C     KPOLM IS NUMBER OF ATTEMPED ITERATIONS BEFORE POLISHING IS STOPP
      KPOLM=20
C     AMAX IS THE LARGEST NUMBER WE ALLOW
      AMAX=2.D0**(32)
      AMIN=1.D0/AMAX
C     RR1 AND RR2 ARE RADII WITHIN WHICH WE WORK FOR POLISHING
      FM=DBLE(M)
      RR1=AMIN**(1.D0/FM)
      RR2=AMAX**(1.D0/FM)
C     EPS IS A TOLERANCE FOR CONVERGENCE
      EPS=1.D-10
      SQTEPS=DSQRT(EPS)
C     MAIN LOOP, M IS CURRENT DEGREE
10    IF(M.LE.0) GO TO 200
C     NEW Z, A POINT ON THE UNIT CIRCLE
      RKOUNT=DBLE(KOUNT)
      Z=DCMPLX(DCOS(RKOUNT),DSIN(RKOUNT))
      KTRY=0
C     R1 AND R2 ARE BOUNDARIES OF AN EXPANDING ANNULUS WITHIN
C     WHICH WE WORK
      FM=DBLE(M)
      R1=AMIN**(1.D0/FM)
      R2=AMAX**(1.D0/FM)
C     INSIDE LOOP
20    PARTR=DREAL(Z)
      PARTI=DIMAG(Z)
      SIZE=DSQRT(PARTR*PARTR+PARTI*PARTI)
      IF (SIZE.LT.R1.OR.SIZE.GT.R2) GO TO 300
      IF (KTRY.GE.KTRYM) GO TO 300
      KTRY=KTRY+1
      IF (KOUNT.GE.KMAX) GO TO 400
      KOUNT=KOUNT+1
C     GET VALUE OF POLYNOMIAL AT Z, SYNTHETIC DIVISION
      B(MP)=A(MP)
      DO 30 J=1,M
      K=M-J+1
30    B(K)=Z*B(K+1)+A(K)
      P=B(1)
      PARTR=DREAL(P)
      PARTI=DIMAG(P)
      IF(DSQRT(PARTR*PARTR+PARTI*PARTI).GT.AMAX) GO TO 300
C     GET VALUE OF DERIVATIVE AT Z, SYNTHETIC DIVISION
      C(MP)=B(MP)
      MDEC=M-1
      DO 60 J=1,MDEC
      K=M-J+1
60    C(K)=Z*C(K+1)+B(K)
      PP=C(2)
      PARTR=DREAL(PP)
      PARTI=DIMAG(PP)
      IF (DSQRT(PARTR*PARTR+PARTI*PARTI).LT.AMIN) GO TO 300
C     TEST FOR CONVERGENCE
      PARTR=DREAL(P)
      PARTI=DIMAG(P)
      SIZE=DSQRT(PARTR*PARTR+PARTI*PARTI)
      IF (SIZE.GT.EPS) GO TO 775
      NROOT=MM-M+1
      IF (KPRINT.EQ.1) WRITE(*,776) KOUNT,NROOT
776   FORMAT(' KOUNT='I5,' ROOT NO = ',I5)
      GO TO 500
775   CONTINUE
      Z=Z-P/PP
      GO TO 20
C     END OF MAIN LOOP
C     NORMAL RETURN
200   KERR=0
      GO TO 600
C     NEW START
300   RKOUNT=DBLE(KOUNT)
      Z=DCMPLX(DCOS(RKOUNT),DSIN(RKOUNT))
      KTRY=0
      NEWST=NEWST+1
      GO TO 20
C     TOO MANY ITERATIONS
400   KERR=400
      DO 450 I=M,MM
450	 ZN(I) = (0.D0,0.D0)
      GO TO 600
C     ROOT Z LOCATED
C     POLISH Z TO GET W
500   W=Z
      KPOL=0
510   PARTR=DREAL(W)
      PARTI=DIMAG(W)
      SIZE=DSQRT(PARTR*PARTR+PARTI*PARTI)
C     GIVE UP POLISHING IF W IS OUTSIDE ANNULUS
      IF (SIZE.LT.RR1.OR.SIZE.GT.RR2) GO TO 501
C    GIVE UP POLISHING IF KPOL>=KPOLM
      IF (KPOL.GE.KPOLM) GO TO 501
      KPOL=KPOL+1
      IF(KOUNT.GE.KMAX) GO TO 400
      KOUNT=KOUNT+1
      BB(MMP)=SAVE(MMP)
      DO 530 J=1,MM
      K=MM-J+1
530   BB(K)=W*BB(K+1)+SAVE(K)
      P=BB(1)
      PARTR=DREAL(P)
      PARTI=DIMAG(P)
      IF(DSQRT(PARTR*PARTR+PARTI*PARTI).GT.AMAX) GO TO 300
      CC(MMP)=BB(MMP)
      MDEC=MM-1
      DO 560 J=1,MDEC
      K=MM-J+1
560   CC(K)=W*CC(K+1)+BB(K)
      PP=CC(2)
      PARTR=DREAL(PP)
      PARTI=DIMAG(PP)
      IF(DSQRT(PARTR*PARTR+PARTI*PARTI).LT.AMIN) GO TO 300
      PARTR=DREAL(P)
      PARTI=DIMAG(P)
      SIZE=DSQRT(PARTR*PARTR+PARTI*PARTI)
C     TEST FOR CONVERGENCE OF POLISHING
      IF (SIZE.LE.EPS) GO TO 501
      W=W-P/PP
      GO TO 510
C     DEFLATE
501   B(MP)=A(MP)
      DO 830 J=1,M
      K=M-J+1
830   B(K)=Z*B(K+1)+A(K)
      P=B(1)
      ZN(M) = W
      IF(KPRINT.EQ.1)WRITE(*,11125)DBLE(ZN(M)),DIMAG(ZN(M))
      M=M-1
      MP=MP-1
      DO 100 J=1,MP
100   A(J)=B(J+1)
      GO TO 10
11125 FORMAT(2(D14.6,','))
C    REPORT AND RETURN
600   REAL1=DBLE(KOUNT)
      REAL2=DBLE(MM)
      TEMP=REAL1/REAL2
      if (kprint.eq.1) WRITE(*,150)KOUNT,TEMP
150   FORMAT(' KOUNT=',I10,'KOUNT/ROOT=',D15.5)
      IF(KPRINT.EQ.1)WRITE(*,151)NEWST,KERR
151   FORMAT(' NEW STARTS=',I10,'KERR=',I10)
      RETURN
      END
