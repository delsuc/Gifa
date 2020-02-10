C regular single precision blas.
c
c
c
c
      SUBROUTINE SAXPY(N,SA,SX,INCX,SY,INCY)                            00000010
C                                                                       00000020
C     CONSTANT TIMES A VECTOR PLUS A VECTOR.                            00000030
C     USES UNROLLED LOOP FOR INCREMENTS EQUAL TO ONE.                   00000040
C     JACK DONGARRA, LINPACK, 3/11/78.                                  00000050
C                                                                       00000060
      REAL SX(1),SY(1),SA                                               00000070
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N                                 00000080
C                                                                       00000090
      IF(N.LE.0)RETURN                                                  00000100
      IF (SA .EQ. 0.0) RETURN                                           00000110
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20                               00000120
C                                                                       00000130
C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS                00000140
C          NOT EQUAL TO 1                                               00000150
C                                                                       00000160
      IX = 1                                                            00000170
      IY = 1                                                            00000180
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1                                 00000190
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1                                 00000200
      DO 10 I = 1,N                                                     00000210
        SY(IY) = SY(IY) + SA*SX(IX)                                     00000220
        IX = IX + INCX                                                  00000230
        IY = IY + INCY                                                  00000240
   10 CONTINUE                                                          00000250
      RETURN                                                            00000260
C                                                                       00000270
C        CODE FOR BOTH INCREMENTS EQUAL TO 1                            00000280
C                                                                       00000290
C                                                                       00000300
C        CLEAN-UP LOOP                                                  00000310
C                                                                       00000320
   20 M = MOD(N,4)                                                      00000330
      IF( M .EQ. 0 ) GO TO 40                                           00000340
      DO 30 I = 1,M                                                     00000350
        SY(I) = SY(I) + SA*SX(I)                                        00000360
   30 CONTINUE                                                          00000370
      IF( N .LT. 4 ) RETURN                                             00000380
   40 MP1 = M + 1                                                       00000390
      DO 50 I = MP1,N,4                                                 00000400
        SY(I) = SY(I) + SA*SX(I)                                        00000410
        SY(I + 1) = SY(I + 1) + SA*SX(I + 1)                            00000420
        SY(I + 2) = SY(I + 2) + SA*SX(I + 2)                            00000430
        SY(I + 3) = SY(I + 3) + SA*SX(I + 3)                            00000440
   50 CONTINUE                                                          00000450
      RETURN                                                            00000460
      END                                                               00000470
      SUBROUTINE  SCOPY(N,SX,INCX,SY,INCY)                              00000010
C                                                                       00000020
C     COPIES A VECTOR, X, TO A VECTOR, Y.                               00000030
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO 1.                    00000040
C     JACK DONGARRA, LINPACK, 3/11/78.                                  00000050
C                                                                       00000060
      REAL SX(1),SY(1)                                                  00000070
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N                                 00000080
C                                                                       00000090
      IF(N.LE.0)RETURN                                                  00000100
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20                               00000110
C                                                                       00000120
C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS                00000130
C          NOT EQUAL TO 1                                               00000140
C                                                                       00000150
      IX = 1                                                            00000160
      IY = 1                                                            00000170
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1                                 00000180
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1                                 00000190
      DO 10 I = 1,N                                                     00000200
        SY(IY) = SX(IX)                                                 00000210
        IX = IX + INCX                                                  00000220
        IY = IY + INCY                                                  00000230
   10 CONTINUE                                                          00000240
      RETURN                                                            00000250
C                                                                       00000260
C        CODE FOR BOTH INCREMENTS EQUAL TO 1                            00000270
C                                                                       00000280
C                                                                       00000290
C        CLEAN-UP LOOP                                                  00000300
C                                                                       00000310
   20 M = MOD(N,7)                                                      00000320
      IF( M .EQ. 0 ) GO TO 40                                           00000330
      DO 30 I = 1,M                                                     00000340
        SY(I) = SX(I)                                                   00000350
   30 CONTINUE                                                          00000360
      IF( N .LT. 7 ) RETURN                                             00000370
   40 MP1 = M + 1                                                       00000380
      DO 50 I = MP1,N,7                                                 00000390
        SY(I) = SX(I)                                                   00000400
        SY(I + 1) = SX(I + 1)                                           00000410
        SY(I + 2) = SX(I + 2)                                           00000420
        SY(I + 3) = SX(I + 3)                                           00000430
        SY(I + 4) = SX(I + 4)                                           00000440
        SY(I + 5) = SX(I + 5)                                           00000450
        SY(I + 6) = SX(I + 6)                                           00000460
   50 CONTINUE                                                          00000470
      RETURN                                                            00000480
      END                                                               00000490
      REAL FUNCTION SDOT(N,SX,INCX,SY,INCY)                             00000010
C                                                                       00000020
C     FORMS THE DOT PRODUCT OF TWO VECTORS.                             00000030
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.                  00000040
C     JACK DONGARRA, LINPACK, 3/11/78.                                  00000050
C                                                                       00000060
      REAL SX(1),SY(1),STEMP                                            00000070
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N                                 00000080
C                                                                       00000090
      STEMP = 0.0E0                                                     00000100
      SDOT = 0.0E0                                                      00000110
      IF(N.LE.0)RETURN                                                  00000120
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20                               00000130
C                                                                       00000140
C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS                00000150
C          NOT EQUAL TO 1                                               00000160
C                                                                       00000170
      IX = 1                                                            00000180
      IY = 1                                                            00000190
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1                                 00000200
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1                                 00000210
      DO 10 I = 1,N                                                     00000220
        STEMP = STEMP + SX(IX)*SY(IY)                                   00000230
        IX = IX + INCX                                                  00000240
        IY = IY + INCY                                                  00000250
   10 CONTINUE                                                          00000260
      SDOT = STEMP                                                      00000270
      RETURN                                                            00000280
C                                                                       00000290
C        CODE FOR BOTH INCREMENTS EQUAL TO 1                            00000300
C                                                                       00000310
C                                                                       00000320
C        CLEAN-UP LOOP                                                  00000330
C                                                                       00000340
   20 M = MOD(N,5)                                                      00000350
      IF( M .EQ. 0 ) GO TO 40                                           00000360
      DO 30 I = 1,M                                                     00000370
        STEMP = STEMP + SX(I)*SY(I)                                     00000380
   30 CONTINUE                                                          00000390
      IF( N .LT. 5 ) GO TO 60                                           00000400
   40 MP1 = M + 1                                                       00000410
      DO 50 I = MP1,N,5                                                 00000420
        STEMP = STEMP + SX(I)*SY(I) + SX(I + 1)*SY(I + 1) +             00000430
     *   SX(I + 2)*SY(I + 2) + SX(I + 3)*SY(I + 3) + SX(I + 4)*SY(I + 4)00000440
   50 CONTINUE                                                          00000450
   60 SDOT = STEMP                                                      00000460
      RETURN                                                            00000470
      END                                                               00000480
      REAL FUNCTION SMACH(JOB)                                          00000010
      INTEGER JOB                                                       00000020
C                                                                       00000030
C     SMACH COMPUTES MACHINE PARAMETERS OF FLOATING POINT               00000040
C     ARITHMETIC FOR USE IN TESTING ONLY.  NOT REQUIRED BY              00000050
C     LINPACK PROPER.                                                   00000060
C                                                                       00000070
C     IF TROUBLE WITH AUTOMATIC COMPUTATION OF THESE QUANTITIES,        00000080
C     THEY CAN BE SET BY DIRECT ASSIGNMENT STATEMENTS.                  00000090
C     ASSUME THE COMPUTER HAS                                           00000100
C                                                                       00000110
C        B = BASE OF ARITHMETIC                                         00000120
C        T = NUMBER OF BASE  B  DIGITS                                  00000130
C        L = SMALLEST POSSIBLE EXPONENT                                 00000140
C        U = LARGEST POSSIBLE EXPONENT                                  00000150
C                                                                       00000160
C     THEN                                                              00000170
C                                                                       00000180
C        EPS = B**(1-T)                                                 00000190
C        TINY = 100.0*B**(-L+T)                                         00000200
C        HUGE = 0.01*B**(U-T)                                           00000210
C                                                                       00000220
C     DMACH SAME AS SMACH EXCEPT T, L, U APPLY TO                       00000230
C     DOUBLE PRECISION.                                                 00000240
C                                                                       00000250
C     CMACH SAME AS SMACH EXCEPT IF COMPLEX DIVISION                    00000260
C     IS DONE BY                                                        00000270
C                                                                       00000280
C        1/(X+I*Y) = (X-I*Y)/(X**2+Y**2)                                00000290
C                                                                       00000300
C     THEN                                                              00000310
C                                                                       00000320
C        TINY = SQRT(TINY)                                              00000330
C        HUGE = SQRT(HUGE)                                              00000340
C                                                                       00000350
C                                                                       00000360
C     JOB IS 1, 2 OR 3 FOR EPSILON, TINY AND HUGE, RESPECTIVELY.        00000370
C                                                                       00000380
C                                                                       00000390
      REAL EPS,TINY,HUGE,S                                              00000400
C                                                                       00000410
      EPS = 1.0                                                         00000420
   10 EPS = EPS/2.0                                                     00000430
      S = 1.0 + EPS                                                     00000440
      IF (S .GT. 1.0) GO TO 10                                          00000450
      EPS = 2.0*EPS                                                     00000460
C                                                                       00000470
      S = 1.0                                                           00000480
   20 TINY = S                                                          00000490
      S = S/16.0                                                        00000500
      IF (S*100. .NE. 0.0) GO TO 20                                     00000510
      TINY = (TINY/EPS)*100.0                                           00000520
      HUGE = 1.0/TINY                                                   00000530
C                                                                       00000540
      IF (JOB .EQ. 1) SMACH = EPS                                       00000550
      IF (JOB .EQ. 2) SMACH = TINY                                      00000560
      IF (JOB .EQ. 3) SMACH = HUGE                                      00000570
      RETURN                                                            00000580
      END                                                               00000590
      REAL FUNCTION SNRM2 ( N, SX, INCX)                                00000010
      INTEGER          NEXT                                             00000020
      REAL   SX(1),  CUTLO, CUTHI, HITEST, SUM, XMAX, ZERO, ONE         00000030
      DATA   ZERO, ONE /0.0E0, 1.0E0/                                   00000040
C                                                                       00000050
C     EUCLIDEAN NORM OF THE N-VECTOR STORED IN SX() WITH STORAGE        00000060
C     INCREMENT INCX .                                                  00000070
C     IF    N .LE. 0 RETURN WITH RESULT = 0.                            00000080
C     IF N .GE. 1 THEN INCX MUST BE .GE. 1                              00000090
C                                                                       00000100
C           C.L.LAWSON, 1978 JAN 08                                     00000110
C                                                                       00000120
C     FOUR PHASE METHOD     USING TWO BUILT-IN CONSTANTS THAT ARE       00000130
C     HOPEFULLY APPLICABLE TO ALL MACHINES.                             00000140
C         CUTLO = MAXIMUM OF  SQRT(U/EPS)  OVER ALL KNOWN MACHINES.     00000150
C         CUTHI = MINIMUM OF  SQRT(V)      OVER ALL KNOWN MACHINES.     00000160
C     WHERE                                                             00000170
C         EPS = SMALLEST NO. SUCH THAT EPS + 1. .GT. 1.                 00000180
C         U   = SMALLEST POSITIVE NO.   (UNDERFLOW LIMIT)               00000190
C         V   = LARGEST  NO.            (OVERFLOW  LIMIT)               00000200
C                                                                       00000210
C     BRIEF OUTLINE OF ALGORITHM..                                      00000220
C                                                                       00000230
C     PHASE 1    SCANS ZERO COMPONENTS.                                 00000240
C     MOVE TO PHASE 2 WHEN A COMPONENT IS NONZERO AND .LE. CUTLO        00000250
C     MOVE TO PHASE 3 WHEN A COMPONENT IS .GT. CUTLO                    00000260
C     MOVE TO PHASE 4 WHEN A COMPONENT IS .GE. CUTHI/M                  00000270
C     WHERE M = N FOR X() REAL AND M = 2*N FOR COMPLEX.                 00000280
C                                                                       00000290
C     VALUES FOR CUTLO AND CUTHI..                                      00000300
C     FROM THE ENVIRONMENTAL PARAMETERS LISTED IN THE IMSL CONVERTER    00000310
C     DOCUMENT THE LIMITING VALUES ARE AS FOLLOWS..                     00000320
C     CUTLO, S.P.   U/EPS = 2**(-102) FOR  HONEYWELL.  CLOSE SECONDS ARE00000330
C                   UNIVAC AND DEC AT 2**(-103)                         00000340
C                   THUS CUTLO = 2**(-51) = 4.44089E-16                 00000350
C     CUTHI, S.P.   V = 2**127 FOR UNIVAC, HONEYWELL, AND DEC.          00000360
C                   THUS CUTHI = 2**(63.5) = 1.30438E19                 00000370
C     CUTLO, D.P.   U/EPS = 2**(-67) FOR HONEYWELL AND DEC.             00000380
C                   THUS CUTLO = 2**(-33.5) = 8.23181D-11               00000390
C     CUTHI, D.P.   SAME AS S.P.  CUTHI = 1.30438D19                    00000400
C     DATA CUTLO, CUTHI / 8.232D-11,  1.304D19 /                        00000410
C     DATA CUTLO, CUTHI / 4.441E-16,  1.304E19 /                        00000420
      DATA CUTLO, CUTHI / 4.441E-16,  1.304E19 /                        00000430
C                                                                       00000440
      IF(N .GT. 0) GO TO 10                                             00000450
         SNRM2  = ZERO                                                  00000460
         GO TO 300                                                      00000470
C                                                                       00000480
   10 ASSIGN 30 TO NEXT                                                 00000490
      SUM = ZERO                                                        00000500
      NN = N * INCX                                                     00000510
C                                                 BEGIN MAIN LOOP       00000520
      I = 1                                                             00000530
   20    GO TO NEXT,(30, 50, 70, 110)                                   00000540
   30 IF( ABS(SX(I)) .GT. CUTLO) GO TO 85                               00000550
      ASSIGN 50 TO NEXT                                                 00000560
      XMAX = ZERO                                                       00000570
C                                                                       00000580
C                        PHASE 1.  SUM IS ZERO                          00000590
C                                                                       00000600
   50 IF( SX(I) .EQ. ZERO) GO TO 200                                    00000610
      IF( ABS(SX(I)) .GT. CUTLO) GO TO 85                               00000620
C                                                                       00000630
C                                PREPARE FOR PHASE 2.                   00000640
      ASSIGN 70 TO NEXT                                                 00000650
      GO TO 105                                                         00000660
C                                                                       00000670
C                                PREPARE FOR PHASE 4.                   00000680
C                                                                       00000690
  100 I = J                                                             00000700
      ASSIGN 110 TO NEXT                                                00000710
      SUM = (SUM / SX(I)) / SX(I)                                       00000720
  105 XMAX = ABS(SX(I))                                                 00000730
      GO TO 115                                                         00000740
C                                                                       00000750
C                   PHASE 2.  SUM IS SMALL.                             00000760
C                             SCALE TO AVOID DESTRUCTIVE UNDERFLOW.     00000770
C                                                                       00000780
   70 IF( ABS(SX(I)) .GT. CUTLO ) GO TO 75                              00000790
C                                                                       00000800
C                     COMMON CODE FOR PHASES 2 AND 4.                   00000810
C                     IN PHASE 4 SUM IS LARGE.  SCALE TO AVOID OVERFLOW.00000820
C                                                                       00000830
  110 IF( ABS(SX(I)) .LE. XMAX ) GO TO 115                              00000840
         SUM = ONE + SUM * (XMAX / SX(I))**2                            00000850
         XMAX = ABS(SX(I))                                              00000860
         GO TO 200                                                      00000870
C                                                                       00000880
  115 SUM = SUM + (SX(I)/XMAX)**2                                       00000890
      GO TO 200                                                         00000900
C                                                                       00000910
C                                                                       00000920
C                  PREPARE FOR PHASE 3.                                 00000930
C                                                                       00000940
   75 SUM = (SUM * XMAX) * XMAX                                         00000950
C                                                                       00000960
C                                                                       00000970
C     FOR REAL OR D.P. SET HITEST = CUTHI/N                             00000980
C     FOR COMPLEX      SET HITEST = CUTHI/(2*N)                         00000990
C                                                                       00001000
   85 HITEST = CUTHI/FLOAT( N )                                         00001010
C                                                                       00001020
C                   PHASE 3.  SUM IS MID-RANGE.  NO SCALING.            00001030
C                                                                       00001040
      DO 95 J =I,NN,INCX                                                00001050
      IF(ABS(SX(J)) .GE. HITEST) GO TO 100                              00001060
   95    SUM = SUM + SX(J)**2                                           00001070
      SNRM2 = SQRT( SUM )                                               00001080
      GO TO 300                                                         00001090
C                                                                       00001100
  200 CONTINUE                                                          00001110
      I = I + INCX                                                      00001120
      IF ( I .LE. NN ) GO TO 20                                         00001130
C                                                                       00001140
C              END OF MAIN LOOP.                                        00001150
C                                                                       00001160
C              COMPUTE SQUARE ROOT AND ADJUST FOR SCALING.              00001170
C                                                                       00001180
      SNRM2 = XMAX * SQRT(SUM)                                          00001190
  300 CONTINUE                                                          00001200
      RETURN                                                            00001210
      END                                                               00001220
      SUBROUTINE  SROT (N,SX,INCX,SY,INCY,C,S)                          00000010
C                                                                       00000020
C     APPLIES A PLANE ROTATION.                                         00000030
C     JACK DONGARRA, LINPACK, 3/11/78.                                  00000040
C                                                                       00000050
      REAL SX(1),SY(1),STEMP,C,S                                        00000060
      INTEGER I,INCX,INCY,IX,IY,N                                       00000070
C                                                                       00000080
      IF(N.LE.0)RETURN                                                  00000090
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20                               00000100
C                                                                       00000110
C       CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS NOT EQUAL       00000120
C         TO 1                                                          00000130
C                                                                       00000140
      IX = 1                                                            00000150
      IY = 1                                                            00000160
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1                                 00000170
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1                                 00000180
      DO 10 I = 1,N                                                     00000190
        STEMP = C*SX(IX) + S*SY(IY)                                     00000200
        SY(IY) = C*SY(IY) - S*SX(IX)                                    00000210
        SX(IX) = STEMP                                                  00000220
        IX = IX + INCX                                                  00000230
        IY = IY + INCY                                                  00000240
   10 CONTINUE                                                          00000250
      RETURN                                                            00000260
C                                                                       00000270
C       CODE FOR BOTH INCREMENTS EQUAL TO 1                             00000280
C                                                                       00000290
   20 DO 30 I = 1,N                                                     00000300
        STEMP = C*SX(I) + S*SY(I)                                       00000310
        SY(I) = C*SY(I) - S*SX(I)                                       00000320
        SX(I) = STEMP                                                   00000330
   30 CONTINUE                                                          00000340
      RETURN                                                            00000350
      END                                                               00000360
      SUBROUTINE SROTG(SA,SB,C,S)                                       00000010
C                                                                       00000020
C     CONSTRUCT GIVENS PLANE ROTATION.                                  00000030
C     JACK DONGARRA, LINPACK, 3/11/78.                                  00000040
C                                                                       00000050
      REAL SA,SB,C,S,ROE,SCALE,R,Z                                      00000060
C                                                                       00000070
      ROE = SB                                                          00000080
      IF( ABS(SA) .GT. ABS(SB) ) ROE = SA                               00000090
      SCALE = ABS(SA) + ABS(SB)                                         00000100
      IF( SCALE .NE. 0.0 ) GO TO 10                                     00000110
         C = 1.0                                                        00000120
         S = 0.0                                                        00000130
         R = 0.0                                                        00000140
         GO TO 20                                                       00000150
   10 R = SCALE*SQRT((SA/SCALE)**2 + (SB/SCALE)**2)                     00000160
      R = SIGN(1.0,ROE)*R                                               00000170
      C = SA/R                                                          00000180
      S = SB/R                                                          00000190
   20 Z = 1.0                                                           00000200
      IF( ABS(SA) .GT. ABS(SB) ) Z = S                                  00000210
      IF( ABS(SB) .GE. ABS(SA) .AND. C .NE. 0.0 ) Z = 1.0/C             00000220
      SA = R                                                            00000230
      SB = Z                                                            00000240
      RETURN                                                            00000250
      END                                                               00000260
      SUBROUTINE  SSCAL(N,SA,SX,INCX)                                   00000010
C                                                                       00000020
C     SCALES A VECTOR BY A CONSTANT.                                    00000030
C     USES UNROLLED LOOPS FOR INCREMENT EQUAL TO 1.                     00000040
C     JACK DONGARRA, LINPACK, 3/11/78.                                  00000050
C                                                                       00000060
      REAL SA,SX(1)                                                     00000070
      INTEGER I,INCX,M,MP1,N,NINCX                                      00000080
C                                                                       00000090
      IF(N.LE.0)RETURN                                                  00000100
      IF(INCX.EQ.1)GO TO 20                                             00000110
C                                                                       00000120
C        CODE FOR INCREMENT NOT EQUAL TO 1                              00000130
C                                                                       00000140
      NINCX = N*INCX                                                    00000150
      DO 10 I = 1,NINCX,INCX                                            00000160
        SX(I) = SA*SX(I)                                                00000170
   10 CONTINUE                                                          00000180
      RETURN                                                            00000190
C                                                                       00000200
C        CODE FOR INCREMENT EQUAL TO 1                                  00000210
C                                                                       00000220
C                                                                       00000230
C        CLEAN-UP LOOP                                                  00000240
C                                                                       00000250
   20 M = MOD(N,5)                                                      00000260
      IF( M .EQ. 0 ) GO TO 40                                           00000270
      DO 30 I = 1,M                                                     00000280
        SX(I) = SA*SX(I)                                                00000290
   30 CONTINUE                                                          00000300
      IF( N .LT. 5 ) RETURN                                             00000310
   40 MP1 = M + 1                                                       00000320
      DO 50 I = MP1,N,5                                                 00000330
        SX(I) = SA*SX(I)                                                00000340
        SX(I + 1) = SA*SX(I + 1)                                        00000350
        SX(I + 2) = SA*SX(I + 2)                                        00000360
        SX(I + 3) = SA*SX(I + 3)                                        00000370
        SX(I + 4) = SA*SX(I + 4)                                        00000380
   50 CONTINUE                                                          00000390
      RETURN                                                            00000400
      END                                                               00000410
      SUBROUTINE  SSWAP (N,SX,INCX,SY,INCY)                             00000010
C                                                                       00000020
C     INTERCHANGES TWO VECTORS.                                         00000030
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO 1.                    00000040
C     JACK DONGARRA, LINPACK, 3/11/78.                                  00000050
C                                                                       00000060
      REAL SX(1),SY(1),STEMP                                            00000070
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N                                 00000080
C                                                                       00000090
      IF(N.LE.0)RETURN                                                  00000100
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20                               00000110
C                                                                       00000120
C       CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS NOT EQUAL       00000130
C         TO 1                                                          00000140
C                                                                       00000150
      IX = 1                                                            00000160
      IY = 1                                                            00000170
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1                                 00000180
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1                                 00000190
      DO 10 I = 1,N                                                     00000200
        STEMP = SX(IX)                                                  00000210
        SX(IX) = SY(IY)                                                 00000220
        SY(IY) = STEMP                                                  00000230
        IX = IX + INCX                                                  00000240
        IY = IY + INCY                                                  00000250
   10 CONTINUE                                                          00000260
      RETURN                                                            00000270
C                                                                       00000280
C       CODE FOR BOTH INCREMENTS EQUAL TO 1                             00000290
C                                                                       00000300
C                                                                       00000310
C       CLEAN-UP LOOP                                                   00000320
C                                                                       00000330
   20 M = MOD(N,3)                                                      00000340
      IF( M .EQ. 0 ) GO TO 40                                           00000350
      DO 30 I = 1,M                                                     00000360
        STEMP = SX(I)                                                   00000370
        SX(I) = SY(I)                                                   00000380
        SY(I) = STEMP                                                   00000390
   30 CONTINUE                                                          00000400
      IF( N .LT. 3 ) RETURN                                             00000410
   40 MP1 = M + 1                                                       00000420
      DO 50 I = MP1,N,3                                                 00000430
        STEMP = SX(I)                                                   00000440
        SX(I) = SY(I)                                                   00000450
        SY(I) = STEMP                                                   00000460
        STEMP = SX(I + 1)                                               00000470
        SX(I + 1) = SY(I + 1)                                           00000480
        SY(I + 1) = STEMP                                               00000490
        STEMP = SX(I + 2)                                               00000500
        SX(I + 2) = SY(I + 2)                                           00000510
        SY(I + 2) = STEMP                                               00000520
   50 CONTINUE                                                          00000530
      RETURN                                                            00000540
      END                                                               00000550
