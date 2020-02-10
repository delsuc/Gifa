c package from linpack complex, moddified to double precision complex by
c         M.A.DELSUC  10-9-86
c
c
c      <<<<<<<<<<<<<<<<<<<<<< NOT COMPLETE >>>>>>>>>>>>>>>>>>>
c
c
c
      SUBROUTINE CDSVDC(X,LDX,N,P,S,E,U,LDU,V,LDV,WORK,JOB,INFO) 
      IMPLICIT NONE
      INTEGER LDX,N,P,LDU,LDV,JOB,INFO                                  
      COMPLEX*16 X(LDX,1),S(1),E(1),U(LDU,1),V(LDV,1),WORK(1)              
C 
C 
C     CSVDC IS A SUBROUTINE TO REDUCE A COMPLEX*16 NXP MATRIX X BY 
C     UNITARY TRANSFORMATIONS U AND V TO DIAGONAL FORM.  THE 
C     DIAGONAL ELEMENTS S(I) ARE THE SINGULAR VALUES OF X.  THE 
C     COLUMNS OF U ARE THE CORRESPONDING LEFT SINGULAR VECTORS, 
C     AND THE COLUMNS OF V THE RIGHT SINGULAR VECTORS. 
C 
C     ON ENTRY 
C 
C         X         COMPLEX*16(LDX,P), WHERE LDX.GE.N. 
C                   X CONTAINS THE MATRIX WHOSE SINGULAR VALUE 
C                   DECOMPOSITION IS TO BE COMPUTED.  X IS 
C                   DESTROYED BY CSVDC. 
C 
C         LDX       INTEGER. 
C                   LDX IS THE LEADING DIMENSION OF THE ARRAY X. 
C 
C         N         INTEGER. 
C                   N IS THE NUMBER OF COLUMNS OF THE MATRIX X. 
C 
C         P         INTEGER. 
C                   P IS THE NUMBER OF ROWS OF THE MATRIX X. 
C 
C         LDU       INTEGER. 
C                   LDU IS THE LEADING DIMENSION OF THE ARRAY U 
C                   (SEE BELOW). 
C 
C         LDV       INTEGER. 
C                   LDV IS THE LEADING DIMENSION OF THE ARRAY V 
C                   (SEE BELOW). 
C 
C         WORK      COMPLEX*16(N). 
C                   WORK IS A SCRATCH ARRAY. 
C 
C         JOB       INTEGER. 
C                   JOB CONTROLS THE COMPUTATION OF THE SINGULAR 
C                   VECTORS.  IT HAS THE DECIMAL EXPANSION AB 
C                   WITH THE FOLLOWING MEANING 
C 
C                        A.EQ.0    DO NOT COMPUTE THE LEFT SINGULAR 
C                                  VECTORS. 
C                        A.EQ.1    RETURN THE N LEFT SINGULAR VECTORS 
C                                  IN U. 
C                        A.GE.2    RETURNS THE FIRST MIN(N,P) 
C                                  LEFT SINGULAR VECTORS IN U. 
C                        B.EQ.0    DO NOT COMPUTE THE RIGHT SINGULAR 
C                                  VECTORS. 
C                        B.EQ.1    RETURN THE RIGHT SINGULAR VECTORS 
C                                  IN V. 
C 
C     ON RETURN 
C 
C         S         COMPLEX*16(MM), WHERE MM=MIN(N+1,P). 
C                   THE FIRST MIN(N,P) ENTRIES OF S CONTAIN THE 
C                   SINGULAR VALUES OF X ARRANGED IN DESCENDING 
C                   ORDER OF MAGNITUDE. 
C 
C         E         COMPLEX*16(P). 
C                   E ORDINARILY CONTAINS ZEROS.  HOWEVER SEE THE 
C                   DISCUSSION OF INFO FOR EXCEPTIONS. 
C 
C         U         COMPLEX*16(LDU,K), WHERE LDU.GE.N.  IF JOBA.EQ.1 THEN 
C                                   K.EQ.N, IF JOBA.GE.2 THEN 
C                                   K.EQ.MIN(N,P). 
C                   U CONTAINS THE MATRIX OF RIGHT SINGULAR VECTORS. 
C                   U IS NOT REFERENCED IF JOBA.EQ.0.  IF N.LE.P 
C                   OR IF JOBA.GT.2, THEN U MAY BE IDENTIFIED WITH X 
C                   IN THE SUBROUTINE CALL. 
C 
C         V         COMPLEX*16(LDV,P), WHERE LDV.GE.P. 
C                   V CONTAINS THE MATRIX OF RIGHT SINGULAR VECTORS. 
C                   V IS NOT REFERENCED IF JOBB.EQ.0.  IF P.LE.N, 
C                   THEN V MAY BE IDENTIFIED WHTH X IN THE 
C                   SUBROUTINE CALL. 
C 
C         INFO      INTEGER. 
C                   THE SINGULAR VALUES (AND THEIR CORRESPONDING 
C                   SINGULAR VECTORS) S(INFO+1),S(INFO+2),...,S(M) 
C                   ARE CORRECT (HERE M=MIN(N,P)).  THUS IF 
C                   INFO.EQ.0, ALL THE SINGULAR VALUES AND THEIR 
C                   VECTORS ARE CORRECT.  IN ANY EVENT, THE MATRIX 
C                   B = CTRANS(U)*X*V IS THE BIDIAGONAL MATRIX 
C                   WITH THE ELEMENTS OF S ON ITS DIAGONAL AND THE 
C                   ELEMENTS OF E ON ITS SUPER-DIAGONAL (CTRANS(U) 
C                   IS THE CONJUGATE-TRANSPOSE OF U).  THUS THE 
C                   SINGULAR VALUES OF X AND B ARE THE SAME. 
C 
C     LINPACK. THIS VERSION DATED 03/19/79 . 
C     G.W. STEWART, UNIVERSITY OF MARYLAND, ARGONNE NATIONAL LAB. 
C 
C     CDSVDC USES THE FOLLOWING FUNCTIONS AND SUBPROGRAMS. 
C 
C     EXTERNAL CDROT 
C     BLAS CDAXPY,CDDOTC,CDSCAL,CDSWAP,CDNRM2,DROTG 
C     FORTRAN DABS,DIMAG,DMAX1,CDABS,DCMPLX 
C     FORTRAN DCONJG,MAX0,MIN0,MOD,DREAL,DSQRT 
C 
C     INTERNAL VARIABLES 
C 
      INTEGER I,ITER,J,JOBU,K,KASE,KK,L,LL,LLS,LM1,LP1,LS,LU,
     *	M,MAXIT,MM,MM1,MP1,NCT,NCTP1,NCU,NRT,NRTP1 
      COMPLEX*16 CDDOTC,T,R 
      REAL*8 B,C,CS,EL,EMM1,F,G,CDNRM2,SCALE,SHIFT,SL,SM,SN,SMM1,
     *	T1,TEST,ZTEST 
      LOGICAL WANTU,WANTV 
C 
      COMPLEX*16 CDSIGN,ZDUM,ZDUM1,ZDUM2 
      REAL*8 CDABS1 
#_f2c      CDABS1(ZDUM) = DABS(DREAL(ZDUM)) + DABS(DIMAG(ZDUM))
#f2c      CDABS1(ZDUM) = DABS(DBLE(ZDUM)) + DABS(DIMAG(ZDUM))
#f2c      real*8 dreal
#f2c      dreal(zdum) = dble(zdum)
#_f2c      CDSIGN(ZDUM1,ZDUM2) = CDABS(ZDUM1)*(ZDUM2/CDABS(ZDUM2))
#f2c      CDSIGN(ZDUM1,ZDUM2) = ZABS(ZDUM1)*(ZDUM2/ZABS(ZDUM2))


C 
C     SET THE MAXIMUM NUMBER OF ITERATIONS. 
C 
      MAXIT = 30 
C 
C     DETERMINE WHAT IS TO BE COMPUTED. 
C 
      WANTU = .FALSE. 
      WANTV = .FALSE. 
      JOBU = MOD(JOB,100)/10 
      NCU = N 
      IF (JOBU .GT. 1) NCU = MIN0(N,P) 
      IF (JOBU .NE. 0) WANTU = .TRUE. 
      IF (MOD(JOB,10) .NE. 0) WANTV = .TRUE. 
C 
C     REDUCE X TO BIDIAGONAL FORM, STORING THE DIAGONAL ELEMENTS 
C     IN S AND THE SUPER-DIAGONAL ELEMENTS IN E. 
C 
      INFO = 0 
      NCT = MIN0(N-1,P) 
      NRT = MAX0(0,MIN0(P-2,N)) 
      LU = MAX0(NCT,NRT) 
      IF (LU .LT. 1) GO TO 170 
      DO 160 L = 1, LU 
         LP1 = L + 1 
         IF (L .GT. NCT) GO TO 20 
C 
C           COMPUTE THE TRANSFORMATION FOR THE L-TH COLUMN AND 
C           PLACE THE L-TH DIAGONAL IN S(L). 
C 
            S(L) = DCMPLX(CDNRM2(N-L+1,X(L,L),1),0.0D0) 
            IF (CDABS1(S(L)) .EQ. 0.0D0) GO TO 10 
               IF (CDABS1(X(L,L)) .NE. 0.0D0) S(L) = CDSIGN(S(L),X(L,L)) 
               CALL CDSCAL(N-L+1,1.0D0/S(L),X(L,L),1) 
               X(L,L) = (1.0D0,0.0D0) + X(L,L) 
   10       CONTINUE 
            S(L) = -S(L) 
   20    CONTINUE 
         IF (P .LT. LP1) GO TO 50 
         DO 40 J = LP1, P 
            IF (L .GT. NCT) GO TO 30 
            IF (CDABS1(S(L)) .EQ. 0.0D0) GO TO 30 
C 
C              APPLY THE TRANSFORMATION. 
C 
               T = -CDDOTC(N-L+1,X(L,L),1,X(L,J),1)/X(L,L) 
               CALL CDAXPY(N-L+1,T,X(L,L),1,X(L,J),1) 
   30       CONTINUE 
C 
C           PLACE THE L-TH ROW OF X INTO  E FOR THE 
C           SUBSEQUENT CALCULATION OF THE ROW TRANSFORMATION. 
C 
            E(J) = DCONJG(X(L,J)) 
   40    CONTINUE 
   50    CONTINUE 
         IF (.NOT.WANTU .OR. L .GT. NCT) GO TO 70 
C 
C           PLACE THE TRANSFORMATION IN U FOR SUBSEQUENT BACK 
C           MULTIPLICATION. 
C 
            DO 60 I = L, N 
               U(I,L) = X(I,L) 
   60       CONTINUE 
   70    CONTINUE 
         IF (L .GT. NRT) GO TO 150 
C 
C           COMPUTE THE L-TH ROW TRANSFORMATION AND PLACE THE 
C           L-TH SUPER-DIAGONAL IN E(L). 
C 
            E(L) = DCMPLX(CDNRM2(P-L,E(LP1),1),0.0D0) 
            IF (CDABS1(E(L)) .EQ. 0.0D0) GO TO 80 
               IF (CDABS1(E(LP1)) .NE. 0.0D0) E(L) = CDSIGN(E(L),E(LP1)) 
               CALL CDSCAL(P-L,1.0D0/E(L),E(LP1),1) 
               E(LP1) = (1.0D0,0.0D0) + E(LP1) 
   80       CONTINUE 
            E(L) = -DCONJG(E(L)) 
            IF (LP1 .GT. N .OR. CDABS1(E(L)) .EQ. 0.0D0) GO TO 120 
C 
C              APPLY THE TRANSFORMATION. 
C 
               DO 90 I = LP1, N 
                  WORK(I) = (0.0D0,0.0D0)              
   90          CONTINUE 
               DO 100 J = LP1, P 
                  CALL CDAXPY(N-L,E(J),X(LP1,J),1,WORK(LP1),1) 
  100          CONTINUE 
               DO 110 J = LP1, P 
                  CALL CDAXPY(N-L,DCONJG(-E(J)/E(LP1)),WORK(LP1),1, 
     *                       X(LP1,J),1) 
  110          CONTINUE 
  120       CONTINUE 
            IF (.NOT.WANTV) GO TO 140 
C 
C              PLACE THE TRANSFORMATION IN V FOR SUBSEQUENT 
C              BACK MULTIPLICATION. 
C 
               DO 130 I = LP1, P 
                  V(I,L) = E(I) 
  130          CONTINUE 
  140       CONTINUE 
  150    CONTINUE 
  160 CONTINUE 
  170 CONTINUE 
C 
C     SET UP THE FINAL BIDIAGONAL MATRIX OR ORDER M. 
C 
      M = MIN0(P,N+1) 
      NCTP1 = NCT + 1 
      NRTP1 = NRT + 1 
      IF (NCT .LT. P) S(NCTP1) = X(NCTP1,NCTP1) 
      IF (N .LT. M) S(M) = (0.0D0,0.0D0) 
      IF (NRTP1 .LT. M) E(NRTP1) = X(NRTP1,M) 
      E(M) = (0.0D0,0.0D0) 
C 
C     IF REQUIRED, GENERATE U. 
C 
      IF (.NOT.WANTU) GO TO 300 
         IF (NCU .LT. NCTP1) GO TO 200 
         DO 190 J = NCTP1, NCU 
            DO 180 I = 1, N 
               U(I,J) = (0.0D0,0.0D0) 
  180       CONTINUE 
            U(J,J) = (1.0D0,0.0D0) 
  190    CONTINUE 
  200    CONTINUE 
         IF (NCT .LT. 1) GO TO 290 
         DO 280 LL = 1, NCT 
            L = NCT - LL + 1 
            IF (CDABS1(S(L)) .EQ. 0.0D0) GO TO 250 
               LP1 = L + 1 
               IF (NCU .LT. LP1) GO TO 220 
               DO 210 J = LP1, NCU 
                  T = -CDDOTC(N-L+1,U(L,L),1,U(L,J),1)/U(L,L) 
                  CALL CDAXPY(N-L+1,T,U(L,L),1,U(L,J),1) 
  210          CONTINUE 
  220          CONTINUE 
               CALL CDSCAL(N-L+1,(-1.0D0,0.0D0),U(L,L),1) 
               U(L,L) = (1.0D0,0.0D0) + U(L,L) 
               LM1 = L - 1 
               IF (LM1 .LT. 1) GO TO 240 
               DO 230 I = 1, LM1 
                  U(I,L) = (0.0D0,0.0D0) 
  230          CONTINUE 
  240          CONTINUE 
            GO TO 270 
  250       CONTINUE 
               DO 260 I = 1, N 
                  U(I,L) = (0.0D0,0.0D0) 
  260          CONTINUE 
               U(L,L) = (1.0D0,0.0D0) 
  270       CONTINUE 
  280    CONTINUE 
  290    CONTINUE 
  300 CONTINUE 
C 
C     IF IT IS REQUIRED, GENERATE V. 
C 
      IF (.NOT.WANTV) GO TO 350 
         DO 340 LL = 1, P 
            L = P - LL + 1 
            LP1 = L + 1 
            IF (L .GT. NRT) GO TO 320 
            IF (CDABS1(E(L)) .EQ. 0.0D0) GO TO 320 
               DO 310 J = LP1, P 
                  T = -CDDOTC(P-L,V(LP1,L),1,V(LP1,J),1)/V(LP1,L) 
                  CALL CDAXPY(P-L,T,V(LP1,L),1,V(LP1,J),1) 
  310          CONTINUE 
  320       CONTINUE 
            DO 330 I = 1, P 
               V(I,L) = (0.0D0,0.0D0) 
  330       CONTINUE 
            V(L,L) = (1.0D0,0.0D0) 
  340    CONTINUE 
  350 CONTINUE 
C 
C     TRANSFORM S AND E SO THAT THEY ARE REAL. 
C 
      DO 380 I = 1, M 
         IF (CDABS1(S(I)) .EQ. 0.0D0) GO TO 360 
#_f2c            T = DCMPLX(CDABS(S(I)),0.0D0) 
#f2c            T = DCMPLX(ZABS(S(I)),0.0D0) 
            R = S(I)/T 
            S(I) = T 
            IF (I .LT. M) E(I) = E(I)/R 
            IF (WANTU) CALL CDSCAL(N,R,U(1,I),1) 
  360    CONTINUE 
C     ...EXIT 
         IF (I .EQ. M) GO TO 390 
         IF (CDABS1(E(I)) .EQ. 0.0D0) GO TO 370 
#_f2c            T = DCMPLX(CDABS(E(I)),0.0D0) 
#f2c            T = DCMPLX(ZABS(E(I)),0.0D0) 
            R = T/E(I) 
            E(I) = T 
            S(I+1) = S(I+1)*R 
            IF (WANTV) CALL CDSCAL(P,R,V(1,I+1),1) 
  370    CONTINUE 
  380 CONTINUE 
  390 CONTINUE 
C 
C     MAIN ITERATION LOOP FOR THE SINGULAR VALUES. 
C 
      MM = M 
      ITER = 0 
  400 CONTINUE 
C 
C        QUIT IF ALL THE SINGULAR VALUES HAVE BEEN FOUND. 
C 
C     ...EXIT 
         IF (M .EQ. 0) GO TO 660 
C 
C        IF TOO MANY ITERATIONS HAVE BEEN PERFORMED, SET 
C        FLAG AND RETURN. 
C 
         IF (ITER .LT. MAXIT) GO TO 410 
            INFO = M 
C     ......EXIT 
            GO TO 660 
  410    CONTINUE 
C 
C        THIS SECTION OF THE PROGRAM INSPECTS FOR 
C        NEGLIGIBLE ELEMENTS IN THE S AND E ARRAYS.  ON 
C        COMPLETION THE VARIABLES KASE AND L ARE SET AS FOLLOWS. 
C 
C           KASE = 1     IF S(M) AND E(L-1) ARE NEGLIGIBLE AND L.LT.M 
C           KASE = 2     IF S(L) IS NEGLIGIBLE AND L.LT.M 
C           KASE = 3     IF E(L-1) IS NEGLIGIBLE, L.LT.M, AND 
C                        S(L), ..., S(M) ARE NOT NEGLIGIBLE (QR STEP). 
C           KASE = 4     IF E(M-1) IS NEGLIGIBLE (CONVERGENCE). 
C 
         DO 430 LL = 1, M 
            L = M - LL 
C        ...EXIT 
            IF (L .EQ. 0) GO TO 440 
#_f2c            TEST = CDABS(S(L)) + CDABS(S(L+1)) 
#_f2c            ZTEST = TEST + CDABS(E(L)) 
#f2c            TEST = ZABS(S(L)) + ZABS(S(L+1)) 
#f2c            ZTEST = TEST + ZABS(E(L)) 
            IF (ZTEST .NE. TEST) GO TO 420 
               E(L) = (0.0D0,0.0D0) 
C        ......EXIT 
               GO TO 440 
  420       CONTINUE 
  430    CONTINUE 
  440    CONTINUE 
         IF (L .NE. M - 1) GO TO 450 
            KASE = 4 
         GO TO 520 
  450    CONTINUE 
            LP1 = L + 1 
            MP1 = M + 1 
            DO 470 LLS = LP1, MP1 
               LS = M - LLS + LP1 
C           ...EXIT 
               IF (LS .EQ. L) GO TO 480 
               TEST = 0.0D0 
#f2c               IF (LS .NE. M) TEST = TEST + ZABS(E(LS)) 
#f2c               IF (LS .NE. L + 1) TEST = TEST + ZABS(E(LS-1)) 
#f2c               ZTEST = TEST + ZABS(S(LS)) 
#_f2c               IF (LS .NE. M) TEST = TEST + CDABS(E(LS)) 
#_f2c               IF (LS .NE. L + 1) TEST = TEST + CDABS(E(LS-1)) 
#_f2c               ZTEST = TEST + CDABS(S(LS)) 
               IF (ZTEST .NE. TEST) GO TO 460 
                  S(LS) = (0.0D0,0.0D0) 
C           ......EXIT 
                  GO TO 480 
  460          CONTINUE 
  470       CONTINUE 
  480       CONTINUE 
            IF (LS .NE. L) GO TO 490 
               KASE = 3 
            GO TO 510 
  490       CONTINUE 
            IF (LS .NE. M) GO TO 500 
               KASE = 1 
            GO TO 510 
  500       CONTINUE 
               KASE = 2 
               L = LS 
  510       CONTINUE 
  520    CONTINUE 
         L = L + 1 
C 
C        PERFORM THE TASK INDICATED BY KASE. 
C 
         GO TO (530, 560, 580, 610), KASE 
C 
C        DEFLATE NEGLIGIBLE S(M). 
C 
  530    CONTINUE 
            MM1 = M - 1 
            F = DREAL(E(M-1)) 
            E(M-1) = (0.0D0,0.0D0) 
            DO 550 KK = L, MM1 
               K = MM1 - KK + L 
               T1 = DREAL(S(K)) 
               CALL DROTG(T1,F,CS,SN) 
               S(K) = DCMPLX(T1,0.0D0) 
               IF (K .EQ. L) GO TO 540 
                  F = -SN*DREAL(E(K-1)) 
                  E(K-1) = CS*E(K-1) 
  540          CONTINUE 
               IF (WANTV) CALL CDROT(P,V(1,K),1,V(1,M),1,CS,SN) 
  550       CONTINUE 
         GO TO 650 
C 
C        SPLIT AT NEGLIGIBLE S(L). 
C 
  560    CONTINUE 
            F = DREAL(E(L-1)) 
            E(L-1) = (0.0D0,0.0D0) 
            DO 570 K = L, M 
               T1 = DREAL(S(K)) 
               CALL DROTG(T1,F,CS,SN) 
               S(K) = DCMPLX(T1,0.0D0) 
               F = -SN*DREAL(E(K)) 
               E(K) = CS*E(K) 
               IF (WANTU) CALL CDROT(N,U(1,K),1,U(1,L-1),1,CS,SN) 
  570       CONTINUE 
         GO TO 650 
C 
C        PERFORM ONE QR STEP. 
C 
  580    CONTINUE 
C 
C           CALCULATE THE SHIFT. 
C 
#f2c            SCALE = DMAX1(ZABS(S(M)),ZABS(S(M-1)),ZABS(E(M-1)), 
#f2c     *                    ZABS(S(L)),ZABS(E(L))) 
#_f2c            SCALE = DMAX1(CDABS(S(M)),CDABS(S(M-1)),CDABS(E(M-1)), 
#_f2c     *                    CDABS(S(L)),CDABS(E(L))) 
            SM = DREAL(S(M))/SCALE 
            SMM1 = DREAL(S(M-1))/SCALE 
            EMM1 = DREAL(E(M-1))/SCALE 
            SL = DREAL(S(L))/SCALE 
            EL = DREAL(E(L))/SCALE 
            B = ((SMM1 + SM)*(SMM1 - SM) + EMM1**2)/2.0D0 
            C = (SM*EMM1)**2 
            SHIFT = 0.0D0 
            IF (B .EQ. 0.0D0 .AND. C .EQ. 0.0D0) GO TO 590 
               SHIFT = DSQRT(B**2+C) 
               IF (B .LT. 0.0D0) SHIFT = -SHIFT 
               SHIFT = C/(B + SHIFT) 
  590       CONTINUE 
            F = (SL + SM)*(SL - SM) - SHIFT 
            G = SL*EL 
C 
C           CHASE ZEROS. 
C 
            MM1 = M - 1 
            DO 600 K = L, MM1 
               CALL DROTG(F,G,CS,SN) 
               IF (K .NE. L) E(K-1) = DCMPLX(F,0.0D0) 
               F = CS*DREAL(S(K)) + SN*DREAL(E(K)) 
               E(K) = CS*E(K) - SN*S(K) 
               G = SN*DREAL(S(K+1)) 
               S(K+1) = CS*S(K+1) 
               IF (WANTV) CALL CDROT(P,V(1,K),1,V(1,K+1),1,CS,SN) 
               CALL DROTG(F,G,CS,SN) 
               S(K) = DCMPLX(F,0.0D0) 
               F = CS*DREAL(E(K)) + SN*DREAL(S(K+1)) 
               S(K+1) = -SN*E(K) + CS*S(K+1) 
               G = SN*DREAL(E(K+1)) 
               E(K+1) = CS*E(K+1) 
               IF (WANTU .AND. K .LT. N) 
     *            CALL CDROT(N,U(1,K),1,U(1,K+1),1,CS,SN) 
  600       CONTINUE 
            E(M-1) = DCMPLX(F,0.0D0) 
            ITER = ITER + 1 
         GO TO 650 
C 
C        CONVERGENCE. 
C 
  610    CONTINUE 
C 
C           MAKE THE SINGULAR VALUE  POSITIVE 
C 
            IF (DREAL(S(L)) .GE. 0.0D0) GO TO 620 
               S(L) = -S(L) 
               IF (WANTV) CALL CDSCAL(P,(-1.0D0,0.0D0),V(1,L),1) 
  620       CONTINUE 
C 
C           ORDER THE SINGULAR VALUE. 
C 
  630       IF (L .EQ. MM) GO TO 640 
C           ...EXIT 
               IF (DREAL(S(L)) .GE. DREAL(S(L+1))) GO TO 640 
               T = S(L) 
               S(L) = S(L+1) 
               S(L+1) = T 
               IF (WANTV .AND. L .LT. P) 
     *            CALL CDSWAP(P,V(1,L),1,V(1,L+1),1) 
               IF (WANTU .AND. L .LT. N) 
     *            CALL CDSWAP(N,U(1,L),1,U(1,L+1),1) 
               L = L + 1 
            GO TO 630 
  640       CONTINUE 
            ITER = 0 
            M = M - 1 
  650    CONTINUE 
      GO TO 400 
  660 CONTINUE 
      RETURN 
      END 

