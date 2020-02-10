c package from linpack complex, moddified to double precision complex by
c         M.A.DELSUC  10-9-86
c
c
c      <<<<<<<<<<<<<<<<<<<<<< NOT COMPLETE >>>>>>>>>>>>>>>>>>>
c
c
c
      SUBROUTINE CDGECO(A,LDA,N,IPVT,RCOND,Z) 
      INTEGER LDA,N,IPVT(1) 
      COMPLEX*16 A(LDA,1),Z(1) 
      REAL*8 RCOND 
C 
C     CDGECO FACTORS A COMPLEX*16 MATRIX BY GAUSSIAN ELIMINATION
C     AND ESTIMATES THE CONDITION OF THE MATRIX. 
C 
C     IF  RCOND  IS NOT NEEDED, CDGEFA IS SLIGHTLY FASTER. 
C     TO SOLVE  A*X = B , FOLLOW CDGECO BY CDGESL. 
C     TO COMPUTE  INVERSE(A)*C , FOLLOW CDGECO BY CDGESL. 
C     TO COMPUTE  DETERMINANT(A) , FOLLOW CDGECO BY CGEDI. 
C     TO COMPUTE  INVERSE(A) , FOLLOW CDGECO BY CGEDI. 
C 
C     ON ENTRY 
C 
C        A       COMPLEX*16(LDA, N) 
C                THE MATRIX TO BE FACTORED. 
C 
C        LDA     INTEGER 
C                THE LEADING DIMENSION OF THE ARRAY  A . 
C 
C        N       INTEGER 
C                THE ORDER OF THE MATRIX  A . 
C 
C     ON RETURN 
C 
C        A       AN UPPER TRIANGULAR MATRIX AND THE MULTIPLIERS 
C                WHICH WERE USED TO OBTAIN IT. 
C                THE FACTORIZATION CAN BE WRITTEN  A = L*U  WHERE 
C                L  IS A PRODUCT OF PERMUTATION AND UNIT LOWER 
C                TRIANGULAR MATRICES AND  U  IS UPPER TRIANGULAR. 
C 
C        IPVT    INTEGER(N) 
C                AN INTEGER VECTOR OF PIVOT INDICES. 
C 
C        RCOND   REAL*8 
C                AN ESTIMATE OF THE RECIPROCAL CONDITION OF  A . 
C                FOR THE SYSTEM  A*X = B , RELATIVE PERTURBATIONS 
C                IN  A  AND  B  OF SIZE  EPSILON  MAY CAUSE 
C                RELATIVE PERTURBATIONS IN  X  OF SIZE  EPSILON/RCOND .
C                IF  RCOND  IS SO SMALL THAT THE LOGICAL EXPRESSION 
C                           1.0D0 + RCOND .EQ. 1.0D0 
C                IS TRUE, THEN  A  MAY BE SINGULAR TO WORKING 
C                PRECISION.  IN PARTICULAR,  RCOND  IS ZERO  IF 
C                EXACT SINGULARITY IS DETECTED OR THE ESTIMATE 
C                UNDERFLOWS. 
C 
C        Z       COMPLEX*16(N) 
C                A WORK VECTOR WHOSE CONTENTS ARE USUALLY UNIMPORTANT. 
C                IF  A  IS CLOSE TO A SINGULAR MATRIX, THEN  Z  IS 
C                AN APPROXIMATE NULL VECTOR IN THE SENSE THAT 
C                NORM(A*Z) = RCOND*NORM(A)*NORM(Z) . 
C 
C     LINPACK. THIS VERSION DATE 08/14/78. 
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB. 
C 
C     SUBROUTINES AND FUNCTIONS 
C 
C     LINPACK CDGEFA 
C     BLAS CDAXPY,CDDOTC,CDSSCAL,DCASUM 
C     FORTRAN DABS,DIMAG,DMAX1,DCMPLX,DCONJG,DREAL 
C 
C     INTERNAL VARIABLES   
C 
      COMPLEX*16 CDDOTC,EK,T,WK,WKM 
      REAL*8  ANORM,S,DCASUM,SM,YNORM 
      INTEGER INFO,J,K,KB,KP1,L 
C 
      COMPLEX*16 ZDUM,ZDUM1,ZDUM2,CDSIGN1 
      REAL*8  CDABS1 

#_f2c      CDABS1(ZDUM) = DABS(DREAL(ZDUM)) + DABS(DIMAG(ZDUM)) 
#f2c      CDABS1(ZDUM) = DABS(DBLE(ZDUM)) + DABS(DIMAG(ZDUM)) 
      CDSIGN1(ZDUM1,ZDUM2) = CDABS1(ZDUM1)*(ZDUM2/CDABS1(ZDUM2)) 
C 
C     COMPUTE 1-NORM OF A 
C 
      ANORM = 0.0D0
      DO 10 J = 1, N 
         ANORM = DMAX1(ANORM,DCASUM(N,A(1,J),1)) 
   10 CONTINUE 
C 
C     FACTOR 
C 
      CALL CDGEFA(A,LDA,N,IPVT,INFO) 
C 
C     RCOND = 1/(NORM(A)*(ESTIMATE OF NORM(INVERSE(A)))) . 
C     ESTIMATE = NORM(Z)/NORM(Y) WHERE  A*Z = Y  AND  CTRANS(A)*Y = E 00000870
C     CTRANS(A)  IS THE CONJUGATE TRANSPOSE OF A .                    
C     THE COMPONENTS OF  E  ARE CHOSEN TO CAUSE MAXIMUM LOCAL 
C     GROWTH IN THE ELEMENTS OF W  WHERE  CTRANS(U)*W = E . 
C     THE VECTORS ARE FREQUENTLY RESCALED TO AVOID OVERFLOW. 
C 
C     SOLVE CTRANS(U)*W = E 
C 
      EK = (1.0D0,0.0D0) 
      DO 20 J = 1, N 
         Z(J) = (0.0D0,0.0D0) 
   20 CONTINUE     
      DO 100 K = 1, N 
         IF (CDABS1(Z(K)) .NE. 0.0D0) EK = CDSIGN1(EK,-Z(K)) 
         IF (CDABS1(EK-Z(K)) .LE. CDABS1(A(K,K))) GO TO 30 
            S = CDABS1(A(K,K))/CDABS1(EK-Z(K)) 
            CALL CDSSCAL(N,S,Z,1) 
            EK = DCMPLX(S,0.0D0)*EK 
   30    CONTINUE 
         WK = EK - Z(K) 
         WKM = -EK - Z(K) 
         S = CDABS1(WK) 
         SM = CDABS1(WKM) 
         IF (CDABS1(A(K,K)) .EQ. 0.0D0) GO TO 40 
            WK = WK/DCONJG(A(K,K)) 
            WKM = WKM/DCONJG(A(K,K)) 
         GO TO 50 
   40    CONTINUE 
            WK = (1.0D0,0.0D0) 
            WKM = (1.0D0,0.0D0) 
   50    CONTINUE   
         KP1 = K + 1 
         IF (KP1 .GT. N) GO TO 90 
            DO 60 J = KP1, N 
               SM = SM + CDABS1(Z(J)+WKM*DCONJG(A(K,J))) 
               Z(J) = Z(J) + WK*DCONJG(A(K,J)) 
               S = S + CDABS1(Z(J)) 
   60       CONTINUE 
            IF (S .GE. SM) GO TO 80 
               T = WKM - WK 
               WK = WKM 
               DO 70 J = KP1, N 
                  Z(J) = Z(J) + T*DCONJG(A(K,J)) 
   70          CONTINUE 
   80       CONTINUE 
   90    CONTINUE 
         Z(K) = WK 
  100 CONTINUE 
      S = 1.0D0/DCASUM(N,Z,1) 
      CALL CDSSCAL(N,S,Z,1) 
C 
C     SOLVE CTRANS(L)*Y = W 
C 
      DO 120 KB = 1, N 
         K = N + 1 - KB 
         IF (K .LT. N) Z(K) = Z(K) + CDDOTC(N-K,A(K+1,K),1,Z(K+1),1) 
         IF (CDABS1(Z(K)) .LE. 1.0D0) GO TO 110 
            S = 1.0D0/CDABS1(Z(K)) 
            CALL CDSSCAL(N,S,Z,1) 
  110    CONTINUE 
         L = IPVT(K) 
         T = Z(L) 
         Z(L) = Z(K) 
         Z(K) = T 
  120 CONTINUE 
      S = 1.0D0/DCASUM(N,Z,1) 
      CALL CDSSCAL(N,S,Z,1) 
C 
      YNORM = 1.0D0 
C 
C     SOLVE L*V = Y 
C 
      DO 140 K = 1, N 
         L = IPVT(K) 
         T = Z(L) 
         Z(L) = Z(K) 
         Z(K) = T 
         IF (K .LT. N) CALL CDAXPY(N-K,T,A(K+1,K),1,Z(K+1),1) 
         IF (CDABS1(Z(K)) .LE. 1.0D0) GO TO 130 
            S = 1.0D0/CDABS1(Z(K)) 
            CALL CDSSCAL(N,S,Z,1) 
            YNORM = S*YNORM 
  130    CONTINUE 
  140 CONTINUE 
      S = 1.0D0/DCASUM(N,Z,1) 
      CALL CDSSCAL(N,S,Z,1) 
      YNORM = S*YNORM 
C 
C     SOLVE  U*Z = V 
C 
      DO 160 KB = 1, N 
         K = N + 1 - KB 
         IF (CDABS1(Z(K)) .LE. CDABS1(A(K,K))) GO TO 150 
            S = CDABS1(A(K,K))/CDABS1(Z(K)) 
            CALL CDSSCAL(N,S,Z,1) 
            YNORM = S*YNORM 
  150    CONTINUE 
         IF (CDABS1(A(K,K)) .NE. 0.0D0) Z(K) = Z(K)/A(K,K) 
         IF (CDABS1(A(K,K)) .EQ. 0.0D0) Z(K) = (1.0D0,0.0D0) 
         T = -Z(K) 
         CALL CDAXPY(K-1,T,A(1,K),1,Z(1),1) 
  160 CONTINUE 
C     MAKE ZNORM = 1.0D0 
      S = 1.0D0/DCASUM(N,Z,1) 
      CALL CDSSCAL(N,S,Z,1) 
      YNORM = S*YNORM 
C 
      IF (ANORM .NE. 0.0D0) RCOND = YNORM/ANORM 
      IF (ANORM .EQ. 0.0D0) RCOND = 0.0D0 
      RETURN             
      END 
cC M.A.D. modif for f2c
ccf2c      function cdabs1(Zdum)
ccf2c      real*8 cdabs1
ccf2c      complex*16 Zdum
ccf2c      real*8 zr,zi
ccf2c      zr = dreal(zdum)
cccf2c      zi = dimag(zdum)
ccf2c      CDABS1 = DABS(zr) + DABS(zi) 
ccf2c      return
ccf2c      end

