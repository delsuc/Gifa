      SUBROUTINE SGECO(A,LDA,N,IPVT,RCOND,Z)                            00000010
      INTEGER LDA,N,IPVT(1)                                             00000020
      REAL A(LDA,1),Z(1)                                                00000030
      REAL RCOND                                                        00000040
C                                                                       00000050
C     SGECO FACTORS A REAL MATRIX BY GAUSSIAN ELIMINATION               00000060
C     AND ESTIMATES THE CONDITION OF THE MATRIX.                        00000070
C                                                                       00000080
C     IF  RCOND  IS NOT NEEDED, SGEFA IS SLIGHTLY FASTER.               00000090
C     TO SOLVE  A*X = B , FOLLOW SGECO BY SGESL.                        00000100
C     TO COMPUTE  INVERSE(A)*C , FOLLOW SGECO BY SGESL.                 00000110
C     TO COMPUTE  DETERMINANT(A) , FOLLOW SGECO BY SGEDI.               00000120
C     TO COMPUTE  INVERSE(A) , FOLLOW SGECO BY SGEDI.                   00000130
C                                                                       00000140
C     ON ENTRY                                                          00000150
C                                                                       00000160
C        A       REAL(LDA, N)                                           00000170
C                THE MATRIX TO BE FACTORED.                             00000180
C                                                                       00000190
C        LDA     INTEGER                                                00000200
C                THE LEADING DIMENSION OF THE ARRAY  A .                00000210
C                                                                       00000220
C        N       INTEGER                                                00000230
C                THE ORDER OF THE MATRIX  A .                           00000240
C                                                                       00000250
C     ON RETURN                                                         00000260
C                                                                       00000270
C        A       AN UPPER TRIANGULAR MATRIX AND THE MULTIPLIERS         00000280
C                WHICH WERE USED TO OBTAIN IT.                          00000290
C                THE FACTORIZATION CAN BE WRITTEN  A = L*U  WHERE       00000300
C                L  IS A PRODUCT OF PERMUTATION AND UNIT LOWER          00000310
C                TRIANGULAR MATRICES AND  U  IS UPPER TRIANGULAR.       00000320
C                                                                       00000330
C        IPVT    INTEGER(N)                                             00000340
C                AN INTEGER VECTOR OF PIVOT INDICES.                    00000350
C                                                                       00000360
C        RCOND   REAL                                                   00000370
C                AN ESTIMATE OF THE RECIPROCAL CONDITION OF  A .        00000380
C                FOR THE SYSTEM  A*X = B , RELATIVE PERTURBATIONS       00000390
C                IN  A  AND  B  OF SIZE  EPSILON  MAY CAUSE             00000400
C                RELATIVE PERTURBATIONS IN  X  OF SIZE  EPSILON/RCOND . 00000410
C                IF  RCOND  IS SO SMALL THAT THE LOGICAL EXPRESSION     00000420
C                           1.0 + RCOND .EQ. 1.0                        00000430
C                IS TRUE, THEN  A  MAY BE SINGULAR TO WORKING           00000440
C                PRECISION.  IN PARTICULAR,  RCOND  IS ZERO  IF         00000450
C                EXACT SINGULARITY IS DETECTED OR THE ESTIMATE          00000460
C                UNDERFLOWS.                                            00000470
C                                                                       00000480
C        Z       REAL(N)                                                00000490
C                A WORK VECTOR WHOSE CONTENTS ARE USUALLY UNIMPORTANT.  00000500
C                IF  A  IS CLOSE TO A SINGULAR MATRIX, THEN  Z  IS      00000510
C                AN APPROXIMATE NULL VECTOR IN THE SENSE THAT           00000520
C                NORM(A*Z) = RCOND*NORM(A)*NORM(Z) .                    00000530
C                                                                       00000540
C     LINPACK. THIS VERSION DATED 08/14/78 .                            00000550
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.      00000560
C                                                                       00000570
C     SUBROUTINES AND FUNCTIONS                                         00000580
C                                                                       00000590
C     LINPACK SGEFA                                                     00000600
C     BLAS SAXPY,SDOT,SSCAL,SASUM                                       00000610
C     FORTRAN ABS,AMAX1,SIGN                                            00000620
C                                                                       00000630
C     INTERNAL VARIABLES                                                00000640
C                                                                       00000650
      REAL SDOT,EK,T,WK,WKM                                             00000660
      REAL ANORM,S,SASUM,SM,YNORM                                       00000670
      INTEGER INFO,J,K,KB,KP1,L                                         00000680
C                                                                       00000690
C                                                                       00000700
C     COMPUTE 1-NORM OF A                                               00000710
C                                                                       00000720
      ANORM = 0.0E0                                                     00000730
      DO 10 J = 1, N                                                    00000740
         ANORM = AMAX1(ANORM,SASUM(N,A(1,J),1))                         00000750
   10 CONTINUE                                                          00000760
C                                                                       00000770
C     FACTOR                                                            00000780
C                                                                       00000790
      CALL SGEFA(A,LDA,N,IPVT,INFO)                                     00000800
C                                                                       00000810
C     RCOND = 1/(NORM(A)*(ESTIMATE OF NORM(INVERSE(A)))) .              00000820
C     ESTIMATE = NORM(Z)/NORM(Y) WHERE  A*Z = Y  AND  TRANS(A)*Y = E .  00000830
C     TRANS(A)  IS THE TRANSPOSE OF A .  THE COMPONENTS OF  E  ARE      00000840
C     CHOSEN TO CAUSE MAXIMUM LOCAL GROWTH IN THE ELEMENTS OF W  WHERE  00000850
C     TRANS(U)*W = E .  THE VECTORS ARE FREQUENTLY RESCALED TO AVOID    00000860
C     OVERFLOW.                                                         00000870
C                                                                       00000880
C     SOLVE TRANS(U)*W = E                                              00000890
C                                                                       00000900
      EK = 1.0E0                                                        00000910
      DO 20 J = 1, N                                                    00000920
         Z(J) = 0.0E0                                                   00000930
   20 CONTINUE                                                          00000940
      DO 100 K = 1, N                                                   00000950
         IF (Z(K) .NE. 0.0E0) EK = SIGN(EK,-Z(K))                       00000960
         IF (ABS(EK-Z(K)) .LE. ABS(A(K,K))) GO TO 30                    00000970
            S = ABS(A(K,K))/ABS(EK-Z(K))                                00000980
            CALL SSCAL(N,S,Z,1)                                         00000990
            EK = S*EK                                                   00001000
   30    CONTINUE                                                       00001010
         WK = EK - Z(K)                                                 00001020
         WKM = -EK - Z(K)                                               00001030
         S = ABS(WK)                                                    00001040
         SM = ABS(WKM)                                                  00001050
         IF (A(K,K) .EQ. 0.0E0) GO TO 40                                00001060
            WK = WK/A(K,K)                                              00001070
            WKM = WKM/A(K,K)                                            00001080
         GO TO 50                                                       00001090
   40    CONTINUE                                                       00001100
            WK = 1.0E0                                                  00001110
            WKM = 1.0E0                                                 00001120
   50    CONTINUE                                                       00001130
         KP1 = K + 1                                                    00001140
         IF (KP1 .GT. N) GO TO 90                                       00001150
            DO 60 J = KP1, N                                            00001160
               SM = SM + ABS(Z(J)+WKM*A(K,J))                           00001170
               Z(J) = Z(J) + WK*A(K,J)                                  00001180
               S = S + ABS(Z(J))                                        00001190
   60       CONTINUE                                                    00001200
            IF (S .GE. SM) GO TO 80                                     00001210
               T = WKM - WK                                             00001220
               WK = WKM                                                 00001230
               DO 70 J = KP1, N                                         00001240
                  Z(J) = Z(J) + T*A(K,J)                                00001250
   70          CONTINUE                                                 00001260
   80       CONTINUE                                                    00001270
   90    CONTINUE                                                       00001280
         Z(K) = WK                                                      00001290
  100 CONTINUE                                                          00001300
      S = 1.0E0/SASUM(N,Z,1)                                            00001310
      CALL SSCAL(N,S,Z,1)                                               00001320
C                                                                       00001330
C     SOLVE TRANS(L)*Y = W                                              00001340
C                                                                       00001350
      DO 120 KB = 1, N                                                  00001360
         K = N + 1 - KB                                                 00001370
         IF (K .LT. N) Z(K) = Z(K) + SDOT(N-K,A(K+1,K),1,Z(K+1),1)      00001380
         IF (ABS(Z(K)) .LE. 1.0E0) GO TO 110                            00001390
            S = 1.0E0/ABS(Z(K))                                         00001400
            CALL SSCAL(N,S,Z,1)                                         00001410
  110    CONTINUE                                                       00001420
         L = IPVT(K)                                                    00001430
         T = Z(L)                                                       00001440
         Z(L) = Z(K)                                                    00001450
         Z(K) = T                                                       00001460
  120 CONTINUE                                                          00001470
      S = 1.0E0/SASUM(N,Z,1)                                            00001480
      CALL SSCAL(N,S,Z,1)                                               00001490
C                                                                       00001500
      YNORM = 1.0E0                                                     00001510
C                                                                       00001520
C     SOLVE L*V = Y                                                     00001530
C                                                                       00001540
      DO 140 K = 1, N                                                   00001550
         L = IPVT(K)                                                    00001560
         T = Z(L)                                                       00001570
         Z(L) = Z(K)                                                    00001580
         Z(K) = T                                                       00001590
         IF (K .LT. N) CALL SAXPY(N-K,T,A(K+1,K),1,Z(K+1),1)            00001600
         IF (ABS(Z(K)) .LE. 1.0E0) GO TO 130                            00001610
            S = 1.0E0/ABS(Z(K))                                         00001620
            CALL SSCAL(N,S,Z,1)                                         00001630
            YNORM = S*YNORM                                             00001640
  130    CONTINUE                                                       00001650
  140 CONTINUE                                                          00001660
      S = 1.0E0/SASUM(N,Z,1)                                            00001670
      CALL SSCAL(N,S,Z,1)                                               00001680
      YNORM = S*YNORM                                                   00001690
C                                                                       00001700
C     SOLVE  U*Z = V                                                    00001710
C                                                                       00001720
      DO 160 KB = 1, N                                                  00001730
         K = N + 1 - KB                                                 00001740
         IF (ABS(Z(K)) .LE. ABS(A(K,K))) GO TO 150                      00001750
            S = ABS(A(K,K))/ABS(Z(K))                                   00001760
            CALL SSCAL(N,S,Z,1)                                         00001770
            YNORM = S*YNORM                                             00001780
  150    CONTINUE                                                       00001790
         IF (A(K,K) .NE. 0.0E0) Z(K) = Z(K)/A(K,K)                      00001800
         IF (A(K,K) .EQ. 0.0E0) Z(K) = 1.0E0                            00001810
         T = -Z(K)                                                      00001820
         CALL SAXPY(K-1,T,A(1,K),1,Z(1),1)                              00001830
  160 CONTINUE                                                          00001840
C     MAKE ZNORM = 1.0                                                  00001850
      S = 1.0E0/SASUM(N,Z,1)                                            00001860
      CALL SSCAL(N,S,Z,1)                                               00001870
      YNORM = S*YNORM                                                   00001880
C                                                                       00001890
      IF (ANORM .NE. 0.0E0) RCOND = YNORM/ANORM                         00001900
      IF (ANORM .EQ. 0.0E0) RCOND = 0.0E0                               00001910
      RETURN                                                            00001920
      END                                                               00001930
      SUBROUTINE SGEFA(A,LDA,N,IPVT,INFO)                               00000010
      INTEGER LDA,N,IPVT(1),INFO                                        00000020
      REAL A(LDA,1)                                                     00000030
C                                                                       00000040
C     SGEFA FACTORS A REAL MATRIX BY GAUSSIAN ELIMINATION.              00000050
C                                                                       00000060
C     SGEFA IS USUALLY CALLED BY SGECO, BUT IT CAN BE CALLED            00000070
C     DIRECTLY WITH A SAVING IN TIME IF  RCOND  IS NOT NEEDED.          00000080
C     (TIME FOR SGECO) = (1 + 9/N)*(TIME FOR SGEFA) .                   00000090
C                                                                       00000100
C     ON ENTRY                                                          00000110
C                                                                       00000120
C        A       REAL(LDA, N)                                           00000130
C                THE MATRIX TO BE FACTORED.                             00000140
C                                                                       00000150
C        LDA     INTEGER                                                00000160
C                THE LEADING DIMENSION OF THE ARRAY  A .                00000170
C                                                                       00000180
C        N       INTEGER                                                00000190
C                THE ORDER OF THE MATRIX  A .                           00000200
C                                                                       00000210
C     ON RETURN                                                         00000220
C                                                                       00000230
C        A       AN UPPER TRIANGULAR MATRIX AND THE MULTIPLIERS         00000240
C                WHICH WERE USED TO OBTAIN IT.                          00000250
C                THE FACTORIZATION CAN BE WRITTEN  A = L*U  WHERE       00000260
C                L  IS A PRODUCT OF PERMUTATION AND UNIT LOWER          00000270
C                TRIANGULAR MATRICES AND  U  IS UPPER TRIANGULAR.       00000280
C                                                                       00000290
C        IPVT    INTEGER(N)                                             00000300
C                AN INTEGER VECTOR OF PIVOT INDICES.                    00000310
C                                                                       00000320
C        INFO    INTEGER                                                00000330
C                = 0  NORMAL VALUE.                                     00000340
C                = K  IF  U(K,K) .EQ. 0.0 .  THIS IS NOT AN ERROR       00000350
C                     CONDITION FOR THIS SUBROUTINE, BUT IT DOES        00000360
C                     INDICATE THAT SGESL OR SGEDI WILL DIVIDE BY ZERO  00000370
C                     IF CALLED.  USE  RCOND  IN SGECO FOR A RELIABLE   00000380
C                     INDICATION OF SINGULARITY.                        00000390
C                                                                       00000400
C     LINPACK. THIS VERSION DATED 08/14/78 .                            00000410
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.      00000420
C                                                                       00000430
C     SUBROUTINES AND FUNCTIONS                                         00000440
C                                                                       00000450
C     BLAS SAXPY,SSCAL,ISAMAX                                           00000460
C                                                                       00000470
C     INTERNAL VARIABLES                                                00000480
C                                                                       00000490
      REAL T                                                            00000500
      INTEGER ISAMAX,J,K,KP1,L,NM1                                      00000510
C                                                                       00000520
C                                                                       00000530
C     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING                        00000540
C                                                                       00000550
      INFO = 0                                                          00000560
      NM1 = N - 1                                                       00000570
      IF (NM1 .LT. 1) GO TO 70                                          00000580
      DO 60 K = 1, NM1                                                  00000590
         KP1 = K + 1                                                    00000600
C                                                                       00000610
C        FIND L = PIVOT INDEX                                           00000620
C                                                                       00000630
         L = ISAMAX(N-K+1,A(K,K),1) + K - 1                             00000640
         IPVT(K) = L                                                    00000650
C                                                                       00000660
C        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED          00000670
C                                                                       00000680
         IF (A(L,K) .EQ. 0.0E0) GO TO 40                                00000690
C                                                                       00000700
C           INTERCHANGE IF NECESSARY                                    00000710
C                                                                       00000720
            IF (L .EQ. K) GO TO 10                                      00000730
               T = A(L,K)                                               00000740
               A(L,K) = A(K,K)                                          00000750
               A(K,K) = T                                               00000760
   10       CONTINUE                                                    00000770
C                                                                       00000780
C           COMPUTE MULTIPLIERS                                         00000790
C                                                                       00000800
            T = -1.0E0/A(K,K)                                           00000810
            CALL SSCAL(N-K,T,A(K+1,K),1)                                00000820
C                                                                       00000830
C           ROW ELIMINATION WITH COLUMN INDEXING                        00000840
C                                                                       00000850
            DO 30 J = KP1, N                                            00000860
               T = A(L,J)                                               00000870
               IF (L .EQ. K) GO TO 20                                   00000880
                  A(L,J) = A(K,J)                                       00000890
                  A(K,J) = T                                            00000900
   20          CONTINUE                                                 00000910
               CALL SAXPY(N-K,T,A(K+1,K),1,A(K+1,J),1)                  00000920
   30       CONTINUE                                                    00000930
         GO TO 50                                                       00000940
   40    CONTINUE                                                       00000950
            INFO = K                                                    00000960
   50    CONTINUE                                                       00000970
   60 CONTINUE                                                          00000980
   70 CONTINUE                                                          00000990
      IPVT(N) = N                                                       00001000
      IF (A(N,N) .EQ. 0.0E0) INFO = N                                   00001010
      RETURN                                                            00001020
      END                                                               00001030
      SUBROUTINE SGESL(A,LDA,N,IPVT,B,JOB)                              00000010
      INTEGER LDA,N,IPVT(1),JOB                                         00000020
      REAL A(LDA,1),B(1)                                                00000030
C                                                                       00000040
C     SGESL SOLVES THE REAL SYSTEM                                      00000050
C     A * X = B  OR  TRANS(A) * X = B                                   00000060
C     USING THE FACTORS COMPUTED BY SGECO OR SGEFA.                     00000070
C                                                                       00000080
C     ON ENTRY                                                          00000090
C                                                                       00000100
C        A       REAL(LDA, N)                                           00000110
C                THE OUTPUT FROM SGECO OR SGEFA.                        00000120
C                                                                       00000130
C        LDA     INTEGER                                                00000140
C                THE LEADING DIMENSION OF THE ARRAY  A .                00000150
C                                                                       00000160
C        N       INTEGER                                                00000170
C                THE ORDER OF THE MATRIX  A .                           00000180
C                                                                       00000190
C        IPVT    INTEGER(N)                                             00000200
C                THE PIVOT VECTOR FROM SGECO OR SGEFA.                  00000210
C                                                                       00000220
C        B       REAL(N)                                                00000230
C                THE RIGHT HAND SIDE VECTOR.                            00000240
C                                                                       00000250
C        JOB     INTEGER                                                00000260
C                = 0         TO SOLVE  A*X = B ,                        00000270
C                = NONZERO   TO SOLVE  TRANS(A)*X = B  WHERE            00000280
C                            TRANS(A)  IS THE TRANSPOSE.                00000290
C                                                                       00000300
C     ON RETURN                                                         00000310
C                                                                       00000320
C        B       THE SOLUTION VECTOR  X .                               00000330
C                                                                       00000340
C     ERROR CONDITION                                                   00000350
C                                                                       00000360
C        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS A   00000370
C        ZERO ON THE DIAGONAL.  TECHNICALLY THIS INDICATES SINGULARITY  00000380
C        BUT IT IS OFTEN CAUSED BY IMPROPER ARGUMENTS OR IMPROPER       00000390
C        SETTING OF LDA .  IT WILL NOT OCCUR IF THE SUBROUTINES ARE     00000400
C        CALLED CORRECTLY AND IF SGECO HAS SET RCOND .GT. 0.0           00000410
C        OR SGEFA HAS SET INFO .EQ. 0 .                                 00000420
C                                                                       00000430
C     TO COMPUTE  INVERSE(A) * C  WHERE  C  IS A MATRIX                 00000440
C     WITH  P  COLUMNS                                                  00000450
C           CALL SGECO(A,LDA,N,IPVT,RCOND,Z)                            00000460
C           IF (RCOND IS TOO SMALL) GO TO ...                           00000470
C           DO 10 J = 1, P                                              00000480
C              CALL SGESL(A,LDA,N,IPVT,C(1,J),0)                        00000490
C        10 CONTINUE                                                    00000500
C                                                                       00000510
C     LINPACK. THIS VERSION DATED 08/14/78 .                            00000520
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.      00000530
C                                                                       00000540
C     SUBROUTINES AND FUNCTIONS                                         00000550
C                                                                       00000560
C     BLAS SAXPY,SDOT                                                   00000570
C                                                                       00000580
C     INTERNAL VARIABLES                                                00000590
C                                                                       00000600
      REAL SDOT,T                                                       00000610
      INTEGER K,KB,L,NM1                                                00000620
C                                                                       00000630
      NM1 = N - 1                                                       00000640
      IF (JOB .NE. 0) GO TO 50                                          00000650
C                                                                       00000660
C        JOB = 0 , SOLVE  A * X = B                                     00000670
C        FIRST SOLVE  L*Y = B                                           00000680
C                                                                       00000690
         IF (NM1 .LT. 1) GO TO 30                                       00000700
         DO 20 K = 1, NM1                                               00000710
            L = IPVT(K)                                                 00000720
            T = B(L)                                                    00000730
            IF (L .EQ. K) GO TO 10                                      00000740
               B(L) = B(K)                                              00000750
               B(K) = T                                                 00000760
   10       CONTINUE                                                    00000770
            CALL SAXPY(N-K,T,A(K+1,K),1,B(K+1),1)                       00000780
   20    CONTINUE                                                       00000790
   30    CONTINUE                                                       00000800
C                                                                       00000810
C        NOW SOLVE  U*X = Y                                             00000820
C                                                                       00000830
         DO 40 KB = 1, N                                                00000840
            K = N + 1 - KB                                              00000850
            B(K) = B(K)/A(K,K)                                          00000860
            T = -B(K)                                                   00000870
            CALL SAXPY(K-1,T,A(1,K),1,B(1),1)                           00000880
   40    CONTINUE                                                       00000890
      GO TO 100                                                         00000900
   50 CONTINUE                                                          00000910
C                                                                       00000920
C        JOB = NONZERO, SOLVE  TRANS(A) * X = B                         00000930
C        FIRST SOLVE  TRANS(U)*Y = B                                    00000940
C                                                                       00000950
         DO 60 K = 1, N                                                 00000960
            T = SDOT(K-1,A(1,K),1,B(1),1)                               00000970
            B(K) = (B(K) - T)/A(K,K)                                    00000980
   60    CONTINUE                                                       00000990
C                                                                       00001000
C        NOW SOLVE TRANS(L)*X = Y                                       00001010
C                                                                       00001020
         IF (NM1 .LT. 1) GO TO 90                                       00001030
         DO 80 KB = 1, NM1                                              00001040
            K = N - KB                                                  00001050
            B(K) = B(K) + SDOT(N-K,A(K+1,K),1,B(K+1),1)                 00001060
            L = IPVT(K)                                                 00001070
            IF (L .EQ. K) GO TO 70                                      00001080
               T = B(L)                                                 00001090
               B(L) = B(K)                                              00001100
               B(K) = T                                                 00001110
   70       CONTINUE                                                    00001120
   80    CONTINUE                                                       00001130
   90    CONTINUE                                                       00001140
  100 CONTINUE                                                          00001150
      RETURN                                                            00001160
      END                                                               00001170
