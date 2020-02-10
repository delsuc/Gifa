      SUBROUTINE CDGEDI(A,LDA,N,IPVT,DET,WORK,JOB)                     
      INTEGER LDA,N,IPVT(1),JOB                                       
      COMPLEX*16 A(LDA,1),DET(2),WORK(1)                                 
C                                                                     
C     CDGEDI COMPUTES THE DETERMINANT AND INVERSE OF A MATRIX          
C     USING THE FACTORS COMPUTED BY CDGECO OR CDGEFA.                   
C                                                                     
C     ON ENTRY                                                        
C                                                                     
C        A       COMPLEX *16 (LDA, N)                                      
C                THE OUTPUT FROM CDGECO OR CDGEFA.                      
C                                                                     
C        LDA     INTEGER                                              
C                THE LEADING DIMENSION OF THE ARRAY  A .              
C                                                                     
C        N       INTEGER                                              
C                THE ORDER OF THE MATRIX  A .                         
C                                                                     
C        IPVT    INTEGER(N)                                           
C                THE PIVOT VECTOR FROM CDGECO OR CDGEFA.                
C                                                                     
C        WORK    COMPLEX *16 (N)                                           
C                WORK VECTOR.  CONTENTS DESTROYED.                    
C                                                                     
C        JOB     INTEGER                                              
C                = 11   BOTH DETERMINANT AND INVERSE.                 
C                = 01   INVERSE ONLY.                                 
C                = 10   DETERMINANT ONLY.                             
C                                                                     
C     ON RETURN                                                       
C                                                                     
C        A       INVERSE OF ORIGINAL MATRIX IF REQUESTED.             
C                OTHERWISE UNCHANGED.                                 
C                                                                     
C        DET     COMPLEX *16 (2)                                           
C                DETERMINANT OF ORIGINAL MATRIX IF REQUESTED.         
C                OTHERWISE NOT REFERENCED.                            
C                DETERMINANT = DET(1) * 10.0**DET(2)                  
C                WITH  1.0 .LE. CABS1(DET(1)) .LT. 10.0               
C                OR  DET(1) .EQ. 0.0 .                                
C                                                                     
C     ERROR CONDITION                                                 
C                                                                     
C        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS   
C        A ZERO ON THE DIAGONAL AND THE INVERSE IS REQUESTED.         
C        IT WILL NOT OCCUR IF THE SUBROUTINES ARE CALLED CORRECTLY    
C        AND IF CDGECO HAS SET RCOND .GT. 0.0 OR CDGEFA HAS SET         
C        INFO .EQ. 0 .                                                
C                                                                     
C     LINPACK. THIS VERSION DATED 08/14/78 .                          
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.    
C                                                                     
C     SUBROUTINES AND FUNCTIONS                                       
C                                                                     
C     BLAS CDAXPY,CDSCAL,CDSWAP                                          
C     FORTRAN DABS,DIMAG,DCMPLX,MOD,DBLE                                
C                                                                     
C     INTERNAL VARIABLES                                              
C                                                                     
      COMPLEX*16 T                                                       
      REAL*8 TEN                                                        
      INTEGER I,J,K,KB,KP1,L,NM1                                      
C                                                                     
      COMPLEX*16 ZDUM                                                    
      REAL*8 CDABS1                                                      
      CDABS1(ZDUM) = DABS(DBLE(ZDUM)) + DABS(DIMAG(ZDUM))                
C                                                                     
C     COMPUTE DETERMINANT                                             
C                                                                     
      IF (JOB/10 .EQ. 0) GO TO 70                                     
         DET(1) = (1.0D0,0.0D0)                                       
         DET(2) = (0.0D0,0.0D0)                                       
         TEN = 10.0D0                                                 
         DO 50 I = 1, N                                               
            IF (IPVT(I) .NE. I) DET(1) = -DET(1)                      
            DET(1) = A(I,I)*DET(1)                                    
C        ...EXIT                                                      
            IF (CDABS1(DET(1)) .EQ. 0.0D0) GO TO 60                    
   10       IF (CDABS1(DET(1)) .GE. 1.0D0) GO TO 20                    
               DET(1) = DCMPLX(TEN,0.0D0)*DET(1)                       
               DET(2) = DET(2) - (1.0D0,0.0D0)                        
            GO TO 10                                                  
   20       CONTINUE                                                  
   30       IF (CDABS1(DET(1)) .LT. TEN) GO TO 40                      
               DET(1) = DET(1)/DCMPLX(TEN,0.0D0)                       
               DET(2) = DET(2) + (1.0D0,0.0D0)                        
            GO TO 30                                                  
   40       CONTINUE                                                  
   50    CONTINUE                                                     
   60    CONTINUE                                                     
   70 CONTINUE                                                        
C                                                                     
C     COMPUTE INVERSE(U)                                              
C                                                                     
      IF (MOD(JOB,10) .EQ. 0) GO TO 150                               
         DO 100 K = 1, N                                              
            A(K,K) = (1.0D0,0.0D0)/A(K,K)                             
            T = -A(K,K)                                               
            CALL CDSCAL(K-1,T,A(1,K),1)                                
            KP1 = K + 1                                               
            IF (N .LT. KP1) GO TO 90                                  
            DO 80 J = KP1, N                                          
               T = A(K,J)                                             
               A(K,J) = (0.0D0,0.0D0)                                 
               CALL CDAXPY(K,T,A(1,K),1,A(1,J),1)                      
   80       CONTINUE                                                  
   90       CONTINUE                                                  
  100    CONTINUE                                                     
C                                                                     
C        FORM INVERSE(U)*INVERSE(L)                                   
C                                                                     
         NM1 = N - 1                                                  
         IF (NM1 .LT. 1) GO TO 140                                    
         DO 130 KB = 1, NM1                                           
            K = N - KB                                                
            KP1 = K + 1                                               
            DO 110 I = KP1, N                                         
               WORK(I) = A(I,K)                                       
               A(I,K) = (0.0D0,0.0D0)                                 
  110       CONTINUE                                                  
            DO 120 J = KP1, N                                         
               T = WORK(J)                                            
               CALL CDAXPY(N,T,A(1,J),1,A(1,K),1)                      
  120       CONTINUE                                                  
            L = IPVT(K)                                               
            IF (L .NE. K) CALL CDSWAP(N,A(1,K),1,A(1,L),1)             
  130    CONTINUE                                                     
  140    CONTINUE                                                     
  150 CONTINUE                                                        
      RETURN                                                          
      END                                                             
