	SUBROUTINE AUTOPROBLEMA(A,M,N,IQ,ITER,RESFREQ,REST)
! input: A e M: matrizes de rigidez e massa da estrutura, iq aarq a ser impresso auto-respostas
! output: A[,]: contem os autovetores, vetor da coluna i é o desloc da freq natural i; M[]:  contem os autovalores na 1a. coluna 
! arquivo fort.55 imprime autovalores
      Real(8) A(N,N),M(N,N),RESFREQ(10),AUX
      Real(8), Allocatable :: AUTOVALOR(:), WORK(:)    
      CHARACTER*1 JOBZ,UPLO       
      INTEGER N,REST(N)
      ALLOCATE(AUTOVALOR(N))         
      ! MATRIZES TEM QUE SER SIMETRICAS!
      ITYPE = 1   ! = 1:  A*x = (lambda)*B*x
      IF(ITER.EQ.1)JOBZ = 'V'  ! 'V'   = Compute eigenvalues and eigenvectors.
      IF(ITER.NE.1)JOBZ = 'N'  ! 'N'   = Compute only eigenvalues.
      UPLO = 'U'  ! Upper triangles of A and B are stored;  N =  The order of the matrices A and B.  N >= 0.
      LDA = N
      LDB = N
      INFO = 0
      LWORK = (N+2)*N
      ALLOCATE(WORK(LWORK))
      WORK = 0.D0
      !LWORK is INTEGER, The length of the array WORK.  LWORK >= max(1,3*N-1). For optimal efficiency, LWORK >= (NB+2)*N,
                         ! where NB is the blocksize for DSYTRD returned by ILAENV.      
      ! AUTOVALOR is DOUBLE PRECISION array, dimension (N),  If INFO = 0, the eigenvalues in ascending order.     
      CALL DSYGV(ITYPE,JOBZ,UPLO,N,A,LDA,M,LDB,AUTOVALOR,WORK,LWORK,
     + INFO) ! dsygv.for      
 !OUT: INFO is INTEGER: = 0:  successful exit; < 0:  if INFO = -i, the i-th argument had an illegal value;
                 ! > 0:  DPOTRF or DSYEV returned an error code:;   <= N:  if INFO = i, DSYEV failed to converge i off-diagonal elements of an intermediate
                 ! > tridiagonal form did not converge to zero;      
      IO = 0
      DO I =1,N
          AUTOVALOR(I) = DSQRT(AUTOVALOR(I))
          !M(I,1) = AUTOVALOR(I) 
          !IF(M(I,1).GT.1.AND.IO.EQ.0)THEN
          !    RESFREQ(3) = I
          !    IO = 1
          !ENDIF         
      ENDDO
      
      IF(IQ.NE.0)CALL PRINT_AUTOVALOR(AUTOVALOR,N,IQ,ITER)
      
      ! guardando as 10 primeiras freq. naturais, dos gdl nao restritos:
      IO = 0
  	!DO I=1,N
	  !IF (REST(I).EQ.1)  IO = IO + 1                           
   !   ENDDO   
      J = 0
      DO I = IO+1,N
          IF(DABS(AUTOVALOR(I)-1.D0).GT.1E-6)THEN  
              J = J + 1
	        RESFREQ(J) = AUTOVALOR(I)  
              IF(J.GE.10) GOTO 678      
          ENDIF      
      ENDDO 
     
 678  CONTINUE
          
!! NORMALIZANDO AUTOVETOR COM RELACAO A U(1)      
!      IF(ITER.EQ.1)THEN
!          DO i =1,n
!              AUX = A(1,I)
!              A(1,I)= 1.D0
!              DO J=2,N          
!                  IF(DABS(AUX).GT.1E-8) A(J,I) = A(J,I)/AUX
!              ENDDO
!          ENDDO
!      ENDIF
      
    !  IF(IQ.NE.0)CALL PRINT_AUTOVETOR(AUTOVALOR,A,N,IQ)
         
      DEALLOCATE(AUTOVALOR,WORK)
     
      END
!-------------------------------------------------------------------
      SUBROUTINE PRINT_AUTOVALOR(AUTOVALOR,N,IQ,ITER)
      REAL*8 AUTOVALOR(N)
      
	CALL GETDAT (IANO,IMES,IDIA)	 ! Data do processamento
      CALL GETTIM (IHORA1,IMIN1,ISEC1,I100TH)	 ! Hora Inicial 
      
      
	write(IQ,301) IDIA,IMES,IANO, IHORA1,IMIN1,ISEC1      
      WRITE(IQ,34)ITER
      write(IQ,*)'NR       Freq(rad/s)      Freq(Hz)'
      do i = 1,n
          write(IQ,1)i,AUTOVALOR(i),AUTOVALOR(i)/(2*dacos(-1.d0))
      enddo
1     FORMAT('AUTOVALOR(',I5,')=',f12.3,4x,f12.3)      
      write(IQ,2)
2     FORMAT(//)    
301   format (1X'DATA:',I2,1H-,I2,1H-,I4,/,1X,'HORA:',
     $            I2.2,1H:,I2.2,1H:,I2.2,3X)
34    FORMAT('ITER = ',I4)          
      END
!-------------------------------------------------------------------
      SUBROUTINE PRINT_AUTOVETOR(AUTOVALOR,A,N,IQ)
      REAL*8 AUTOVALOR(N),A(N,N)
      do I = 1,N
          write(IQ,1)i,AUTOVALOR(i)   
          if(i.eq.1) then
              DO J=1,N
                  write(IQ,2)J,A(J,I)   
              ENDDO
          endif
          write(IQ,3)
      enddo
1     FORMAT('AUTOVALOR(',I5,')=',ES12.2,/)            
2     FORMAT('AUTOVETOR(',I5,')=',ES20.4)      
3     FORMAT(/)            
      END
!-------------------------------------------------------------------
            