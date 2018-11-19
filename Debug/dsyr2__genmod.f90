        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 14 18:09:39 2018
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DSYR2__genmod
          INTERFACE 
            SUBROUTINE DSYR2(UPLO,N,ALPHA,X,INCX,Y,INCY,A,LDA)
              INTEGER(KIND=4) :: LDA
              CHARACTER(LEN=1) :: UPLO
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: ALPHA
              REAL(KIND=8) :: X(*)
              INTEGER(KIND=4) :: INCX
              REAL(KIND=8) :: Y(*)
              INTEGER(KIND=4) :: INCY
              REAL(KIND=8) :: A(LDA,*)
            END SUBROUTINE DSYR2
          END INTERFACE 
        END MODULE DSYR2__genmod
