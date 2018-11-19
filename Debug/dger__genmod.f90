        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 14 18:14:35 2018
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DGER__genmod
          INTERFACE 
            SUBROUTINE DGER(M,N,ALPHA,X,INCX,Y,INCY,A,LDA)
              INTEGER(KIND=4) :: LDA
              INTEGER(KIND=4) :: M
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: ALPHA
              REAL(KIND=8) :: X(*)
              INTEGER(KIND=4) :: INCX
              REAL(KIND=8) :: Y(*)
              INTEGER(KIND=4) :: INCY
              REAL(KIND=8) :: A(LDA,*)
            END SUBROUTINE DGER
          END INTERFACE 
        END MODULE DGER__genmod
