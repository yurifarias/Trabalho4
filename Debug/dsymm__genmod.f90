        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 14 18:09:33 2018
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DSYMM__genmod
          INTERFACE 
            SUBROUTINE DSYMM(SIDE,UPLO,M,N,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
              INTEGER(KIND=4) :: LDC
              INTEGER(KIND=4) :: LDB
              INTEGER(KIND=4) :: LDA
              CHARACTER(LEN=1) :: SIDE
              CHARACTER(LEN=1) :: UPLO
              INTEGER(KIND=4) :: M
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: ALPHA
              REAL(KIND=8) :: A(LDA,*)
              REAL(KIND=8) :: B(LDB,*)
              REAL(KIND=8) :: BETA
              REAL(KIND=8) :: C(LDC,*)
            END SUBROUTINE DSYMM
          END INTERFACE 
        END MODULE DSYMM__genmod
