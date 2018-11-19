        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 14 18:09:34 2018
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DLACPY__genmod
          INTERFACE 
            SUBROUTINE DLACPY(UPLO,M,N,A,LDA,B,LDB)
              INTEGER(KIND=4) :: LDB
              INTEGER(KIND=4) :: LDA
              CHARACTER(LEN=1) :: UPLO
              INTEGER(KIND=4) :: M
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: A(LDA,*)
              REAL(KIND=8) :: B(LDB,*)
            END SUBROUTINE DLACPY
          END INTERFACE 
        END MODULE DLACPY__genmod
