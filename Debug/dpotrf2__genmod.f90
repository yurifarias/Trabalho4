        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 14 18:09:32 2018
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DPOTRF2__genmod
          INTERFACE 
            RECURSIVE SUBROUTINE DPOTRF2(UPLO,N,A,LDA,INFO)
              INTEGER(KIND=4) :: LDA
              CHARACTER(LEN=1) :: UPLO
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: A(LDA,*)
              INTEGER(KIND=4) :: INFO
            END SUBROUTINE DPOTRF2
          END INTERFACE 
        END MODULE DPOTRF2__genmod
