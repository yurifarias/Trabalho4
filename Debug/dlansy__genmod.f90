        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 14 18:09:38 2018
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DLANSY__genmod
          INTERFACE 
            FUNCTION DLANSY(NORM,UPLO,N,A,LDA,WORK)
              INTEGER(KIND=4) :: LDA
              CHARACTER(LEN=1) :: NORM
              CHARACTER(LEN=1) :: UPLO
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: A(LDA,*)
              REAL(KIND=8) :: WORK(*)
              REAL(KIND=8) :: DLANSY
            END FUNCTION DLANSY
          END INTERFACE 
        END MODULE DLANSY__genmod
