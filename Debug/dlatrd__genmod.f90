        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 14 18:09:37 2018
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DLATRD__genmod
          INTERFACE 
            SUBROUTINE DLATRD(UPLO,N,NB,A,LDA,E,TAU,W,LDW)
              INTEGER(KIND=4) :: LDW
              INTEGER(KIND=4) :: LDA
              CHARACTER(LEN=1) :: UPLO
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: NB
              REAL(KIND=8) :: A(LDA,*)
              REAL(KIND=8) :: E(*)
              REAL(KIND=8) :: TAU(*)
              REAL(KIND=8) :: W(LDW,*)
            END SUBROUTINE DLATRD
          END INTERFACE 
        END MODULE DLATRD__genmod
