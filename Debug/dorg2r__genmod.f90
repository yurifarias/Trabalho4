        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 14 18:09:37 2018
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DORG2R__genmod
          INTERFACE 
            SUBROUTINE DORG2R(M,N,K,A,LDA,TAU,WORK,INFO)
              INTEGER(KIND=4) :: LDA
              INTEGER(KIND=4) :: M
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: K
              REAL(KIND=8) :: A(LDA,*)
              REAL(KIND=8) :: TAU(*)
              REAL(KIND=8) :: WORK(*)
              INTEGER(KIND=4) :: INFO
            END SUBROUTINE DORG2R
          END INTERFACE 
        END MODULE DORG2R__genmod
