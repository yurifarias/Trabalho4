        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 14 18:09:39 2018
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ILADLC__genmod
          INTERFACE 
            FUNCTION ILADLC(M,N,A,LDA)
              INTEGER(KIND=4) :: LDA
              INTEGER(KIND=4) :: M
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: A(LDA,*)
              INTEGER(KIND=4) :: ILADLC
            END FUNCTION ILADLC
          END INTERFACE 
        END MODULE ILADLC__genmod
