        !COMPILER-GENERATED INTERFACE MODULE: Mon Nov 12 21:41:06 2018
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE MMRL__genmod
          INTERFACE 
            SUBROUTINE MMRL(NELE,NNOS,CELE,CNOS,LARGU,ALTUR,MEL,RIGS)
              INTEGER(KIND=4), INTENT(IN) :: NNOS
              INTEGER(KIND=4), INTENT(IN) :: NELE
              INTEGER(KIND=4), INTENT(IN) :: CELE(NELE,2)
              REAL(KIND=8), INTENT(IN) :: CNOS(NNOS,2)
              REAL(KIND=8), INTENT(IN) :: LARGU(NELE)
              REAL(KIND=8), INTENT(IN) :: ALTUR(NELE)
              REAL(KIND=8), INTENT(IN) :: MEL(NELE)
              REAL(KIND=8), INTENT(OUT) :: RIGS(6,6,NELE)
            END SUBROUTINE MMRL
          END INTERFACE 
        END MODULE MMRL__genmod
