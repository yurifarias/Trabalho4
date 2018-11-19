        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 14 18:09:35 2018
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DSYGST__genmod
          INTERFACE 
            SUBROUTINE DSYGST(ITYPE,UPLO,N,A,LDA,B,LDB,INFO)
              INTEGER(KIND=4) :: LDB
              INTEGER(KIND=4) :: LDA
              INTEGER(KIND=4) :: ITYPE
              CHARACTER(LEN=1) :: UPLO
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: A(LDA,*)
              REAL(KIND=8) :: B(LDB,*)
              INTEGER(KIND=4) :: INFO
            END SUBROUTINE DSYGST
          END INTERFACE 
        END MODULE DSYGST__genmod
