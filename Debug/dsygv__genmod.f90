        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 14 18:09:35 2018
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DSYGV__genmod
          INTERFACE 
            SUBROUTINE DSYGV(ITYPE,JOBZ,UPLO,N,A,LDA,B,LDB,W,WORK,LWORK,&
     &INFO)
              INTEGER(KIND=4) :: LDB
              INTEGER(KIND=4) :: LDA
              INTEGER(KIND=4) :: ITYPE
              CHARACTER(LEN=1) :: JOBZ
              CHARACTER(LEN=1) :: UPLO
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: A(LDA,*)
              REAL(KIND=8) :: B(LDB,*)
              REAL(KIND=8) :: W(*)
              REAL(KIND=8) :: WORK(*)
              INTEGER(KIND=4) :: LWORK
              INTEGER(KIND=4) :: INFO
            END SUBROUTINE DSYGV
          END INTERFACE 
        END MODULE DSYGV__genmod
