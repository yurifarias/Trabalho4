        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 14 18:09:32 2018
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DLAED2__genmod
          INTERFACE 
            SUBROUTINE DLAED2(K,N,N1,D,Q,LDQ,INDXQ,RHO,Z,DLAMDA,W,Q2,   &
     &INDX,INDXC,INDXP,COLTYP,INFO)
              INTEGER(KIND=4) :: LDQ
              INTEGER(KIND=4) :: K
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: N1
              REAL(KIND=8) :: D(*)
              REAL(KIND=8) :: Q(LDQ,*)
              INTEGER(KIND=4) :: INDXQ(*)
              REAL(KIND=8) :: RHO
              REAL(KIND=8) :: Z(*)
              REAL(KIND=8) :: DLAMDA(*)
              REAL(KIND=8) :: W(*)
              REAL(KIND=8) :: Q2(*)
              INTEGER(KIND=4) :: INDX(*)
              INTEGER(KIND=4) :: INDXC(*)
              INTEGER(KIND=4) :: INDXP(*)
              INTEGER(KIND=4) :: COLTYP(*)
              INTEGER(KIND=4) :: INFO
            END SUBROUTINE DLAED2
          END INTERFACE 
        END MODULE DLAED2__genmod
