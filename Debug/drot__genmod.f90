        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 14 18:09:39 2018
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DROT__genmod
          INTERFACE 
            SUBROUTINE DROT(N,DX,INCX,DY,INCY,C,S)
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: DX(*)
              INTEGER(KIND=4) :: INCX
              REAL(KIND=8) :: DY(*)
              INTEGER(KIND=4) :: INCY
              REAL(KIND=8) :: C
              REAL(KIND=8) :: S
            END SUBROUTINE DROT
          END INTERFACE 
        END MODULE DROT__genmod