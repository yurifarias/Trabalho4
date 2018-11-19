        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 14 18:09:39 2018
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE AUTOPROBLEMA__genmod
          INTERFACE 
            SUBROUTINE AUTOPROBLEMA(A,M,N,IQ,ITER,RESFREQ,REST)
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: A(N,N)
              REAL(KIND=8) :: M(N,N)
              INTEGER(KIND=4) :: IQ
              INTEGER(KIND=4) :: ITER
              REAL(KIND=8) :: RESFREQ(10)
              INTEGER(KIND=4) :: REST(N)
            END SUBROUTINE AUTOPROBLEMA
          END INTERFACE 
        END MODULE AUTOPROBLEMA__genmod
