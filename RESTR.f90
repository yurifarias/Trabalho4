SUBROUTINE RESTR(NNOS, REST, RIGG, MASG, ESFG)
    ! Subrotina RESTR: Inserir RESTRi��es na matriz de rigidez global e vetor de esfor�os globais
    IMPLICIT NONE

    INTEGER, INTENT(in) :: NNOS
    LOGICAL, DIMENSION(3*NNOS), INTENT(in) :: REST
    REAL(kind=8), DIMENSION(3*NNOS) :: ESFG
    REAL(kind=8), DIMENSION(3*NNOS,3*NNOS) :: RIGG, MASG

    INTEGER :: i, j

    ! NNOS: vari�vel que armazena n�mero de n�s
    ! REST: vetor de valores logicos que significam restri��o quando .TRUE.
    ! ESFG: vetor de esfor�os globais na estrutura
    ! RIGG: matriz de rigidez global da estrutura
    ! MASG: matriz de massa global da estrutura

    ! i: vari�vel para iterar
    ! j: vari�vel para iterar

    ! Inclus�o das restri��es
    DO i=1, 3*NNOS
        IF (REST(i)) THEN
            ESFG(i) = 0
            DO j=1, 3*nnos
                IF (i == j) THEN
                    RIGG(i,i) = 1
                    MASG(i,i) = 1
                ELSE
                    RIGG(i,j) = 0
                    RIGG(j,i) = 0
                    MASG(i,j) = 0
                    MASG(j,i) = 0
                ENDIF
            ENDDO
        ENDIF
    ENDDO

END SUBROUTINE RESTR