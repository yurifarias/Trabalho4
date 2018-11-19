SUBROUTINE CVREAL(NELE, NNOS, CELE, ESFL, DESL, RIGS, REAC)
    ! Subrotina CVREAL: Calcular Vetor de REA��es Locais de cada elemento
    IMPLICIT NONE

    INTEGER, INTENT(in) :: NELE, NNOS
    INTEGER, DIMENSION(NELE,2), INTENT(in) :: CELE
    REAL(kind=8), DIMENSION(6,NELE), INTENT(in) :: ESFL, DESL
    REAL(kind=8), DIMENSION(6,6,NELE), INTENT(in) :: RIGS
    REAL(kind=8), DIMENSION(6,NELE), INTENT(out) :: REAC

    INTEGER :: i, j, k
    REAL(kind=8), DIMENSION(6) :: vesf_loc, vdes_loc, vaux
    REAL(kind=8), DIMENSION(6,6) :: mrig_loc

    ! NELE: vari�vel que armazena n�mero de elementos
    ! NNOS: vari�vel que armazena n�mero de n�s
    ! CELE: matriz que armazena �ndices dos n�s iniciais e finais de cada elemento [n�i, n�f]
    ! ESFL: matriz 2D que armazena vetor de esfor�os locais em cada elemento [6,NELE]
    ! DESL: matriz 2D que armazena vetor de deslocamentos locais em cada elemento [6,NELE]
    ! RIGS: matriz 3D que armazena matrizes de rigidez local de cada elemento [6,6,NELE]
    ! REAC: matriz 2D que armazena rea��es locais de cada elemento [6,NELE]

    ! i: vari�vel para ser iterada
    ! j: vari�vel para ser iterada
    ! k: vari�vel para ser iterada
    ! vesf_loc: vetor auxiliar para armazenar vetor de esfor�os locais de cada elemento
    ! vdes_loc: vetor auxiliar para armazenar vetor de deslocamentos locais de cada elemento
    ! vaux: vetor auxiliar para c�lculo das rea��es locais de cada elemento
    ! mrig_glo: matriz auxiliar para armazenar matriz de rigidez global de cada elemento


    DO i=1, NELE
        ! Montagem da matriz de rigidez local e matriz de rota��o do elemento i
        DO j=1, 6
            vesf_loc(j) = ESFL(j,i)                 ! Vetor de esfor�os locais
            vdes_loc(j) = DESL(j,i)                 ! Vetor de esfor�os locais
            DO k=1, 6
                mrig_loc(j,k) = RIGS(j,k,i)         ! Matriz de rigidez local
            END DO
        END DO

        ! Calcular rea��es locais do elemento i
        vaux = MATMUL(mrig_loc,vdes_loc) - vesf_loc

        DO j=1, 6
            ! Armazenar rea��es locais do elemento i
            REAC(j,i) = vaux(j)
        END DO
    END DO
END SUBROUTINE CVREAL