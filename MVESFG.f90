SUBROUTINE MVESFG(NELE, NNOS, CELE, ROTS, ESFL, CARN, ESFG)
    ! Subrotina MVESFL: Montar Vetor de ESFor�os Globais da estrutura
    IMPLICIT NONE

    INTEGER, INTENT(in) :: NELE, NNOS
    INTEGER, DIMENSION(NELE,2), INTENT(in) :: CELE
    REAL(kind=8), DIMENSION(6,6,NELE), INTENT(in) :: ROTS
    REAL(kind=8), DIMENSION(6,NELE), INTENT(in) :: ESFL
    REAL(kind=8), DIMENSION(3*NNOS), INTENT(in) :: CARN
    REAL(kind=8), DIMENSION(3*NNOS), INTENT(out) :: ESFG

    INTEGER :: i, j, k, noj, nok
    REAL(kind=8), DIMENSION(6) :: vesf_loc, vesf_glo
    REAL(kind=8), DIMENSION(6,6) :: mrot

    ! NELE: vari�vel que armazena n�mero de elementos
    ! NNOS: vari�vel que armazena n�mero de n�s
    ! CELE: matriz que armazena �ndices dos n�s iniciais e finais de cada elemento [n�i,n�f]
    ! ROTS: matriz 3D que armazena a matriz de rota��o local de cada elemento
    ! ESFL: matriz 2D que armazena vetor de esfor�o local em cada elemento [6,num_elem]
    ! CARN: vetor que armazena esfor�os nodais
    ! ESFG: vetor que armazena esfor�os globais na estrutura

    ! i: vari�vel para ser iterada
    ! j: vari�vel para ser iterada
    ! k: vari�vel para ser iterada
    ! noj: vari�vel de aux�lio para montagem da matriz de rigidez da estrutura (define linha)
    ! nok: vari�vel de aux�lio para montagem da matriz de rigidez da estrutura (define coluna)
    ! vesf_loc: vetor auxiliar para armazenar vetor de esfor�os locais de cada elemento
    ! vesf_glo: vetor auxiliar para armazenar vetor de esfor�os globais de cada elemento
    ! mrot: matriz auxiliar para armazenar matriz de rota��o de cada elemento

    ESFG = 0.0

    DO i=1, NELE
        ! Montagem do vetor de esfor�os locais e matriz de rota��o do elemento i
        DO j=1, 6
            vesf_loc(j) = ESFL(j,i)                 ! Vetor de esfor�os locais
            DO k=1, 6
                mrot(j,k) = ROTS(j,k,i)             ! Matriz de rota��o
            END DO
        END DO

        ! C�lculo do vetor de esfor�os globais {esfg} = [ROT]t{esfl}
        vesf_glo = MATMUL(TRANSPOSE(mrot),vesf_loc)

        ! Endere�amento do vetor de solicita��es globais para vetor esfor�os globais
        DO j=-2, 0
            noj = 3 * CELE(i,1) + j
            nok = 3 * CELE(i,2) + j
            ESFG(noj) = ESFG(noj) + vesf_glo(3 + j)
            ESFG(nok) = ESFG(nok) + vesf_glo(6 + j)
        ENDDO
    END DO

    ESFG = ESFG + CARN

END SUBROUTINE MVESFG