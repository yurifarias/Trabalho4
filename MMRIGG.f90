SUBROUTINE MMRIGG(NELE, NNOS, CELE, RIGS, ROTS, RIGG)
    ! Subrotina MMRIGG: Montar Matriz de RIGidez Global da estrutura
    IMPLICIT NONE

    INTEGER, INTENT(in) :: NELE, NNOS
    INTEGER, DIMENSION(NELE,2) :: CELE
    REAL(kind=8), DIMENSION(6,6,NELE), INTENT(in) :: RIGS, ROTS
    REAL(kind=8), DIMENSION(3*NNOS,3*NNOS), INTENT(out) :: RIGG

    INTEGER :: i, j, k, noj, nok
    REAL(kind=8), DIMENSION(6,6) :: mrig_loc, mrig_glo, mrot

    ! NELE: vari�vel que armazena n�mero de elementos
    ! NNOS: vari�vel que armazena n�mero de n�s
    ! CELE: matriz que armazena �ndices dos n�s iniciais e finais de cada elemento [n�i, n�f]
    ! RIGS: matriz 3D que armazena matrizes de rigidez local de cada elemento
    ! ROTS: matriz 3D que armazena matrizes de rota��o local de cada elemento
    ! RIGG: matriz de rigidez global da estrutura

    ! i: vari�vel para ser iterada
    ! j: vari�vel para ser iterada
    ! k: vari�vel para ser iterada
    ! noj: vari�vel de aux�lio para montagem da matriz de rigidez da estrutura (define linha)
    ! nok: vari�vel de aux�lio para montagem da matriz de rigidez da estrutura (define coluna)
    ! mrig_loc: matriz auxiliar para armazenar matriz de rigidez local de cada elemento
    ! mrig_glo: matriz auxiliar para armazenar matriz de rigidez global de cada elemento
    ! mrot: matriz auxiliar para armazenar matriz de rota��o de cada elemento

    RIGG = 0.0

    DO i=1, NELE
        ! Montagem da matriz de rigidez local e matriz de rota��o do elemento i
        DO j=1, 6
            DO k=1, 6
                mrig_loc(j,k) = RIGS(j,k,i)         ! Matriz de rigidez local
                mrot(j,k) = ROTS(j,k,i)             ! Matriz de rota��o
            END DO
        END DO

        ! C�lculo da matriz de rigidez global do elemento i [Kg] = [R]t[Kl][R]
        mrig_glo = MATMUL(MATMUL(TRANSPOSE(mrot),mrig_loc),mrot)

        ! In�cio da montagem da matriz de rigidez global da estrutura

        ! Para (n� inicial, n� inicial)
        DO j=-2, 0
            noj = 3 * CELE(i,1) + j             ! Linha da matriz de rigidez global da estrutura
            DO k=-2, 0
                nok = 3 * CELE(i,1) + k         ! Coluna da matriz de rigidez global da estrutura
                RIGG(noj, nok) = RIGG(noj, nok) + mrig_glo(3 + j, 3 + k)
            ENDDO
        ENDDO

        ! Para (n� inicial, n� final)
        DO j=-2, 0
            noj = 3 * CELE(i,1) + j             ! Linha da matriz de rigidez global da estrutura
            DO k=-2, 0
                nok = 3 * CELE(i,2) + k         ! Coluna da matriz de rigidez global da estrutura
                RIGG(noj, nok) = RIGG(noj, nok) + mrig_glo(3 + j, 6 + k)
            ENDDO
        ENDDO

        ! Para (n� final, n� inicial)
        DO j=-2, 0
            noj = 3 * CELE(i,2) + j             ! Linha da matriz de rigidez global da estrutura
            DO k=-2, 0
                nok = 3 * CELE(i,1) + k         ! Coluna da matriz de rigidez global da estrutura
                RIGG(noj, nok) = RIGG(noj, nok) + mrig_glo(6 + j, 3 + k)
            ENDDO
        ENDDO

        ! Para (n� final, n� final)
        DO j=-2, 0
            noj = 3 * CELE(i,2) + j             ! Linha da matriz de rigidez global da estrutura
            DO k=-2, 0
                nok = 3 * CELE(i,2) + k         ! Coluna da matriz de rigidez global da estrutura
                RIGG(noj, nok) = RIGG(noj, nok) + mrig_glo(6 + j, 6 + k)
            ENDDO
        ENDDO

        ! Fim da montagem da matriz global de rigidez da estrutura
    END DO

END SUBROUTINE MMRIGG