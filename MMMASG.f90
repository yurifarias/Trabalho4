SUBROUTINE MMMASG(NELE, NNOS, CELE, MASL, ROTS, MASG)
    ! Subrotina MMMASG: Montar Matriz de MASsa Global da estrutura
    IMPLICIT NONE

    INTEGER, INTENT(in) :: NELE, NNOS
    INTEGER, DIMENSION(NELE,2) :: CELE
    REAL(kind=8), DIMENSION(6,6,NELE), INTENT(in) :: MASL, ROTS
    REAL(kind=8), DIMENSION(3*NNOS,3*NNOS), INTENT(out) :: MASG

    INTEGER :: i, j, k, noj, nok
    REAL(kind=8), DIMENSION(6,6) :: mmas_loc, mmas_glo, mrot

    ! NELE: vari�vel que armazena n�mero de elementos
    ! NNOS: vari�vel que armazena n�mero de n�s
    ! CELE: matriz que armazena �ndices dos n�s iniciais e finais de cada elemento [n�i, n�f]
    ! MASL: matriz 3D que armazena matrizes de massa local de cada elemento
    ! ROTS: matriz 3D que armazena matrizes de rota��o local de cada elemento
    ! MASG: matriz de massa global da estrutura

    ! i: vari�vel para ser iterada
    ! j: vari�vel para ser iterada
    ! k: vari�vel para ser iterada
    ! noj: vari�vel de aux�lio para montagem da matriz de massa da estrutura (define linha)
    ! nok: vari�vel de aux�lio para montagem da matriz de massa da estrutura (define coluna)
    ! mmas_loc: matriz auxiliar para armazenar matriz de massa local de cada elemento
    ! mmas_glo: matriz auxiliar para armazenar matriz de massa global de cada elemento
    ! mrot: matriz auxiliar para armazenar matriz de rota��o de cada elemento

    MASG = 0.0

    DO i=1, NELE
        ! Montagem da matriz de massa local e matriz de rota��o do elemento i
        DO j=1, 6
            DO k=1, 6
                mmas_loc(j,k) = MASL(j,k,i)         ! Matriz de massa local
                mrot(j,k) = ROTS(j,k,i)             ! Matriz de rota��o
            END DO
        END DO

        ! C�lculo da matriz de massa global do elemento i [Kg] = [R]t[Kl][R]
        mmas_glo = MATMUL(MATMUL(TRANSPOSE(mrot),mmas_loc),mrot)

        ! In�cio da montagem da matriz de massa global da estrutura

        ! Para (n� inicial, n� inicial)
        DO j=-2, 0
            noj = 3 * CELE(i,1) + j             ! Linha da matriz de massa global da estrutura
            DO k=-2, 0
                nok = 3 * CELE(i,1) + k         ! Coluna da matriz de massa global da estrutura
                MASG(noj, nok) = MASG(noj, nok) + mmas_glo(3 + j, 3 + k)
            ENDDO
        ENDDO

        ! Para (n� inicial, n� final)
        DO j=-2, 0
            noj = 3 * CELE(i,1) + j             ! Linha da matriz de massa global da estrutura
            DO k=-2, 0
                nok = 3 * CELE(i,2) + k         ! Coluna da matriz de massa global da estrutura
                MASG(noj, nok) = MASG(noj, nok) + mmas_glo(3 + j, 6 + k)
            ENDDO
        ENDDO

        ! Para (n� final, n� inicial)
        DO j=-2, 0
            noj = 3 * CELE(i,2) + j             ! Linha da matriz de massa global da estrutura
            DO k=-2, 0
                nok = 3 * CELE(i,1) + k         ! Coluna da matriz de massa global da estrutura
                MASG(noj, nok) = MASG(noj, nok) + mmas_glo(6 + j, 3 + k)
            ENDDO
        ENDDO

        ! Para (n� final, n� final)
        DO j=-2, 0
            noj = 3 * CELE(i,2) + j             ! Linha da matriz de massa global da estrutura
            DO k=-2, 0
                nok = 3 * CELE(i,2) + k         ! Coluna da matriz de massa global da estrutura
                MASG(noj, nok) = MASG(noj, nok) + mmas_glo(6 + j, 6 + k)
            ENDDO
        ENDDO

        ! Fim da montagem da matriz global de massa da estrutura
    END DO
END SUBROUTINE MMMASG