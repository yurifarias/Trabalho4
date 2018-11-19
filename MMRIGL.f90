SUBROUTINE MMRIGL(NELE, NNOS, CELE, CNOS, LAR, ALT, MEL, RIGS)
    ! Subrotina MMRIGL: Montar Matriz de RIGidez Local de cada elemento
    IMPLICIT NONE

    INTEGER, INTENT(in) :: NELE, NNOS
    INTEGER, DIMENSION(NELE,2), INTENT(in) :: CELE
    REAL(kind=8), DIMENSION(NNOS,2), INTENT(in) :: CNOS
    REAL(kind=8), DIMENSION(NELE), INTENT(in) :: LAR, ALT, MEL
    REAL(kind=8), DIMENSION(6,6,NELE), INTENT(out) :: RIGS

    INTEGER :: i
    REAL(kind=8) :: aux1, aux2, aux3, aux4, aux5
    REAL(kind=8), DIMENSION(:), ALLOCATABLE :: area, iner, comp
    REAL(kind=8), DIMENSION(:,:), ALLOCATABLE :: proj

    ! NELE: vari�vel que armazena n�mero de elementos
    ! NNOS: vari�vel que armazena n�mero de n�s
    ! CELE: matriz que armazena �ndices dos n�s iniciais e finais de cada elemento [n�i, n�f]
    ! CNOS: posi��es x e y de cada n� [pos_x, pos_y]
    ! MEL: vetor que armazena m�dulo de elasticidade de cada elemento em MPa
    ! LAR: vetor que armazena largura de cada elemento em metros
    ! ALT: vetor que armazena altura de cada elemento em metros
    ! RIGS: matriz 3D para armazenar matriz de rigidez local de cada elemento

    ! i: vari�vel para ser iterada
    ! area: vetor que armazena �rea de cada elemento em m�
    ! iner: vetor que armazena momento de in�rcia de cada elemento em m^4
    ! comp: vetor que armazena comprimento de cada elemento em m
    ! proj: matriz que armazena proje��es de cada elemento sobre o eixo x e y em m [proj_x, proj_y]
    ! aux1: vari�vel para montagem de matriz de rigidez local
    ! aux2: vari�vel para montagem de matriz de rigidez local
    ! aux3: vari�vel para montagem de matriz de rigidez local
    ! aux4: vari�vel para montagem de matriz de rigidez local
    ! aux5: vari�vel para montagem de matriz de rigidez local

    ! Rotina para alocar vetores e matrizes
    ALLOCATE(area(NELE))
    ALLOCATE(iner(NELE))
    ALLOCATE(proj(NELE,2))
    ALLOCATE(comp(NELE))

    RIGS = 0.0              ! Iniciar matriz 3D com zeros

    DO i=1, NELE
        area(i) = LAR(i) * ALT(i)                                   ! �rea
        iner(i) = LAR(i) * ALT(i) ** 3 / 12                         ! Momento de in�rcia
        proj(i,1) = CNOS(CELE(i,2), 1) - CNOS(CELE(i,1), 1)         ! Proje��o sobre o eixo x
        proj(i,2) = CNOS(CELE(i,2), 2) - CNOS(CELE(i,1), 2)         ! Proje��o sobre o eixo y
        comp(i) = ((proj(i,1) ** 2) + (proj(i,2) ** 2)) ** 0.5      ! Comprimento

        aux1 = (MEL(i)) * area(i) / comp(i)                         ! EA/L
        aux2 = 12 * (MEL(i)) * iner(i) / (comp(i) ** 3)             ! 12EI/L^3
        aux3 = 6 * (MEL(i)) * iner(i) / (comp(i) ** 2)              ! 6EI/L^2
        aux4 = 4 * (MEL(i)) * iner(i) / comp(i)                     ! 4EI/L
        aux5 = 2 * (MEL(i)) * iner(i) / comp(i)                     ! 2EI/L

        ! In�cio da montagem da matriz de rigidez local de cada elemento

        ! Linha 1
        RIGS(1,1,i) = aux1
        RIGS(1,4,i) = - aux1

        ! Linha 2
        RIGS(2,2,i) = aux2
        RIGS(2,3,i) = aux3
        RIGS(2,5,i) = - aux2
        RIGS(2,6,i) = aux3

        ! Linha 3
        RIGS(3,2,i) = aux3
        RIGS(3,3,i) = aux4
        RIGS(3,5,i) = - aux3
        RIGS(3,6,i) = aux5

        ! Linha 4
        RIGS(4,1,i) = - aux1
        RIGS(4,4,i) = aux1

        ! Linha 5
        RIGS(5,2,i) = - aux2
        RIGS(5,3,i) = - aux3
        RIGS(5,5,i) = aux2
        RIGS(5,6,i) = - aux3

        ! Linha 6
        RIGS(6,2,i) = aux3
        RIGS(6,3,i) = aux5
        RIGS(6,5,i) = - aux3
        RIGS(6,6,i) = aux4

        ! Fim da montagem da matriz de rigidez local de cada elemento
    END DO
    RETURN
END SUBROUTINE MMRIGL