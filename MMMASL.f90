SUBROUTINE MMMASL(NELE, NNOS, CELE, CNOS, LAR, ALT, MESP, TIPO, MASL)
    ! Subrotina MMMASL: Montar Matriz de MASsa Local de cada elemento
    IMPLICIT NONE

    CHARACTER(len=1), INTENT(in) :: TIPO
    INTEGER, INTENT(in) :: NELE, NNOS
    INTEGER, DIMENSION(NELE,2), INTENT(in) :: CELE
    REAL(kind=8), DIMENSION(NNOS,2), INTENT(in) :: CNOS
    REAL(kind=8), DIMENSION(NELE), INTENT(in) :: LAR, ALT, MESP
    REAL(kind=8), DIMENSION(6,6,NELE), INTENT(out) :: MASL

    INTEGER :: i
    REAL(kind=8) :: aux1, aux2, aux3, aux4, aux5, aux6, aux7
    REAL(kind=8), DIMENSION(NELE) :: area, iner, comp
    REAL(kind=8), DIMENSION(NELE,2) :: proj

    ! NELE: vari�vel que armazena n�mero de elementos
    ! NNOS: vari�vel que armazena n�mero de n�s
    ! CELE: matriz que armazena �ndices dos n�s iniciais e finais de cada elemento [n�i,n�f]
    ! CNOS: posi��es x e y de cada n� [pos_x,pos_y]
    ! MESP: vetor que armazena massa espec�fica de cada elemento em kg/m�
    ! LAR: vetor que armazena largura de cada elemento em metros
    ! ALT: vetor que armazena altura de cada elemento em metros
    ! MASL: matriz 3D para armazenar matriz de massa local de cada elemento

    ! i: vari�vel para ser iterada
    ! area: vetor que armazena �rea de cada elemento em m�
    ! iner: vetor que armazena momento de in�rcia de cada elemento em m^4
    ! comp: vetor que armazena comprimento de cada elemento em m
    ! proj: matriz que armazena proje��es de cada elemento sobre o eixo x e y em m [proj_x,proj_y]
    ! aux1: vari�vel para montagem de matriz de massa consistente local
    ! aux2: vari�vel para montagem de matriz de massa consistente local
    ! aux3: vari�vel para montagem de matriz de massa consistente local
    ! aux4: vari�vel para montagem de matriz de massa consistente local
    ! aux5: vari�vel para montagem de matriz de massa consistente local
    ! aux6: vari�vel para montagem de matriz de massa Lumped local
    ! aux7: vari�vel para montagem de matriz de massa Lumped local


    MASL = 0.0              ! Iniciar matriz 3D com zeros

    IF (TIPO=='C') THEN
        DO i=1, NELE
            area(i) = LAR(i) * ALT(i)                                   ! �rea
            proj(i,1) = CNOS(CELE(i,2), 1) - CNOS(CELE(i,1), 1)         ! Proje��o sobre o eixo x
            proj(i,2) = CNOS(CELE(i,2), 2) - CNOS(CELE(i,1), 2)         ! Proje��o sobre o eixo y
            comp(i) = ((proj(i,1) ** 2) + (proj(i,2) ** 2)) ** 0.5      ! Comprimento

            aux1 = (area(i) * MESP(i) * comp(i)) / 420                  ! A*y*L/420
            aux2 = 22 * comp(i)                                         ! 22*L
            aux3 = 13 * comp(i)                                         ! 13*L
            aux4 = 4 * comp(i) ** 2                                     ! 4*L�
            aux5 = -3 * comp(i) ** 2                                    ! -3*L�

            ! In�cio da montagem da matriz de massa consistente local de cada elemento

            ! Linha 1
            MASL(1,1,i) = aux1 * 140
            MASL(1,4,i) = aux1 * 70

            ! Linha 2
            MASL(2,2,i) = aux1 * 156
            MASL(2,3,i) = aux1 * (- aux2)
            MASL(2,5,i) = aux1 * 54
            MASL(2,6,i) = aux1 * aux3

            ! Linha 3
            MASL(3,2,i) = aux1 * (- aux2)
            MASL(3,3,i) = aux1 * aux4
            MASL(3,5,i) = aux1 * (- aux3)
            MASL(3,6,i) = aux1 * aux5

            ! Linha 4
            MASL(4,1,i) = aux1 * 70
            MASL(4,4,i) = aux1 * 140

            ! Linha 5
            MASL(5,2,i) = aux1 * 54
            MASL(5,3,i) = aux1 * (- aux3)
            MASL(5,5,i) = aux1 * 156
            MASL(5,6,i) = aux1 * aux2

            ! Linha 6
            MASL(6,2,i) = aux1 * aux3
            MASL(6,3,i) = aux1 * aux5
            MASL(6,5,i) = aux1 * aux2
            MASL(6,6,i) = aux1 * aux4

            ! Fim da montagem da matriz de massa consistente local de cada elemento
        END DO
    ELSE IF (TIPO=='L') THEN
        DO i=1, NELE
            area(i) = LAR(i) * ALT(i)                                   ! �rea
            iner(i) = LAR(i) * ALT(i) ** 3 / 12                         ! Momento de in�rcia
            proj(i,1) = CNOS(CELE(i,2), 1) - CNOS(CELE(i,1), 1)         ! Proje��o sobre o eixo x
            proj(i,2) = CNOS(CELE(i,2), 2) - CNOS(CELE(i,1), 2)         ! Proje��o sobre o eixo y
            comp(i) = ((proj(i,1) ** 2) + (proj(i,2) ** 2)) ** 0.5      ! Comprimento

            aux6 = (area(i) * MESP(i) * comp(i)) / 2                    ! A*y*L/2
            aux7 = aux6*((iner(i)/(area(i)))+((comp(i)**2)/12))         ! A*y*L/2*((I/A)+(L�/12))

            ! In�cio da montagem da matriz de massa Lumped local de cada elemento

            MASL(1,1,i) = aux6
            MASL(2,2,i) = aux6
            MASL(3,3,i) = aux7
            MASL(4,4,i) = aux6
            MASL(5,5,i) = aux6
            MASL(6,6,i) = aux7

            ! Fim da montagem da matriz de massa Lumped local de cada elemento
        END DO
    END IF
    RETURN
END SUBROUTINE MMMASL