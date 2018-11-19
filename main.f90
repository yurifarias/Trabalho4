PROGRAM Trabalho4
    IMPLICIT NONE

    ! Variáveis de entrada de dados
    INTEGER, PARAMETER :: f1 = 10, f2 = 11, f3 =12
    CHARACTER(len=50) :: titulo
    INTEGER :: num_elem, num_nos, i, j
    INTEGER, DIMENSION(:,:), ALLOCATABLE :: conf_ele
    REAL(kind=8), DIMENSION(:,:), ALLOCATABLE :: conf_nos, car_elem
    REAL(kind=8), DIMENSION(:), ALLOCATABLE :: car_noda, mod_elas, mas_espe, larg, altu
    LOGICAL, DIMENSION(:), ALLOCATABLE :: res_noda
    CHARACTER(len=1) :: tipo_mas

    ! Variáveis de processamento de dados
    CHARACTER(len=20) :: form
    REAL(kind=8), DIMENSION(:,:,:), ALLOCATABLE :: rigs_loc, mass_loc, mrot
    REAL(kind=8), DIMENSION(:,:), ALLOCATABLE :: esfo_loc, rig_estr, desl_loc, reac_loc, mas_estr
    REAL(kind=8), DIMENSION(:), ALLOCATABLE :: esfo_glo, desl_glo, freq
    INTEGER, DIMENSION(:), ALLOCATABLE :: vaux


    ! f1: unit do arquivo de entrada 'entrada.txt'
    ! f2: unit do arquivo de saída 'saida.txt'
    ! titulo: variável que armazena o título do trabalho
    ! num_elem: variável que armazena número de elementos
    ! num_nos: variável que armazena número de nós
    ! i: variável para iterar
    ! j: variável para iterar
    ! conf_ele: matriz que armazena índices dos nós iniciais e finais de cada elemento [nói,nóf]
    ! conf_nos: posições x e y de cada nó [pos_x,pos_y]
    ! car_elem: matriz que armazena carregamentos locais de cada elemento em kN/m [Nli,Nlf,Qli,Qlf]
    ! car_noda: matriz que armazena cargas locais em kN/kNm
    ! mod_elas: vetor que armazena módulo de elasticidade de cada elemento em MPa
    ! larg: vetor que armazena largura de cada elemento em metros
    ! altu: vetor que armazena altura de cada elemento em metros
    ! res_noda: vetor que armazena restrições em cada nó em TRUE/FALSE

    ! form: variável para armazenar formats variáveis
    ! rigs_loc: matriz 3D que armazena matriz de rigidez local de cada elemento [6,6,num_elem]
    ! mrot: matriz 3D que armazena matriz de rotação de cada elemento [6,6,num_elem]
    ! esfo_loc: matriz 2D que armazena vetor de esforço local em cada elemento [6,num_elem]
    ! rig_estr: matriz de rigidez global da estrutura [3*num_nos,3*num_nos]
    ! mas_estr: matriz de massa global da estrutura [3*num_nos,3*num_nos]
    ! esfo_glo: vetor de esforços globais na estrutura {3*num_nos}
    ! desl_glo: vetor de deslocamentos globais na estrutura {3*num_nos}

    ! INÍCIO DE ROTINAS PARA LEITURA DOS DADOS DE ENTRADA

    ! Rotina para abrir arquivos de entrada e de saída
    OPEN(f1,file='entrada.txt')
    OPEN(f2,file='saida.txt')
    OPEN(f3,file='autorespostas.txt')

    ! Rotina para pegar título do trabalho
    READ(f1, '(a)') titulo      ! Linha 01
    WRITE(f2, '(a)') titulo

    ! Rotina para pegar número de elementos
    READ(f1, *)                 ! Linha 02
    READ(f1, *)                 ! Linha 03
    READ(f1, *) num_elem        ! Linha 04

    ! Rotina para alocar matrizes
    ALLOCATE(conf_ele(num_elem, 2))         ! [nói, nóf]
    ALLOCATE(car_elem(num_elem, 4))         ! [Nli, Nlf, Qli, Qlf]
    ALLOCATE(mod_elas(num_elem))
    ALLOCATE(mas_espe(num_elem))
    ALLOCATE(larg(num_elem))
    ALLOCATE(altu(num_elem))

    ! Rotina para pegar dados dos elementos
    READ(f1, *)                 ! Linha 05
    READ(f1, *)                 ! Linha 06
    READ(f1, *)                 ! Linha 07
    DO i=1, num_elem
        READ(f1, *) j, conf_ele(j,1), conf_ele(j,2), mod_elas(j), mas_espe(j), larg(j), &
                & altu(j), car_elem(j,1), car_elem(j,2), car_elem(j,3), car_elem(j,4)
    END DO                      ! Linha 08 a 07 + num_elem

    ! Rotina para pegar número de nós
    READ(f1, *)                 ! Linha 08 + num_elem
    READ(f1, *)                 ! Linha 09 + num_elem
    READ(f1, *) num_nos         ! Linha 10 + num_elem

    ! Rotina para alocar matrizes
    ALLOCATE(conf_nos(num_nos,2))
    ALLOCATE(car_noda(3*num_nos))
    ALLOCATE(res_noda(3*num_nos))

    ! Rotina para pegar dados dos nós
    READ(f1, *)                 ! Linha 11 + num_elem
    READ(f1, *)                 ! Linha 12 + num_elem
    READ(f1, *)                 ! Linha 13 + num_elem
    DO i=1, num_nos
        READ(f1, *) j, conf_nos(i,1), conf_nos(i,2), car_noda(3*i-2), car_noda(3*i-1), &
                & car_noda(3*i), res_noda(3*i-2), res_noda(3*i-1), res_noda(3*i)
    END DO                      ! Linha 14 + num_elem a 13 + num_elem + num_nos

    ! Rotina para definir o tipo de matriz de massa
    READ(f1, *)                 ! Linha 14 + num_elem + num_nos
    READ(f1, *)                 ! Linha 15 + num_elem + num_nos
    READ(f1, *) tipo_mas        ! Linha 16 + num_elem + num_nos

    ! FIM DE ROTINAS PARA LEITURA DOS DADOS DE ENTRADA

    ! INÍCIO DE ROTINAS PARA PROCESSAMENTO DE DADOS

    ! Conversões
    mod_elas = mod_elas * 1000000   ! MPa para N/m²
    car_elem = car_elem * 1000      ! kN/m para N/m
    car_noda = car_noda * 1000      ! kN para N e kN*m para N*m

    ! Montagem de matrizes de rigidez local
    ALLOCATE(rigs_loc(6,6,num_elem))
    CALL MMRIGL(num_elem,num_nos,conf_ele,conf_nos,larg,altu,mod_elas,rigs_loc)

    DO i=1, num_elem
        WRITE(f2, *)
        WRITE(f2, '(a36,i3)') 'MATRIZ DE RIGIDEZ LOCAL DO ELEMENTO ', i
        DO j=1, 6
            WRITE(f2, '(6f16.1)') rigs_loc(j,1,i), rigs_loc(j,2,i), rigs_loc(j,3,i), &
                    & rigs_loc(j,4,i), rigs_loc(j,5,i), rigs_loc(j,6,i)
        END DO
    END DO

    ! Montagem de matrizes de rotação
    ALLOCATE(mrot(6,6,num_elem))
    CALL MMROT(num_elem,num_nos,conf_ele,conf_nos,mrot)

    DO i=1, num_elem
        WRITE(f2, *)
        WRITE(f2, '(a36,i3)') 'MATRIZ DE ROTACAO LOCAL DO ELEMENTO ', i
        DO j=1, 6
            WRITE(f2, '(6f16.1)') mrot(j,1,i), mrot(j,2,i), mrot(j,3,i), &
                    & mrot(j,4,i), mrot(j,5,i), mrot(j,6,i)
        END DO
    END DO

    ! Montagem da matriz de rigidez global da estrutura
    ALLOCATE(rig_estr(3*num_nos,3*num_nos))
    CALL MMRIGG(num_elem,num_nos,conf_ele,rigs_loc,mrot,rig_estr)
    WRITE(form, '(a1,i4,a6)') '(',3*num_nos,'f16.3)'
    WRITE(f2, *)
    WRITE(f2, '(a)') 'MATRIZ DE RIGIDEZ GLOBAL DA ESTRUTURA'
    WRITE(f2, form) rig_estr

    ! Montagem de vetores de esforços locais
    ALLOCATE(esfo_loc(6,num_elem))
    CALL MVESFL(num_elem,num_nos,conf_ele,conf_nos,car_elem,esfo_loc)
    DO i=1, num_elem
        WRITE(f2, *)
        WRITE(f2, '(a37,i3)') 'VETOR DE ESFORCOS LOCAIS DO ELEMENTO ', i
        DO j=1, 6
            WRITE(f2, '(f16.1)') esfo_loc(j,i)
        END DO
    END DO

    ! Montagem do vetor de esforços globais
    ALLOCATE(esfo_glo(3*num_nos))
    CALL MVESFG(num_elem,num_nos,conf_ele,mrot,esfo_loc,car_noda,esfo_glo)
    WRITE(f2, *)
    WRITE(f2, '(a)') 'VETOR DE ESFORCOS GLOBAIS NA ESTRUTURA'
    WRITE(f2, '(f16.1)') esfo_glo

    ! Montar matriz de massa local de cada elemento
    ALLOCATE(mass_loc(6,6,num_elem))
    CALL MMMASL(num_elem,num_nos,conf_ele,conf_nos,larg,altu,mas_espe,tipo_mas,mass_loc)

    ! Montar matriz de massa global da estrutura
    ALLOCATE(mas_estr(3*num_nos,3*num_nos))
    CALL MMMASG(num_elem,num_nos,conf_ele,mass_loc,mrot,mas_estr)

    ! Inserir restrições
    CALL RESTR(num_nos,res_noda,rig_estr,mas_estr,esfo_glo)

    ! Cálculo da deslocamentos globais da estrutura
    ALLOCATE(desl_glo(3*num_nos))
    CALL DLSLRGa(3*num_nos,3*num_nos,rig_estr,esfo_glo,1,desl_glo)
    WRITE(f2, *)
    WRITE(f2, '(a)') 'VETOR DE DESLOCAMENTOS GLOBAIS NA ESTRUTURA'
    WRITE(f2, '(es16.3)') desl_glo

    ! Montar matriz 2D que armazena deslocamentos locais de cada elemento
    ALLOCATE(desl_loc(6,num_elem))
    CALL MVDESL(num_elem,num_nos,conf_ele,desl_glo,mrot,desl_loc)
    DO i=1, num_elem
        WRITE(f2, *)
        WRITE(f2, '(a41,i3)') 'VETOR DE DESLOCAMENTOS LOCAIS DO ELEMENTO', i
        DO j=1, 6
            WRITE(f2, '(es16.3)') desl_loc(j,i)
        END DO
    END DO

    ! Calcular reações locais em cada elemento
    ALLOCATE(reac_loc(6,num_elem))
    CALL CVREAL(num_elem,num_nos,conf_ele,esfo_loc,desl_loc,rigs_loc,reac_loc)
    DO i=1, num_elem
        WRITE(f2, *)
        WRITE(f2, '(a19,i3)') 'REACOES NO ELEMENTO', i
        WRITE(f2, '(a)') '                 Esf Normal (N) Esf Cortante (N) Mom Fletor (Nm)'
        WRITE(f2, '(a13,3es16.3)') 'No inicial:  ', reac_loc(1,i), reac_loc(2,i), reac_loc(3,i)
        WRITE(f2, '(a13,3es16.3)') 'No final:    ', reac_loc(4,i), reac_loc(5,i), reac_loc(6,i)
    END DO

    ALLOCATE(vaux(3*num_nos))
    WRITE(f2, *)
    DO i=1, 3*num_nos
        IF (res_noda(i)) THEN
            vaux(i) = 1
        ELSE
            vaux(i) = 0
        END IF
    END DO
    ALLOCATE(freq(10))
    CALL AUTOPROBLEMA(rig_estr,mas_estr,3*num_nos,f3,1,freq,vaux)

    ! FIM DE ROTINAS PARA PROCESSAMENTO DE DADOS

    CLOSE(f1)
    CLOSE(f2)
    CLOSE(f3)

END PROGRAM Trabalho4