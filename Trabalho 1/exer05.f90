program exer05
    implicit none
    real:: precisao, lambda = 0 !precisao real e autovalor (chute inicial=0)
    integer:: i,j,n !variaveis auxiliares, tamanho da matriz
    real, allocatable, dimension(:,:) :: M !matriz
    real, allocatable, dimension(:) :: x,y !vetor aleatorio, vetor auxiliar do loop


    read(*,*) precisao !erro da matriz


    read(*,*) n !tamanho da matriz
    allocate(M(n,n), x(n), y(n)) !alocando o tamanho da matriz


    do i=1,n !lendo os valores da matriz
        read(*,*) (M(i,j), j=1,n)
    end do
    

    do i=1,n !gerando o vetor aleatorio (1,2,3,4...n)
        x(i)=i
    end do


    y=matmul(M,x) !valor inicial de y (primeira iteração)

    do while(abs(lambda-dot_product(x,y)/dot_product(x,x)) > precisao)

        !se a diferença entre o valor atual e o próximo valor aproximado de lambda for
        !maior que a precisão escolhida, o loop rodará mais uma iteração; caso contrário,
        !isto é, se a diferença for menor que a precisão, o loop quebrará.

        lambda=dot_product(x,y)/dot_product(x,x) !substituindo o valor aproximado do autovalor
        x=y !substituindo o valor de x pelo resultado da multiplicação da iteração atual

        y=matmul(M,x)!y=vetor resultante da matriz X vetor x atual (inicialmente era o aleatório):
        !alterando o valor de y para a PRÓXIMA iteração, cujo resultado só será utilizado se a
        !condição do loop for satisfeita.

    end do


    write(*,*) lambda !printando autovalor

    do i=1,n
        write(*,*) x(i) !printando uma posição do autovetor por linha
    end do


    deallocate(M,x,y) !dealocando a memoria
end program exer05