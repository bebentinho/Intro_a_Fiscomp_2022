program exer2
implicit none
integer :: N_0, i, N
real(8) :: dt, lambda, t, prob
real(8), dimension(:), allocatable :: N_atomos, tm_atomos
real(8) :: tm_simulado, tm_exato !valores para os tempos exato e simulado

read(*,*) N_0
read(*,*) dt
read(*,*) lambda

tm_exato = 1.0d0/lambda !wolfram alpha

allocate(N_atomos(N_0))
allocate(tm_atomos(N_0))

do i=1,N_0

    N_atomos(i) = 1

end do

N=N_0
t=0.0d0
prob = lambda * dt
!probabilidade de decair. Se um numero aleatorio entre 0 e 1 for menor ou igual a prob, o atomo decai,
!se for maior, o atomo nao decai (probabilidade representa os casos efetivos, assim, se for igual a 0.6,
!por exemplo, significa que 6 de 10 casos sao efetivos, 4 nao)

do while(t + dt <= 10.0d0)

    t = t + dt

    do i = 1, N_0
        if (N_atomos(i) /= 0) then !separado em dois ifs para minimizar a qtd de vezes q se usa o rand()

            if (rand() <= prob) then !verificando se decaiu
                N_atomos(i) = 0
                tm_atomos(i) = t !atribuindo ao átomo o tempo vivido
                N = N - 1
            end if
            
        end if
    end do

end do

tm_simulado = (sum(tm_atomos) + N * 10.0d0)/N_0
!obs: o termo N*10 está associado aos átomos que sobraram da simulaçao, pois o comando da questao informa que
!N(t=10)=0, assim o instante de decaimento dos N átomos que sobraram é exatamente t=10.

write(*,*) tm_simulado, tm_exato

deallocate(N_atomos)
deallocate(tm_atomos)
end program exer2