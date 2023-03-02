program exer1
implicit none
integer :: N_0, i, N
real(8) :: dt, lambda, t, prob
real(8), dimension(:), allocatable :: N_atomos

read(*,*) N_0
read(*,*) dt
read(*,*) lambda

allocate(N_atomos(N_0))

do i=1,N_0

    N_atomos(i) = 1

end do

N=N_0
t=0.0d0
prob = lambda * dt
!probabilidade de decair. Se um numero aleatorio entre 0 e 1 for menor ou igual a prob, o atomo decai,
!se for maior, o atomo nao decai (probabilidade representa os casos efetivos, assim, se for igual a 0.6,
!por exemplo, significa que 6 de 10 casos sao efetivos, 4 nao)

open(unit = 1, file="decai_out")

    write(1,*) t, N

    do while(t + dt <= 10.0d0)

        t = t + dt

        do i = 1, N_0
            if (N_atomos(i) /= 0) then !separado em dois ifs para minimizar a qtd de vezes q se usa o rand()

                if (rand() <= prob) then !verificando se decaiu
                    N_atomos(i) = 0
                    N = N - 1
                end if
                
            end if
        end do

        write(1,*) t, N
    end do

close(1)

deallocate(N_atomos)
end program exer1