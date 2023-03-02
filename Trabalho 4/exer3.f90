program exer3
    implicit none
    integer :: N_0, i
    real(8) :: dt, lambda, t, T_total, N, dN
    !Obs: N de átomos definido como real para os cálculos, mas será convertido a inteiro na impressão
    
    open(unit = 1, file = "decai_in") !recebendo os valores
        read(1,*) T_total
        read(1,*) N_0
        read(1,*) dt
        read(1,*) lambda
    close(1)

    N=N_0
    t=0.0d0
    
    open(unit = 2, file="decai_out")
    
        write(2,*) t, int(N)
    
        do i = 1, nint(T_Total/dt) !Considerando T múltiplo de dt
    
            t = t + dt
            dN = -1.0d0 * lambda * N * dt
            N = N + dN
            
            write(2,*) t, nint(N) !inteiro mais próximo do resultado encontrado para o número de átomos
        end do

    close(2)
    
    end program exer3