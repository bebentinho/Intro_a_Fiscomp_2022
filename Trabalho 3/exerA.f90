program exerA
implicit none
real(8) :: m = 80.0d0, P = 400.0d0, tempo = 0.0d0 !massa bike+cicl; potencia; tempo decorrido
real(8) :: T, dt, v0 !tempo total; variacao de tempo; velocidade inicial
integer :: Nlinhas, i

read(*,*) T, dt, v0 !lendo o valor das variáveis

Nlinhas = int(T/dt)

open(unit = 1, file="velA_out.dat")

    write(1,*) 0.0d0, v0

    do i = 1, Nlinhas

        tempo = tempo + dt

        v0 = v0 + (P/(m*v0))*dt

        write(1,*) tempo, v0

    end do

close(1)

end program exerA