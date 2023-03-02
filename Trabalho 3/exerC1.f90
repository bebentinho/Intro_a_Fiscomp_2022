program exerC1
implicit none
real(8), parameter :: pi = dacos(-1.d0), g = 9.80665d0 !valores de pi e gravidade com precisao dupla
real(8) :: theta_0, l, m, dt, T !ang inicial; compr da haste; massa; intervalo de tempo; tempo total
real(8) :: w_0 = 0.0d0, tempo = 0.d0 !freq inicial
real(8) :: theta, w, thetaaux, waux
integer :: Nlinhas, i
!Variáveis para o cálculo da energia
real(8) :: E_U, E_K, E_T !en potencial; en cinetica; en total

read(*,*) theta_0 !em graus!
read(*,*) l
read(*,*) m
read(*,*) dt
read(*,*) T

theta_0 = theta_0 * pi / 180.0d0

Nlinhas = int(T/dt)

open(unit = 1, file = "exerC1_out.dat") !para o gráfico theta vs t
open(unit = 2, file = "exerC1ener_out.dat") !arquivo para o gráfico da energia
!obs: apesar de não solicitado, achei melhor criar um arquivo para plotar o gráfico da energia.

write(1,*) 0.d0, theta_0

theta = theta_0
w = w_0

E_U = (1.d0/2.d0) * m * g * l * theta ** 2 ! kx^2/2
E_K = (1.d0/2.d0) * m * l**2 * w**2 !mv^2/2
E_T=E_U+E_K

write(2,*) 0.d0, E_T

do i = 1, Nlinhas

    waux = w
    thetaaux = theta

    tempo = tempo + dt

    w = w - (g/l) * thetaaux * dt

    if (theta + waux * dt > 1.d0 * pi) then
        theta = theta + waux * dt - 2.d0 * pi

    else if (theta + waux * dt < -1.d0 * pi) then
        theta = theta + waux * dt + 2.d0 * pi

    else
        theta = theta + waux * dt

    end if

    E_U = (1.d0/2.d0) * m * g * l * theta ** 2
    E_K = (1.d0/2.d0) * m * l**2 * w**2
    E_T=E_U+E_K

    write(1,*) tempo, theta
    write(2,*) tempo, E_T

end do

close(1)
close(2)

end program exerC1