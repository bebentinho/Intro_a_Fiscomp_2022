program exerA
implicit none
real(8) :: y, y0, yantes
real(8) :: x, x0, xantes
real(8) :: r, v0, dt, vx, vy, t
real(8), parameter :: pi = 4.0d0 * atan(1.d0)
integer :: i
character(16), dimension(9) :: planetas
real(8), dimension(9) :: raios
logical :: aux

read(*,*) r
read(*,*) v0
read(*,*) dt

x0 = r
y0 = 0.0d0
vx = 0.0d0
vy = v0
t = 0.0d0

open(unit = 1, file = "trajA1_out.dat")

write(1,*) "           t           ", "            x(t)           ", "           y(t)" !cabeçalho
write(1,*)  0.0d0, x0, y0

y = y0 + vy * dt !definindo yi
x = x0 + vx * dt !definindo xi

yantes = y0 !definindo y_(i-1)
xantes = x0 !definindo x_(i-1)

aux = .true.

do while(aux .eqv. .true.)

    r = sqrt( x**2 + y**2 ) 
    t = t + dt

    y0 = y !salvando a variavel y_(i-1) da proxima iteração
    y = 2 * y - yantes - (4.0d0 * pi**2 / r**3) * y * dt**2 !calculando y_(i+1) com o y_(i-1) desta iteração
    yantes = y0 !y_(i-1) = variavel salva

    x0 = x !repetindo o procedimento para x
    x = 2 * x - xantes - (4.0d0 * pi**2 / r**3) * x * dt**2
    xantes = x0

    if ((y > 0) .and. (yantes < 0)) then !completando uma órbita
        aux = .false.
    end if

    write(1,*) t, x, y

end do

close(1)

!=======================================================================================================

!Escrevendo a tabela tabA1_out.dat:

raios = (/0.39d0, 0.72d0, 1.00d0, 1.52d0, 5.20d0, 9.24d0, 19.19d0, 30.06d0, 39.53d0/)
planetas = (/"Mercurio        ", "Venus           ", "Terra           ", "Marte           ", &
"Jupiter         ", "Saturno         ", "Urano           ", "Netuno          ", "Plutao          "/)

open(unit = 2, file = "tabA1_out.dat")
write(2,*) "(Planeta)                 (v0)                     (T2/R3)"
do i = 1,9
    !repetindo o procedimento para cada planeta
    x0 = raios(i)
    y0 = 0.0d0
    vx = 0.0d0
    v0 = sqrt(4.d0 * pi**2 / raios(i))
    vy = v0
    t = 0.0d0

    y = y0 + vy * dt 
    x = x0 + vx * dt 

    yantes = y0 
    xantes = x0 

    aux = .true.

    do while(aux .eqv. .true.)

        r = sqrt( x**2 + y**2 ) 
        t = t + dt
    
        y0 = y 
        y = 2 * y - yantes - (4.0d0 * pi**2 / r**3) * y * dt**2
        yantes = y0
    
        x0 = x
        x = 2 * x - xantes - (4.0d0 * pi**2 / r**3) * x * dt**2
        xantes = x0
    
        if ((y > 0) .and. (yantes < 0)) then
            aux = .false.
        end if
    
    end do

    write(2,*) planetas(i), v0, (t**2/(raios(i))**3)

end do

close(2)

!=============================================================================================================

!respondendo a questão

write(*,*) "A escolha do dt muda diretamente o comportamento da órbita do planeta. Mesmo com valores que devessem"
write(*,*) "fazer uma órbita elíptica, se o dt for muito grande (apresentar ordens de grandeza comparáveis com os"
write(*,*) "valores de r e v0), a trajetória será degenerada a uma reta. Além disso, é possível observar que exis-"
write(*,*) "tem valores de dt suficientes para que a trajetória seja observada, mas, ao permitir que o código seja"
write(*,*) "executado ao longo de vários períodos (e não só um), nota-se que a trajetória está instável, isto é,"
write(*,*) "as posições periódicas não condizem perfeitamente (o gŕafico torna-se uma elipse grossa). Novamente, isso"
write(*,*) "ocorre porque a ordem de grandeza do dt**2 é comparável ao termo que o multiplica no método de Verlet."
write(*,*) "Além disso, se o valor de dt for muito pequeno (o quão pequeno depende da precisão adotada no programa),"
write(*,*) "o termo dt**2 pode se igualar a zero, e as iterações para a trajetória não funcionarão como deveriam."
write(*,*) "Mais detalhadamente, se o dt for menor que 10^(-16) (precisão dupla), a trajetória é degenerada por uma"
write(*,*) "reta vertical. Isso só ocorre pois a primeira iteração é feita utilizando o método de Euler-Cromer, cri-"
write(*,*) "ando valores de y_(i+1) e y_i não nulos. Assim, as iterações de Verlet ocorrem desprezando o termo de se-"
write(*,*) "gunda ordem. No entanto, no eixo x, a iteração de Euler-Cromer não altera os valores de x_(i+1) e de x_i,"
write(*,*) "assim, as iterações de Verlet mantém x=r. Dessa forma, nota-se, novamente, que o valor de dt deve ser ado-"
write(*,*) "tado de forma consciente e não aleatória, apresentando coerência com os outros valores de r e v0 escolhidos."

end program exerA