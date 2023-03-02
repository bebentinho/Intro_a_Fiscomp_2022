program exerB
implicit none
real(8), parameter :: intExata = -2.0d0/3.0d0 + cos(1.0d0/2.0d0) - cos(3.0d0/2.0d0)/3.0d0
real(8), parameter :: x = 0.0d0
real(8) :: trapezio, simpson, bode
real(8), dimension(:), allocatable :: valoresh
integer, dimension(:), allocatable :: valoresN
real(8), dimension(:), allocatable:: intTrap, intSimp, intBode
real(8) :: valintTrap, valintSimp, valintBode
integer :: N, i, j
character (len=30) :: fmt 

fmt="(I6,F16.13,5(ES16.8))"


open(unit=1, file="tabB_in.dat")
    read(1,*) N
    allocate(valoresN(N))
    allocate(valoresh(N),intSimp(N),intTrap(N),intBode(N))
    read(1,*) valoresN
close(1)


do i=1,N
    valoresh(i) = 1.0d0 / valoresN(i)
end do


do i=1,N !loop para cada valor de h
    valintTrap = 0.0d0 !setando os valores das integrais como 0
    valintSimp = 0.0d0
    valintBode = 0.0d0

    !o loop seria de 0 a 1, subindo de 2h em 2h. Isso equivale a um loop de 0 a 1/h, subindo de 2 em 2.
    !h=(1-0)/N portanto N=1/h. substitui-se no loop pois ele só assume inteiros.

    do j=1, valoresN(i)-1, 2
        valintTrap = valintTrap + trapezio(x,j, valoresh(i))
    end do

    intTrap(i) = valintTrap

    do j=1, valoresN(i)-1,2
        valintSimp = valintSimp + simpson(x,j, valoresh(i))
    end do

    intSimp(i) = valintSimp

    do j=0, valoresN(i)-4, 4
        valintBode = valintBode + bode(x,j, valoresh(i))
    end do

    intBode(i) = valintBode

end do


open(unit=2,file="tabB_out.dat")

write(2,*) "   N  ", "        h       ", "    Trapézio    ", "     Simpson    ", "      Bode      "

do i=1,N

    write(2,fmt) valoresN(i), valoresh(i), abs(intTrap(i) - intExata),&
    abs(intSimp(i) - intExata), abs(intBode(i) - intExata)

end do

close(2)

deallocate(valoresN,valoresh,intTrap,intSimp,intBode)

print*, "Observe que os métodos do Trapézio e de Simpson aproximam intervalos de tamanho 2*h, enquanto o método"
print*, "de Bode aproxima intervalos de 4h. No entanto, a aproximação de Bode utiliza mais pontos no intervalo"
print*, "analisado. Apesar de ser plausível pensar que mais pontos necessariamente produzirão resultados mais"
print*, "precisos, mas, na verdade, isso depende de como a interpolação polinomial da função se comporta quando"
print*, "o número de pontos é aumentado, podendo implicar ou não em convergência para o valor exato da integral."
print*, "Assim, novamente, mostra-se que os resultados numéricos e outras condições devem indicar qual o método"
print*, "mais preciso para aquela função. No entanto, N não representa o número de pontos utilizados em um"
print*, "intervalo, mas sim a quantidade de subintervalos de integração que serão utilizados. É razoável pensar"
print*, "que quanto maior o número desses subintervalos, melhor o resultado obtido para a integral. Porém, nova-"
print*, "mente por causa do comportamento de convergência da interpolação polinomial, isso não necessariamente"
print*, "pode ser afirmado: note, nos casos do método do Trapézio e de Simpson, que utilizam menos pontos, o com-"
print*, "portamento da precisão demonstra-se, até então, uniforme: a precisão aumenta com o valor de N. No entanto,"
print*, "no caso de Bode, com muitos pontos, o valor da precisão começou crescendo mas, depois, passou a flutuar e"
print*, "até diminuir. Assim, o melhor valor de N depende do método utilizado e das limitações computacionais impostas,"
print*, "visto que a flutuação nos valores finais do método de Bode podem estar vinculadas a isso."

end program exerB


real(8) function f(x,n,h)
    integer :: n
    real(8) :: x, h
    real(8) :: x0

    x0 = x + n*h
    
    f = sin((x0)/2.0d0)*cos(x0)

end function f


real(8) function trapezio(x,j0,h)
    real(8) :: x, h, f
    integer :: j0

    trapezio = (h/ 2.0d0) * ( f(x, 1+j0, h) + 2.0d0 * f(x, j0, h) + f(x, j0-1, h) )

end function trapezio


real(8) function simpson(x,j0,h)
    real(8) :: x, h, f
    integer :: j0

    simpson = (h / 3.0d0) * ( f(x, 1+j0, h) + 4.0d0 * f(x, j0, h) + f(x, j0-1, h) )

end function simpson


real(8) function bode(x,j0,h)
    real(8) :: x, h, f
    integer :: j0

    bode = (2.0d0 * h / 45.0d0) * ( 7.0d0 * f(x, j0, h) + 32.0d0 * f(x, 1+j0, h) + 12.0d0 * f(x, 2+j0, h)&
    + 32.0d0 * f(x, 3+j0, h) + 7.0d0 * f(x, 4+j0, h) )

end function bode