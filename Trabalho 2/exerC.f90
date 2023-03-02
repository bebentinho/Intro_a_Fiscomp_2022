program exerC
implicit none
integer :: nITE !numero de iteraçoes
real(8) :: f, df
real(8) :: bd1,bd2,bd3,nr1,nr2,nr3,sx01,sx02,sx03,sx11,sx12,sx13
real(8) :: variacao !variacao na busca direta
real(8) :: secaux1,secaux2,secaux3 !valores auxiliares para o metodo das secantes
integer :: i

read(*,*) nITE

!chutes (para cada raiz, é necessário um x0 e um x1)
!os chutes são valores razoáveis, que podem ser obtidos visualmente por meio do gráfico.

!busca direta
bd1 = -1.4d0
bd2 = 0.2d0
bd3 = 1.6d0
variacao = 0.2d0

!newton-raphson
nr1 = -1.4d0
nr2 = 0.2d0
nr3 = 1.6d0

!secante
sx01 = -1.4d0
sx02 = 0.2d0
sx03 = 1.6d0

sx11 = -1.0d0
sx12 = 0.6d0
sx13 = 2.0d0

open(unit=1, file="tabC_out.dat")
write(1,*) "iter","dir1","dir2","dir3","NR1","NR2","NR3","sec1","sec2","sec3"

do i=1,nITE !loop com número de iterações

    !busca direta
    !todos os x0 estão antes da raiz. note que para raiz 1, f(r1)<0, para 2, f(r2)>0 e f(r3)<0
    if (f(bd1 - variacao) > 0) then !quando f(x) troca o sinal, a busca acaba
       bd1 = bd1 - variacao
    end if
  
    if (f(bd2 + variacao) < 0) then
    bd2 = bd2 + variacao
    end if

    if (f(bd3 + variacao) > 0) then
    bd3 = bd3 + variacao
    end if

    !newton-raphson
    nr1 = nr1 - (f(nr1)/df(nr1))
    nr2 = nr2 - (f(nr2)/df(nr2))
    nr3 = nr3 - (f(nr3)/df(nr3))

    !secante
    secaux1 = sx01 - f(sx01)*((sx01 - sx11)/(f(sx01) - f(sx11)))
    sx11 = sx01
    sx01 = secaux1

    secaux2 = sx02 - f(sx02)*((sx02 - sx12)/(f(sx02) - f(sx12)))
    sx12 = sx02
    sx02 = secaux2

    secaux3 = sx03 - f(sx03)*((sx03 - sx13)/(f(sx03) - f(sx13)))
    sx13 = sx03
    sx03 = secaux3

    write(1,*) i, bd1, bd2, bd3, nr1, nr2, nr3, sx01, sx02, sx03
end do

close(1)

end program exerC

real(8) function f(x)
    real(8) :: x

    f = x**3 - x**2 - 2.0d0*x + 1.0d0

end function f

real(8) function df(x)
    real(8) :: x

    df = 3.0d0*x**2 - 2.0d0*x - 2.0d0

end function df