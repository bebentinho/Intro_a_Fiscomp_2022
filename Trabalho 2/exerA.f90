program exerA
implicit none
real(8), parameter:: x = 1.0d0/3.0d0 !parâmetro x constante
integer :: tamh, i !tamanho do vetor h; aux i
real(8), allocatable, dimension(:) :: h !vetor com os valores de h
real(8), dimension(3) :: de !vetor contendo os valores das derivadas exatas
real(8) :: derivada1, derivada2, derivada3, ds3p, df2p, dt2p, d2s3p, d2s5p, d3as5p
character (len=30) :: fmt

fmt="(F16.13, 6(ES16.8))"

de=(/derivada1(x, 0, 0.0d0),derivada2(x, 0, 0.0d0),derivada3(x, 0, 0.0d0)/) !vetor derivada exata para o valor de x=1/3


open(unit=1,file="tabA_in.dat") !abrindo arquivo para ler os valores de h

read(1,*) tamh !lendo o tamanho do vetor
allocate(h(tamh)) !alocando o tamanho do vetor
read(1,*) h

close(1)


open(unit=2,file="tabA_out.dat") !abrindo arquivo para escrever a tabela

write(2,*) "        h       ","      DS3P      ","      DF2P      ","      DT2P      ", &
"      D2S3P     ","      D2S5P     ","     D3AS5P     "

do i=1,tamh !fmt no write
    write(2,fmt) (h(i)), abs(ds3p(x,h(i)) - de(1)), abs(df2p(x,h(i)) - de(1)), abs(dt2p(x,h(i)) - de(1)), &
    abs(d2s3p(x,h(i)) - de(2)), abs(d2s5p(x,h(i)) - de(2)), abs(d3as5p(x,h(i)) - de(3))
end do

close(2)

deallocate(h) !dealocando memória dos vetores

print*, "Assim, conclui-se que não necessariamente existe um valor de h mais adequado, e nem um tipo de"
print*, "derivação mais eficaz. A escolha deve ser feita de acordo com a precisão escolhida e de acordo com"
print*, "o esforço computacional disponível. Observe que cada derivada possui um valor de h que maximiza sua"
print*, "precisão, de forma que em algumas derivadas um mesmo h fornece desvios da ordem de E-10 e E+2. Assim,"
print*, "as escolhas da derivada e do h utilizado devem ser feitas de forma a equilibrar todos os interesses"
print*, "desejados e ainda assim minimizar o máximo possível o desvio. Cabe apontar que, aparentemente, a pre-"
print*, "cisão dessas derivadas é um gŕafico com um pico, um valor máximo, e não crescente com h."

end program exerA

!==========================================================================================
!definindo funções que serão utilizadas

function expcos(x,n,h) !definindo a função para o X fixo, mas com n e h variáveis
    integer :: n !índice dos termos de Taylor usados
    real(8) :: h !valor real de h, que será escolhido por meio do documento lido
    real(8) :: x !valor de x (será parâmetro no programa)
    real(8) :: expcos

    expcos = exp(4.0d0*(x+n*h))*cos((x+n*h)/2.0d0) !função retorna o valor para n e h especificados

end function expcos

real(8) function expsin(x,n,h) !função que será usada nas derivadas exatas
    integer :: n
    real(8) :: h
    real(8) :: x

    expsin = exp(4.0d0*(x+n*h))*sin((x+n*h)/2.0d0) !expressão da função (para facilitar, apenas)

end function expsin

function derivada1(x,n,h) !função para retornar a derivada primeira exata
    integer :: n
    real(8) :: h
    real(8) :: x
    real(8) :: derivada1,expcos,expsin

    derivada1 = 4.0d0*expcos(x,n,h) - expsin(x,n,h)/2.0d0

end function derivada1

function derivada2(x,n,h) !função para retornar a derivada segunda exata
    integer :: n
    real(8) :: h
    real(8) :: x
    real(8) :: derivada2, expcos, expsin

    derivada2 = (63.0d0 * expcos(x,n,h) - 16.0d0 * expsin(x,n,h))/4.0d0

end function derivada2

function derivada3(x,n,h)
    integer :: n
    real(8) :: h
    real(8) :: x
    real(8) :: derivada3, expcos, expsin

    derivada3 = (488.0d0 * expcos(x,n,h) - 191.0d0 * expsin(x,n,h))/8.0d0
end function derivada3

!==========================================================================================
!definindo as derivadas numéricas:

function df2p(x,h) !derivada para frente de 2 pontos
    real(8) :: x,h
    real(8) :: df2p, expcos

    df2p = (expcos(x,1,h)-expcos(x,0,h))/h

end function df2p

function dt2p(x,h) !derivada para trás de 2 pontos
    real(8) :: x,h
    real(8) :: dt2p, expcos

    dt2p = (expcos(x,0,h)-expcos(x,-1,h))/h

end function dt2p

function ds3p(x,h) !derivada simétrica de 3 pontos
    real(8) :: x,h
    real(8) :: ds3p, expcos

    ds3p = (expcos(x,1,h)-expcos(x,-1,h))/(2.0d0*h)

end function ds3p

real(8) function ds5p(x,h) !derivada simétrica de 5 pontos
    real(8) :: x,h
    real(8) :: expcos

    ds5p = (-1.0d0*expcos(x,2,h)+8.0d0*expcos(x,1,h)-8*expcos(x,-1,h)+expcos(x,-2,h))/(12.0d0*h)

end function ds5p

real(8) function d2s3p(x,h) !derivada segunda simétrica de 3 pontos
    real(8) :: x,h, expcos

    d2s3p = (expcos(x,1,h)-2.0d0*expcos(x,0,h)+expcos(x,-1,h))/(h**2)

end function d2s3p

real(8) function d2s5p(x,h) !derivada segunda simétrica de 5 pontos
    real(8) :: x,h, expcos

    d2s5p = (-1*expcos(x,2,h) + 16.0d0 * expcos(x,1,h) - 30.0d0 * expcos(x,0,h) + 16.0d0 * expcos(x,-1,h)&
    - expcos(x,-2,h))/(12.0d0*h**2)

end function d2s5p

real(8) function d3as5p(x,h) !derivada terceira anti-simétrica de 5 pontos
    real(8) :: x,h, expcos

    d3as5p = (expcos(x,2,h)-2.0d0*expcos(x,1,h)+2.0d0*expcos(x,-1,h)-1.0d0*expcos(x,-2,h))/(2.0d0*h**3)

end function d3as5p