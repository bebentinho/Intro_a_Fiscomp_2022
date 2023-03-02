program exer02
real(4),dimension(4):: xSimples !vetor contendo os valores de x; precisao simples
real(8),dimension(4):: xDupla !vetor contendo os valores de x; precisao dupla
!variaveis auxiliares utilizadas
real(4)::y1,z1
real(8):: y2,z2
integer:: c1,c2

!definindo os vetores com os valores de x (nas precisoes corretas)

xSimples=(/0.1e0,0.2e0,0.3e0,0.4e0/)
xDupla=(/0.1d0,0.2d0,0.3d0,0.4d0/)

!=================================================
!taylor começa a rodar com y=0.1,0.2,0.3 ou 0.4 (nesse caso, 0.1)
y1=xSimples(1) !senx~x...

c1=1 !constante que define o sinal do termo
c2=3 !valor do primeiro fatorial (começa no 3)

do while( y1+((-1)**c1)*(xSimples(1)**(c2))/fact(c2) /= y1 ) !se y+z=y, z é menor que a precisao
    y1=y1+((-1)**c1)*(xSimples(1)**(c2))/fact(c2)
    c2=c2+2 !fatorial (cresce de 2 em 2; 3,5,7...)
    c1=c1+1 !constante que altera o sinal do termo
enddo

!calculando a precisao:
z1=(((-1)**(c1-1))*(xSimples(1)**(c2-2))/fact(c2-2))/y1 !precisa pegar c1 e c2 da iteração anterior

!repetindo o procedimento para a outra precisão, ainda 0.1
y2=xDupla(1)
c1=1
c2=3

do while( y2/=y2+((-1)**c1)*(xDupla(1)**(c2))/fact(c2) )
    y2=y2+((-1)**c1)*(xDupla(1)**(c2))/fact(c2)
    c2=c2+2
    c1=c1+1
enddo

z2=abs((((-1)**(c1-1))*(xDupla(1)**(c2-2))/fact(c2-2))/y2)

write(*,*) "0.1",z1,z2

!=================================================
!repetindo todo o procedimento para 0.2
y1=xSimples(2)
c1=1
c2=3

do while( y1/=y1+((-1)**c1)*(xSimples(2)**(c2))/fact(c2) )
    y1=y1+((-1)**c1)*(xSimples(2)**(c2))/fact(c2)
    c2=c2+2
    c1=c1+1
enddo

z1=abs((((-1)**(c1-1))*(xSimples(2)**(c2-2))/fact(c2-2))/y1)

y2=xDupla(2)
c1=1
c2=3

do while( y2/=y2+((-1)**(c1))*(xDupla(2)**(c2))/fact(c2) )
    y2=y2+((-1)**c1)*(xDupla(2)**(c2))/fact(c2)
    c2=c2+2
    c1=c1+1
enddo

z2=abs((((-1)**(c1-1))*(xDupla(2)**(c2-2))/fact(c2-2))/y2)

write(*,*) "0.2",z1,z2

!=================================================
!repetindo para 0.3

y1=xSimples(3)
c1=1
c2=3

do while( y1/=y1+((-1)**c1)*(xSimples(3)**(c2))/fact(c2) )
    y1=y1+((-1)**c1)*(xSimples(3)**(c2))/fact(c2)
    c2=c2+2
    c1=c1+1
enddo

z1=abs((((-1)**(c1-1))*(xSimples(3)**(c2-2))/fact(c2-2))/y1)

y2=xDupla(3)
c1=1
c2=3

do while( y2/=y2+((-1)**c1)*(xDupla(3)**(c2))/fact(c2) )
    y2=y2+((-1)**c1)*(xDupla(3)**(c2))/fact(c2)
    c2=c2+2
    c1=c1+1
enddo

z2=abs((((-1)**(c1-1))*(xDupla(3)**(c2-2))/fact(c2-2))/y2)

write(*,*) "0.3",z1,z2

!=================================================
!repetindo para 0.4
y1=xSimples(4)
c1=1
c2=3

do while( y1/=y1+((-1)**c1)*(xSimples(4)**(c2))/fact(c2) )
    y1=y1+((-1)**c1)*(xSimples(4)**(c2))/fact(c2)
    c2=c2+2
    c1=c1+1
enddo

z1=abs((((-1)**(c1-1))*(xSimples(4)**(c2-2))/fact(c2-2))/y1)

y2=xDupla(4)
c1=1
c2=3

do while( y2/=y2+((-1)**c1)*(xDupla(4)**(c2))/fact(c2) )
    y2=y2+((-1)**c1)*(xDupla(4)**(c2))/fact(c2)
    c2=c2+2
    c1=c1+1
enddo

z2=abs((((-1)**(c1-1))*(xDupla(4)**(c2-2))/fact(c2-2))/y2)

write(*,*) "0.4",z1,z2

write(*,*) "A partir dos resultados obtidos, nota-se que a precisão obtida é excelente,"
write(*,*) "indicando que a aproximação por séries deve ser usada para calcular funções"
write(*,*) "trigonométricas, visto que sua precisão pode ser escolhida conforme o desejado."
write(*,*) "Assim, se o número escolhido for muito pequeno, ainda assim é possível"
write(*,*) "selecionar computacionalmente uma precisão pequena o suficiente."

end program exer02

real function fact(n) !definindo a funçao fatorial
    integer:: n
    integer :: i

    fact = 1.0
    do i = 2, n
        fact = fact * i !1*2*3*...*n
    enddo

end function fact