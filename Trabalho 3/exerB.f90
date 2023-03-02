program exerB
implicit none
!tarefa A:
real(8) :: m = 80.0d0, P = 400.0d0, tempo = 0.0d0 !massa bike+cicl; potencia; tempo decorrido
real(8) :: T, dt, v0 !tempo total; variacao de tempo; velocidade inicial
integer :: Nlinhas, i
!tarefa B:
real(8) :: rho = 1.2d0, C = 1.0d0/2.0d0
real(8) :: area, dist_perc, tterm !area de choque; distancia total percorrida; instante de vel term
logical :: varaux = .TRUE.


read(*,*) T, dt, v0, area !lendo o valor das variáveis

Nlinhas = int(T/dt)

open(unit = 1, file="velB_out.dat")

    write(1,*) 0.0d0, v0

    do i = 1, Nlinhas
        
        if ((v0 + (P/(m*v0))*dt - ((C * rho * area * v0**2)/m)*dt == v0) .and. (varaux .eqv. .TRUE.)) then
            tterm = tempo
            varaux = .FALSE.
        end if

        tempo = tempo + dt

        v0 = v0 + (P/(m*v0))*dt - ((C * rho * area * v0**2)/m)*dt !area=0 transforma no exerA
        
        dist_perc = dist_perc + v0 * dt

        write(1,*) tempo, v0

    end do

close(1)

write(*,*) "I) O ciclista se curva em corridas pois, se inclinando, a área efetiva de choque com o ar é"
write(*,*) "diminuída consideravelmente, visto que apenas a componente com o ângulo da inclinação gera"
write(*,*) "efeitos resistivos. II) Pois assim são diminuídos os efeitos de cansaço associados ao atrito"
write(*,*) "do ar, visto que apenas quem está na frente do grupo sofrerá seus efeitos de forma significativa."
write(*,*) "Dessa forma, com revezamentos e organização suficiente, a média de esforço do ciclista para andar"
write(*,*) "uma mesma distância diminui. III) É mais vantajoso por um efeito chamado aproveitamento de vácuo."
write(*,*) "Quando o ciclista percorre contra o ar, ele sofre os efeitos da resistência do ar, colidindo com"
write(*,*) "suas moléculas. Dessa forma, deixa para trás uma região de baixa pressão, em que os efeitos resis-"
write(*,*) "tivos do ar serão consideravelmente minimizados. Portanto, para o ciclista que pretende ultrapassar"
write(*,*) "o outro, é vantajoso deixar o da frente sofrer os efeitos do atrito do ar em seu lugar, e se deslo-"
write(*,*) "car mais facilmente devido ao baixo atrito. Neste cenário, dois ciclistas com a mesma potência produ"
write(*,*) "zida terão velocidades bem diferentes: o de trás correrá bem mais rápido que o da frente."
write(*,*) dist_perc
write(*,*) v0
write(*,*) tterm
write(*,*) (dist_perc/T)
end program exerB