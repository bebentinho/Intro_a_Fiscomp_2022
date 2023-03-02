program exerB
    implicit none
    real(8) :: yT, yT0, yTantes
    real(8) :: xT, xT0, xTantes
    real(8) :: yJ, yJ0, yJantes
    real(8) :: xJ, xJ0, xJantes
    real(8) :: vTx, vTy, vJx, vJy
    real(8) :: rTS, rJS, rTJ, dt, t
    real(8) :: mS, mT, mJ, mJS, mTS
    real(8), parameter :: pi = 4.0d0 * atan(1.d0)
    integer :: m
    logical :: aux
    
    print*, "Quantas vezes a massa de Júpiter? Digite um inteiro:"
    read(*,*) m
    print*, "Qual intervalo dt?"
    read(*,*) dt

    xT0 = 1.0d0 !1 U.A.
    yT0 = 0.0d0
    vTx = 0.0d0
    vTy = 2.d0 * pi

    xJ0 = 5.20d0 !5.20 U.A.
    yJ0 = 0.0d0
    vJx = 0.0d0
    vJy = 2.7553590302269777d0 !v0 de Júpiter (tabA1_out.dat)

    t = 0.0d0
    
    mJ = m * 1.9E+27 !massas (tabela do trabalho)
    mT = 6.0E+24
    mS = 2.0E+30

    mJS = mJ/mS
    mTS = mT/mS

    open(unit = 1, file = "trajBterra_out.dat") !trajetoria da terra
    open(unit = 2, file = "trajBjupiter_out.dat") !trajetoria de jupiter
    
    write(1,*) "           t           ", "            x(t)           ", "           y(t)" !cabeçalho
    write(1,*)  0.0d0, xT0, yT0

    write(2,*) "           t           ", "            x(t)           ", "           y(t)" !cabeçalho
    write(2,*)  0.0d0, xJ0, yJ0
    
    !terra
    yT = yT0 + vTy * dt !definindo yi
    xT = xT0 + vTx * dt !definindo xi
    
    yTantes = yT0 !definindo y_(i-1)
    xTantes = xT0 !definindo x_(i-1)
    
    !jupiter
    yJ = yJ0 + vJy * dt !definindo yi
    xJ = xJ0 + vJx * dt !definindo xi
    
    yJantes = yJ0 !definindo y_(i-1)
    xJantes = xJ0 !definindo x_(i-1)

    aux = .true. !variavel auxilicar para terminar o loop
    
    do while(aux .eqv. .true.)
    
        rTS = sqrt( (xT)**2 + (yT)**2 ) !(xS,yS) = (0,0)
        rJS = sqrt( (xJ)**2 + (yJ)**2 )
        rTJ = sqrt( (xT-xJ)**2 + (yT-yJ)**2 )
        t = t + dt
    
        !y para os dois planetas
        yT0 = yT
        yJ0 = yJ

        yT = 2 * yT - yTantes - (4.0d0 * pi**2 / rTS**3) * yT * dt**2 - &
        (4.0d0 * pi**2 * mJS / rTJ**3) * (yT-yJ) * dt**2 !aplicando a formula
        yJ = 2 * yJ - yJantes - (4.0d0 * pi**2 / rJS**3) * yJ * dt**2 - &
        (4.0d0 * pi**2 * mTS / rTJ**3) * (yJ-yT) * dt**2
        
        yTantes = yT0
        yJantes = yJ0
    
        !x para os dois planetas
        xT0 = xT
        xJ0 = xJ

        xT = 2 * xT - xTantes - (4.0d0 * pi**2 / rTS**3) * xT * dt**2 - &
        (4.0d0 * pi**2 * mJS / rTJ**3) * (xT-xJ) * dt**2 !aplicando a formula
        xJ = 2 * xJ - xJantes - (4.0d0 * pi**2 / rJS**3) * xJ * dt**2 - &
        (4.0d0 * pi**2 * mTS / rTJ**3) * (xJ-xT) * dt**2
        
        xTantes = xT0
        xJantes = xJ0
    
        if ((yJ > 0) .and. (yJantes < 0)) then !completando uma órbita (de jupiter)
            aux = .false.
        end if
    
        write(1,*) t, xT, yT
        write(2,*) t, xJ, yJ
    
    end do
    
    close(2)
    close(1)
    
    write(*,*) "Observe que quanto maior a massa de Júpiter, mais a órbita da Terra desvia. Em m = 1000"
    write(*,*) "(lembrando que m é definido como o inteiro que multiplica a massa de Júpiter), a Terra"
    write(*,*) "chega a sair de órbita, e com m maiores o comportamento da trajetória fica ainda mais"
    write(*,*) "irregular. A não peridiocidade da órbita da Terra pode ser notada pela não-sobreposição"
    write(*,*) "dos pontos de sua trajetória, que, com um dt muito pequeno, gerariam uma elipse grossa."

    end program exerB