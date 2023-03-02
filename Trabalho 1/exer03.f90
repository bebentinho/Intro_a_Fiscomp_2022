program exer03
implicit none

character:: aux*9
integer:: m,i

read(*,*) m

open (unit = 1, file = "primos_out.dat")

do i = 2, m
    call verificar(i,aux)

    if (aux .eq. "primo") then
        write(1,*) i    
    end if

enddo

close(1)

end program exer03

subroutine verificar(n,result)
    integer:: n, i, raiz
    character:: result*9

    if (n .eq. 1) then
        result = "nao primo"
        return
    end if

    if (mod(n,2) .eq. 0) then
        result = "nao primo"
        return
    end if

    raiz = sqrt(real(n))

    do i = 2, raiz
        if (mod(n,i) .eq. 0) then
            result = "nao primo"
            return
        end if
    end do
    
    result = "primo"
end