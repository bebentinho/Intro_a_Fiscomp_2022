program exer01
implicit none
real(4):: a1=1.0e0
real(8):: a2=1.0d0
real(16):: a3=1.0_16
integer:: c1=0,c2=0,c3=0

WRITE(*,*) "PRECISAO SIMPLES"

do while((1.0e0+a1)/=1.0e0)
    a1=a1/(2.0e0)
    WRITE(*,*) a1, a1+1.0e0
    c1=c1+1
enddo

WRITE(*,*) "PRECISAO DUPLA"

do while((1.0d0+a2)/=1.0d0)
    a2=a2/2.0d0
    WRITE(*,*) a2,a2+1.0d0
    c2=c2+1
enddo

WRITE(*,*) "PRECISAO QUADRUPLA"

do while(1.0_16+a3/=1.0_16)
    a3=a3/2.0_16
    WRITE(*,*) a3,a3+1.0_16
    c3=c3+1
enddo

WRITE(*,*) c1,a1*2.0e0
WRITE(*,*) c2,a2*2.0d0
WRITE(*,*) c3,a3*2.0_16

end program exer01