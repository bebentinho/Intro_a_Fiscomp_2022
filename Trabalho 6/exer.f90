program exer
implicit none
real(8), parameter :: pi = 4.0d0 * atan(1.d0)

open(unit = 1, file = "tabela.dat")

write(1,*) (2.0d0* pi * 0.39d0 / sqrt((4*pi**2)/0.39d0))**2 / (0.39d0)**3
write(1,*) (2.0d0* pi * 0.72d0/sqrt((4*pi**2)/0.72d0))**2/(0.72d0)**3
write(1,*) (2.0d0* pi * 1.d0/sqrt((4*pi**2)/1.d0))**2/(1.d0)**3
write(1,*) (2.0d0* pi * 1.52d0/sqrt((4*pi**2)/1.52d0))**2/(1.52d0)**3
write(1,*) (2.0d0* pi * 5.20d0/sqrt((4*pi**2)/5.20d0))**2/(5.20d0)**3
write(1,*) (2.0d0* pi * 9.24d0/sqrt((4*pi**2)/9.24d0))**2/(9.24d0)**3
write(1,*) (2.0d0* pi * 19.19d0/sqrt((4*pi**2)/19.19d0))**2/(19.19d0)**3
write(1,*) (2.0d0* pi * 30.06d0/sqrt((4*pi**2)/30.06d0))**2/(30.06d0)**3
write(1,*) (2.0d0* pi * 39.53d0/sqrt((4*pi**2)/39.53d0))**2/(39.53d0)**3

close(1)

end program exer