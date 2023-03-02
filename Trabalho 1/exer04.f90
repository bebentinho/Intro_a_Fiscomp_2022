program exer04
    implicit none
    real, dimension (4) :: Lista, ListaSorted !Listas que conterão as áreas
    real,dimension(3) :: vertice1, vertice2, vertice3, vertice4 !Pontos lidos pelo documento
    real,dimension(3) :: vetor0, vetor1, vetor2, vetor3, vetor4, vetor5 !Vetores dos lados=>áreas
    real,dimension(3) :: resultado !vetor resultante da função "Vetorial"
    real :: area=0, volume !valor para áreas e volume
    real :: Determinante, ModVetor !valores das funções "Determinante" e "ModVetor"
    integer :: i !Índice auxiliar p/ soma das áreas

    vetor0=(/1,1,1/) !Criando o vetor para o cálculo das áreas
    resultado=(/0,0,0/) !Criando o vetor auxiliar resultante do prod vetorial

    open(unit=1,file="vet_in.dat") !Abrindo o arquivo com os vértices
        read(1,*) vertice1,vertice2,vertice3,vertice4 !Criando vetores com os vértices
    close(1)

    !Criando os vetores dos lados do tetraedro:
    vetor1=vertice2-vertice1
    vetor2=vertice3-vertice1
    vetor3=vertice4-vertice1
    vetor4=vertice3-vertice2
    vetor5=vertice4-vertice2

    !Calculando o volume

    call Vetorial(vetor1,vetor2,vetor3,resultado) !alterando o valor de resultado p/ o prod vetorial
    volume = abs(Determinante(resultado))/6 !volume = |(vetorial dos 3)/6| (1/6 do paralelepipedo)

    !Calculando as áreas
    call Vetorial(vetor0,vetor1,vetor2,resultado) !alterando o valor de resultado p/ dois lados
    Lista(1) = ModVetor(resultado)/2 !área do lado = metade da área do paralelepipedo (modulo do vetor)

    call Vetorial(vetor0,vetor1,vetor3,resultado)
    Lista(2) = ModVetor(resultado)/2

    call Vetorial(vetor0,vetor2,vetor3,resultado)
    Lista(3) = ModVetor(resultado)/2

    call Vetorial(vetor0,vetor4,vetor5,resultado)
    Lista(4) = ModVetor(resultado)/2

    !Calculando a soma das áreas
    do i=1,4
        area=area+Lista(i) !área=soma das áreas dos lados
    enddo

    call sort(Lista,ListaSorted,4) !ordenando a lista com as áreas

    open(unit=2,file="tetra_out.dat") !abrindo o arquivo desejado
        !escrevendo, nele, as variáveis na ordem desejada
        write(2,*) volume !volume do sólido
        write(2,*) area !soma das áreas
        write(2,*) ListaSorted
    close(2)

end program exer04

!======================================FUNÇÕES======================================

!FUNÇÃO PRODUTO VETORIAL
!Será usada para (I) o produto misto (volume) e para (II) o cálculo das áreas:
!(I) Para calcular o volume, basta calcular o DETERMINANTE do vetor;
!(II) Para o cálculo das áreas, é necessário que vetor1=(1,1,1), e a área=módulo do vetor.

subroutine Vetorial(vetor1,vetor2,vetor3,resultado) 
    real,dimension(3):: vetor1,vetor2, vetor3, resultado

    resultado(1)=(vetor1(1)*vetor2(2)*vetor3(3)-vetor1(1)*vetor2(3)*vetor3(2))
    resultado(2)=(vetor1(2)*vetor2(3)*vetor3(1)-vetor1(2)*vetor2(1)*vetor3(3))
    resultado(3)=(vetor1(3)*vetor2(1)*vetor3(2)-vetor1(3)*vetor2(2)*vetor3(1))

end subroutine Vetorial

!FUNÇÃO DETERMINANTE
!(I) Para o cálculo do volume.

real function Determinante(vetor)
    real, dimension(3) :: vetor

    Determinante = vetor(1)+vetor(2)+vetor(3)

end function Determinante

!FUNÇÃO MÓDULO DO VETOR
!(II) Para o cálculo das áreas.

real function ModVetor(vetor)
    real, dimension(3) :: vetor
    
    ModVetor=sqrt(vetor(1)**2+vetor(2)**2+vetor(3)**2)

end function ModVetor

!ORDENAR MENOR-MAIOR
!Ordenação insertion sort (se não me engano :D).

subroutine sort(array, Outarray, m)
    real,dimension(m)::array, OutArray
    integer::m, i, aux, j

    do j = 1, m
        aux = 1
        do i = 1, m-j+1
            if (array(i) < array(aux)) then
                aux = i        
            end if
        end do
        OutArray(j) = array(aux)
        do i = aux + 1, m
            array(i-1)=array(i)            
        end do
    end do
   
end