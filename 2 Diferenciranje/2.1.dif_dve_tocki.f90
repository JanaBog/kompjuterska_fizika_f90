
program diferenciranje
    implicit none
    real(8) :: x, h, d1, tocno, greska

    print *, "Vnesi x i h:"
    read *, x, h

    if (h <= 0.0d0) stop "h mora da bide pozitivno"

    ! Priblizen prv izvod
    d1 = (f(x+h) - f(x))/h

    ! Tona analiticka vrednost
    tocno = 2.0d0*x

    ! Apsolutna greska
    greska = abs(d1 - tocno)

    print *, "Priblizen izvod:", d1
    print *, "Tocen izvod:    ", tocno
    print *, "Apsolutna greska:", greska

contains

    real(8) function f(x)
        implicit none
        real(8), intent(in) :: x
        f = x**2
    end function f

end program diferenciranje
