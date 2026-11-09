
program grafik_funkcija
    implicit none

    integer :: i, n
    real(8) :: a, b, h, x, y

    ! Interval i broj na podelbi
    a = 1.0d0
    b = 2.0d0
    n = 100

    h = (b-a)/n

    ! Kreiranje CSV datoteka
    open(10, file='grafik.csv', status='replace')

    write(10,'(A)') 'x,f(x)'

    do i = 0, n
        x = a + i*h
        y = f(x)

        write(10,'(F12.6,",",F15.8)') x, y
    end do

    close(10)

    print *, 'Podatocite se zacuvani vo grafik.csv'

contains

    real(8) function f(x)
        real(8), intent(in) :: x
        f = x**3 - x - 2.0d0
    end function f

end program grafik_funkcija
