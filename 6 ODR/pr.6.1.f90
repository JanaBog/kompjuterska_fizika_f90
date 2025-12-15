program taylor_oscillator
    implicit none
    ! Taylor metod od 2-ri red za linearniot oscilator:
    !   x'' = -x
    !
    ! Go pisuvame kako sistem od prv red:
    !   x' = v
    !   v' = -x
    !
    ! Tocko resenie:
    !   x(t) = x0*cos(t) + v0*sin(t)
    !   v(t) = -x0*sin(t) + v0*cos(t)
    !
    ! Taylor od 2-ri red:
    !   x_{n+1} = x_n + h*x'_n + (h^2/2)*x''_n
    !   v_{n+1} = v_n + h*v'_n + (h^2/2)*v''_n
    !
    ! Bidejki x'_n = v_n, x''_n = -x_n,
    !         v'_n = -x_n, v''_n = -v_n,
    ! dobivame:
    !   x_{n+1} = x_n + h*v_n - 0.5*h^2*x_n
    !   v_{n+1} = v_n - h*x_n - 0.5*h^2*v_n

    integer :: i, nsteps
    real(8) :: t0, t, tmax, h
    real(8) :: x, v           ! numericko resenie
    real(8) :: x0, v0         ! pochetni uslovi
    real(8) :: x_ex, v_ex     ! tocko resenie
    real(8) :: err_x          ! apsolutna greska vo x
    real(8) :: x_new, v_new   ! vrednosti za sledniot cekor

    ! ======= Vlez od korisnik =======
    print *, 'Taylor (2nd order) za x'''' = -x'
    print *, 'Vnesi x0, v0 (pochetna pozicija i brzina):'
    read(*,*) x0, v0

    print *, 'Vnesi cekor h i kraj tmax:'
    read(*,*) h, tmax

    ! Pochhetno vreme
    t0 = 0.0d0
    t  = t0

    ! Kolku cekori treba da ima?
    nsteps = int( (tmax - t0)/h )

    ! Pochhetni uslovi
    x = x0
    v = v0

    ! Otvori fajl za rezultatite
    open(unit=10, file='taylor_oscillator.csv', status='replace', action='write')
    write(10,'(A)') '# t,      x_num,        v_num,        x_exact,      v_exact,      err_x'

    ! Prv red (t = 0)
    x_ex = x_exact(t, x0, v0)
    v_ex = v_exact(t, x0, v0)
    err_x = abs(x - x_ex)

    write(10,'(F10.4, 5(a, 1X,ES14.6))') t,',', x,',', v,',', x_ex,',', v_ex,',', err_x

    ! ======= Glavna petlja =======
    do i = 1, nsteps

        ! Taylor-cekor za x i v
        x_new = x + h*v - 0.5d0*h*h*x
        v_new = v - h*x - 0.5d0*h*h*v

        ! Nov moment na vreme
        t = t0 + dble(i)*h

        ! Update na sostojbata
        x = x_new
        v = v_new

        ! Tocko resenie + greska
        x_ex = x_exact(t, x0, v0)
        v_ex = v_exact(t, x0, v0)
        err_x = abs(x - x_ex)

        ! Zapis vo fajl
        write(10,'(F10.4, 5(1X,ES14.6))') t, x, v, x_ex, v_ex, err_x
    end do

    close(10)
    print *, 'Gotovo. Rezultatite se vo taylor_oscillator.dat'

contains

    ! Tocko resenie za x(t)
    real(8) function x_exact(t, x0, v0)
        implicit none
        real(8), intent(in) :: t, x0, v0
        x_exact = x0*cos(t) + v0*sin(t)
    end function x_exact

    ! Tocko resenie za v(t)
    real(8) function v_exact(t, x0, v0)
        implicit none
        real(8), intent(in) :: t, x0, v0
        v_exact = -x0*sin(t) + v0*cos(t)
    end function v_exact

end program taylor_oscillator
