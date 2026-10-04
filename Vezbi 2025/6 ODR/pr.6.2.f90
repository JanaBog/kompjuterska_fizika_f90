program euler_explicit_oscillator
    implicit none
    ! Ekspliziten Ojler za harmoniski oscilator:
    !   x'' = -x
    ! Sistem:
    !   x' = v
    !   v' = -x
    !
    ! Shema na Ojler:
    !   x_{n+1} = x_n + h*v_n
    !   v_{n+1} = v_n - h*x_n
    !
    ! Isto kako kaj Taylor, gi chuvame i tocnite reshenija za da presmetame greska.

    integer :: i, nsteps
    real(8) :: t0, t, tmax, h
    real(8) :: x, v           ! numericko resenie
    real(8) :: x0, v0         ! pochetni uslovi
    real(8) :: x_ex, v_ex     ! tocko resenie
    real(8) :: err_x          ! apsolutna greska vo x
    real(8) :: x_new, v_new   ! vrednosti za sledniot cekor

    ! ====== Vlez od korisnik ======
    print *, 'Ekspliziten Ojler za x'''' = -x'
    print *, 'Vnesi x0, v0 (pochetna pozicija i brzina):'
    read(*,*) x0, v0

    print *, 'Vnesi cekor h i krajno vreme tmax:'
    read(*,*) h, tmax

    ! Pochhetno vreme
    t0 = 0.0d0
    t  = t0

    ! Broj na cekori
    nsteps = int( (tmax - t0)/h )

    ! Pochetni vrednosti za x i v
    x = x0
    v = v0

    ! Otvori fajl za rezultatite (pogodni za Excel)
    open(unit=20, file='euler_explicit_oscillator.dat', status='replace', action='write')

    ! Zaglavie na tabelata (imenja na kolonite)
    write(20,'(A)') 't x_num v_num x_exact v_exact err_x'

    ! Pochetna tocka (t = 0)
    x_ex = x_exact(t, x0, v0)
    v_ex = v_exact(t, x0, v0)
    err_x = abs(x - x_ex)

    write(20,'(F12.6, 5(1X,F12.6))') t, x, v, x_ex, v_ex, err_x

    ! ====== Glavna petlja vo vremeto ======
    do i = 1, nsteps

        ! Ekspliziten Ojler:
        ! x_{n+1} = x_n + h*v_n
        ! v_{n+1} = v_n - h*x_n
        x_new = x + h*v
        v_new = v - h*x

        ! Nov moment na vreme
        t = t0 + dble(i)*h

        ! Update na sostojbata
        x = x_new
        v = v_new

        ! Tocko resenie
        x_ex = x_exact(t, x0, v0)
        v_ex = v_exact(t, x0, v0)

        ! Apsolutna greska vo x
        err_x = abs(x - x_ex)

        ! Zapis na eden red (Excel-friendly)
        write(20,'(F12.6, 5(1X,F12.6))') t, x, v, x_ex, v_ex, err_x
    end do

    close(20)
    print *, 'Gotovo. Rezultatite se vo euler_explicit_oscillator.dat'

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

end program euler_explicit_oscillator
