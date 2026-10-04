program euler_explicit_oscillator_exercise
    implicit none
    ! Ekspliziten Ojler za linearen oscilator:
    !   x'' = -x
    ! Sistem:
    !   x' = v
    !   v' = -x

    ! TODO: deklariraj gi promenlivite
    ! integer: i, nsteps
    ! real(8): t0, t, tmax, h
    ! real(8): x, v, x0, v0, x_ex, v_ex, err_x
    ! real(8): x_new, v_new

    ! ====== Vlez od korisnik ======
    print *, 'Ekspliziten Ojler za x'''' = -x'
    print *, 'Vnesi x0, v0 (pochetna pozicija i brzina):'
    ! TODO: procitaj x0, v0

    print *, 'Vnesi cekor h i krajno vreme tmax:'
    ! TODO: procitaj h, tmax

    ! Pochhetno vreme
    ! TODO: postavi t0 = 0.0d0, t = t0

    ! Broj na cekori: nsteps ≈ (tmax - t0)/h
    ! TODO: presmetaj nsteps so int( (tmax - t0)/h )

    ! Inicijalizacija na sostojbata
    ! TODO: x = x0 ; v = v0

    ! Otvori fajl za Excel-friendly izlez
    open(unit=20, file='euler_explicit_oscillator.dat', status='replace', action='write')

    ! Header: iminja na koloni razdvoeni so prazni mesta
    write(20,'(A)') 't x_num v_num x_exact v_exact err_x'

    ! Pochetna tocka (t0)
    ! TODO: presmetaj x_ex = x_exact(t, x0, v0)
    ! TODO: presmetaj v_ex = v_exact(t, x0, v0)
    ! TODO: err_x = abs(x - x_ex)
    ! TODO: zapishi red so format, na primer:
    !       (F12.6, 5(1X,F12.6))

    ! ====== Glaven DO-ciklus ======
    do i = 1, nsteps

        ! Ekspliziten Ojler:
        ! x_{n+1} = x_n + h*v_n
        ! v_{n+1} = v_n - h*x_n

        ! TODO: presmetaj x_new i v_new

        ! Nov moment na vreme
        ! TODO: t = t0 + dble(i)*h

        ! Update na sostojbata
        ! TODO: x = x_new
        ! TODO: v = v_new

        ! Tocko resenie i greska
        ! TODO: x_ex = x_exact(t, x0, v0)
        ! TODO: v_ex = v_exact(t, x0, v0)
        ! TODO: err_x = abs(x - x_ex)

        ! Zapis vo fajl (eden red)
        ! TODO: write(20,'(F12.6, 5(1X,F12.6))') ...
    end do

    close(20)
    print *, 'Gotovo. Rezultatite se vo euler_explicit_oscillator.dat'

contains

    ! Tocno resenie za x(t)
    real(8) function x_exact(t, x0, v0)
        implicit none
        real(8), intent(in) :: t, x0, v0
        ! TODO: x_exact = x0*cos(t) + v0*sin(t)
    end function x_exact

    ! Tocko resenie za v(t) = x'(t)
    real(8) function v_exact(t, x0, v0)
        implicit none
        real(8), intent(in) :: t, x0, v0
        ! TODO: v_exact = -x0*sin(t) + v0*cos(t)
    end function v_exact

end program euler_explicit_oscillator_exercise
