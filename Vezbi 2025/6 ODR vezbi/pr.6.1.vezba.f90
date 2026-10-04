program taylor_oscillator_exercise
    implicit none
    ! Taylor metod od 2-ri red za x'' = -x, zapisan kako sistem:
    ! x' = v
    ! v' = -x

    ! TODO: deklariraj gi site promenlivi eksplicitno (bez implicit typing)
    ! integer: i, nsteps
    ! real(8): t0, t, tmax, h, x, v, x0, v0, x_ex, v_ex, err_x, x_new, v_new

    ! 1) Vlez: pochetni uslovi i parametri za cekor
    print *, 'Taylor metod, 2-ri red, za x'''' = -x'
    print *, 'Tocno resenie: x(t) = x0*cos(t) + v0*sin(t)'
    print *, 'Vnesi x0, v0 (pochetna pozicija i brzina):'
    ! TODO: procitaj x0 i v0

    print *, 'Vnesi cekor h i krajna vrednost tmax:'
    ! TODO: procitaj h i tmax

    ! Pochhetno vreme
    ! TODO: t0 = 0.0d0 i t = t0

    ! 2) Broj na cekori vo vremeto
    ! nsteps ≈ (tmax - t0)/h
    ! TODO: presmetaj nsteps so int()

    ! 3) Inicijalizacija
    ! TODO: postavi x = x0 i v = v0

    ! 4) Otvaranje izlezna datoteka + zapisuvanje zaglavie
    open(unit=10, file='taylor_oscillator.dat', status='replace', action='write')
    write(10,'(A)') '# t      x_num        v_num        x_exact      v_exact      err_x'

    ! 5) Pochetna vrednost (t0)
    ! TODO: presmetaj exact resenie za t0
    ! TODO: presmetaj greska err_x = abs(x - x_ex)
    ! TODO: zapishi go prviot red vo fajlot

    ! 6) Glavna petlja vo vremeto
    do i = 1, nsteps

        ! --- Taylor od 2-ri red ---
        ! Za ovoj problem:
        ! x_{n+1} = x_n + h*v_n - 0.5*h*h*x_n
        ! v_{n+1} = v_n - h*x_n - 0.5*h*h*v_n

        ! TODO: presmetaj x_new i v_new

        ! Nov moment na vreme
        ! TODO: t = t0 + i*h

        ! Podgotovka za sledniot cekor
        ! TODO: x = x_new ; v = v_new

        ! Tocko resenie i greska
        ! TODO: x_ex = x_exact(t, x0, v0)
        ! TODO: v_ex = v_exact(t, x0, v0)
        ! TODO: err_x = abs(x - x_ex)

        ! Zapis na rezultatot
        ! TODO: WRITE so format (F10.4, 5(1X,ES14.6))
    end do

    close(10)
    print *, 'Gotovo. Rezultatite se vo taylor_oscillator.dat'

contains

    ! Tocno resenie za x(t)
    real(8) function x_exact(t, x0, v0)
        implicit none
        real(8), intent(in) :: t, x0, v0
        ! TODO: x_exact = x0*cos(t) + v0*sin(t)
    end function x_exact

    ! Tocno resenie za v(t) = x'(t)
    real(8) function v_exact(t, x0, v0)
        implici
