program velocity_verlet_oscillator_exercise
    implicit none
    ! Velocity Verlet (Leapfrog) za harmoniski oscilator:
    ! x'' = -x
    !
    ! a_n = -x_n
    ! v_{n+1/2} = v_n + 0.5*h*a_n
    ! x_{n+1}   = x_n + h*v_{n+1/2}
    ! a_{n+1}   = -x_{n+1}
    ! v_{n+1}   = v_{n+1/2} + 0.5*h*a_{n+1}

    ! TODO: deklariraj promenlivi:
    ! integer :: i, nsteps
    ! real(8): t0, t, tmax, h
    ! real(8): x, v, x0, v0
    ! real(8): a, a_new
    ! real(8): vhalf
    ! real(8): x_ex, v_ex, err_x

    print *, 'Velocity Verlet (Leapfrog) za x'''' = -x'
    print *, 'Vnesi x0, v0:'
    ! TODO: read x0, v0

    print *, 'Vnesi h i tmax:'
    ! TODO: read h, tmax

    ! TODO: t0 = 0.0d0
    ! TODO: t = t0
    ! TODO: nsteps = int( (tmax - t0)/h )

    ! Pochetna sostojba
    ! TODO: x = x0
    ! TODO: v = v0
    ! Akceleracija a0 = -x0
    ! TODO: a = -x

    ! Otvori CSV fajl
    open(unit=90, file='velocity_verlet_oscillator.csv', status='replace', action='write')
    write(90,'(A)') 't,x_num,v_num,x_exact,v_exact,err_x'

    ! --- Tocka t = 0 ---
    ! TODO: x_ex = x_exact(t, x0, v0)
    ! TODO: v_ex = v_exact(t, x0, v0)
    ! TODO: err_x = abs(x - x_ex)
    ! TODO: write CSV red

    ! --- Glavna DO-petlja ---
    do i = 1, nsteps

        ! Pola-chekor brzina
        ! TODO: vhalf = v + 0.5d0*h*a

        ! Novata pozicija
        ! TODO: x = x + h*vhalf

        ! Novata akceleracija
        ! TODO: a_new = -x

        ! Poven chekor brzina
        ! TODO: v = vhalf + 0.5d0*h*a_new

        ! Zameni a = a_new
        ! TODO: a = a_new

        ! Presmetaj vreme
        ! TODO: t = t0 + dble(i)*h

        ! Tocno resenie + greska
        ! TODO: x_ex = x_exact(t, x0, v0)
        ! TODO: v_ex = v_exact(t, x0, v0)
        ! TODO: err_x = abs(x - x_ex)

        ! Zapis vo CSV
        ! TODO: write CSV line

    end do

    close(90)
    print *, 'Gotovo. CSV fajlot e velocity_verlet_oscillator.csv'

contains

    real(8) function x_exact(t, x0, v0)
        implicit none
        real(8), intent(in) :: t, x0, v0
        ! TODO: x_exact = x0*cos(t) + v0*sin(t)
    end function x_exact

    real(8) function v_exact(t, x0, v0)
        implicit none
        real(8), intent(in) :: t, x0, v0
        ! TODO: v_exact = -x0*sin(t) + v0*cos(t)
    end function v_exact

end program velocity_verlet_oscillator_exercise
