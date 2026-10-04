program velocity_verlet_oscillator
    implicit none
    ! Velocity Verlet (Leapfrog) za harmoniski oscilator:
    !
    ! a_n = -x_n
    ! v_{n+1/2} = v_n + 0.5*h*a_n
    ! x_{n+1}   = x_n + h*v_{n+1/2}
    ! a_{n+1}   = -x_{n+1}
    ! v_{n+1}   = v_{n+1/2} + 0.5*h*a_{n+1}

    integer :: i, nsteps
    real(8) :: t0, t, tmax, h
    real(8) :: x, v, x0, v0
    real(8) :: a, a_new
    real(8) :: vhalf
    real(8) :: x_ex, v_ex, err_x

    print *, 'Velocity Verlet (Leapfrog) za x'''' = -x'
    print *, 'Vnesi x0, v0:'
    read(*,*) x0, v0

    print *, 'Vnesi h i tmax:'
    read(*,*) h, tmax

    t0 = 0.0d0
    t  = t0
    nsteps = int( (tmax - t0)/h )

    ! Pochetni uslovi
    x = x0
    v = v0
    a = -x

    open(unit=90, file='velocity_verlet_oscillator.csv', status='replace', action='write')
    write(90,'(A)') 't,x_num,v_num,x_exact,v_exact,err_x'

    ! --- t = 0 ---
    x_ex = x_exact(t, x0, v0)
    v_ex = v_exact(t, x0, v0)
    err_x = abs(x - x_ex)

    write(90,'(F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6)') &
         t, x, v, x_ex, v_ex, err_x

    ! --- Glavna petlja ---
    do i = 1, nsteps

        ! Pola-chekor brzina
        vhalf = v + 0.5d0*h*a

        ! Novata pozicija
        x = x + h*vhalf

        ! Novata akceleracija
        a_new = -x

        ! Poven chekor brzina
        v = vhalf + 0.5d0*h*a_new

        ! Zamena
        a = a_new

        ! Tekovno vreme
        t = t0 + dble(i)*h

        ! Tocno resenie i greska
        x_ex = x_exact(t, x0, v0)
        v_ex = v_exact(t, x0, v0)
        err_x = abs(x - x_ex)

        write(90,'(F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6)') &
             t, x, v, x_ex, v_ex, err_x
    end do

    close(90)
    print *, 'Gotovo. CSV fajlot e velocity_verlet_oscillator.csv'

contains

    real(8) function x_exact(t, x0, v0)
        implicit none
        real(8), intent(in):: t, x0, v0
        x_exact = x0*cos(t) + v0*sin(t)
    end function x_exact

    real(8) function v_exact(t, x0, v0)
        implicit none
        real(8), intent(in):: t, x0, v0
        v_exact = -x0*sin(t) + v0*cos(t)
    end function v_exact

end program velocity_verlet_oscillator
