program rk4_oscillator
    implicit none
    ! Klasichen RK4 za harmoniski oscilator:
    !   x'' = -x
    ! Sistem:
    !   x' = v
    !   v' = -x
    !
    ! Shema:
    !   k1x = v_n
    !   k1v = -x_n
    !   k2x = v_n + (h/2)*k1v
    !   k2v = -(x_n + (h/2)*k1x)
    !   k3x = v_n + (h/2)*k2v
    !   k3v = -(x_n + (h/2)*k2x)
    !   k4x = v_n + h*k3v
    !   k4v = -(x_n + h*k3x)
    !
    !   x_{n+1} = x_n + h/6*(k1x + 2*k2x + 2*k3x + k4x)
    !   v_{n+1} = v_n + h/6*(k1v + 2*k2v + 2*k3v + k4v)
    !
    ! Izlez vo CSV:
    !   t,x_num,v_num,x_exact,v_exact,err_x

    integer :: i, nsteps
    real(8) :: t0, t, tmax, h
    real(8) :: x, v, x0, v0
    real(8) :: x_ex, v_ex, err_x
    real(8) :: k1x, k1v, k2x, k2v, k3x, k3v, k4x, k4v

    print *, 'RK4 za harmoniski oscilator x'''' = -x'
    print *, 'Vnesi x0, v0:'
    read(*,*) x0, v0

    print *, 'Vnesi h i tmax:'
    read(*,*) h, tmax

    t0 = 0.0d0
    t  = t0

    nsteps = int( (tmax - t0)/h )

    x = x0
    v = v0

    open(unit=70, file='rk4_oscillator.csv', status='replace', action='write')
    write(70,'(A)') 't,x_num,v_num,x_exact,v_exact,err_x'

    ! Pochetna tocka
    x_ex = x_exact(t, x0, v0)
    v_ex = v_exact(t, x0, v0)
    err_x = abs(x - x_ex)

    write(70,'(F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6)') &
         t, x, v, x_ex, v_ex, err_x

    do i = 1, nsteps

        ! 1) k1 vo t_n
        k1x = v
        k1v = -x

        ! 2) k2
        k2x = v + 0.5d0*h*k1v
        k2v = -(x + 0.5d0*h*k1x)

        ! 3) k3
        k3x = v + 0.5d0*h*k2v
        k3v = -(x + 0.5d0*h*k2x)

        ! 4) k4
        k4x = v + h*k3v
        k4v = -(x + h*k3x)

        ! 5) RK4 chekor
        x = x + (h/6.0d0)*(k1x + 2.0d0*k2x + 2.0d0*k3x + k4x)
        v = v + (h/6.0d0)*(k1v + 2.0d0*k2v + 2.0d0*k3v + k4v)

        ! Nov moment na vreme
        t = t0 + dble(i)*h

        ! Tocno resenie + greska
        x_ex = x_exact(t, x0, v0)
        v_ex = v_exact(t, x0, v0)
        err_x = abs(x - x_ex)

        write(70,'(F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6)') &
             t, x, v, x_ex, v_ex, err_x
    end do

    close(70)
    print *, 'Gotovo. CSV fajlot e rk4_oscillator.csv'

contains

    real(8) function x_exact(t, x0, v0)
        implicit none
        real(8), intent(in) :: t, x0, v0
        x_exact = x0*cos(t) + v0*sin(t)
    end function x_exact

    real(8) function v_exact(t, x0, v0)
        implicit none
        real(8), intent(in) :: t, x0, v0
        v_exact = -x0*sin(t) + v0*cos(t)
    end function v_exact

end program rk4_oscillator
