program rk2_midpoint_oscillator
    implicit none
    ! Ekspliziten RK2 (midpoint) za harmoniski oscilator:
    !   x'' = -x
    ! Sistem:
    !   x' = v
    !   v' = -x
    !
    ! Shema:
    !   k1x = v_n
    !   k1v = -x_n
    !   x_mid = x_n + (h/2)*k1x
    !   v_mid = v_n + (h/2)*k1v
    !   k2x = v_mid
    !   k2v = -x_mid
    !   x_{n+1} = x_n + h*k2x
    !   v_{n+1} = v_n + h*k2v
    !
    ! Izlez vo CSV:
    !   t,x_num,v_num,x_exact,v_exact,err_x

    integer :: i, nsteps
    real(8) :: t0, t, tmax, h
    real(8) :: x, v, x0, v0
    real(8) :: x_ex, v_ex, err_x
    real(8) :: k1x, k1v, k2x, k2v
    real(8) :: x_mid, v_mid

    print *, 'Ekspliziten RK2 (midpoint) za x'''' = -x'
    print *, 'Vnesi x0, v0:'
    read(*,*) x0, v0

    print *, 'Vnesi h i tmax:'
    read(*,*) h, tmax

    t0 = 0.0d0
    t  = t0

    nsteps = int( (tmax - t0)/h )

    x = x0
    v = v0

    open(unit=50, file='rk2_midpoint_oscillator.csv', status='replace', action='write')
    write(50,'(A)') 't,x_num,v_num,x_exact,v_exact,err_x'

    ! Pochetna tocka (t0)
    x_ex = x_exact(t, x0, v0)
    v_ex = v_exact(t, x0, v0)
    err_x = abs(x - x_ex)

    write(50,'(F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6)') &
         t, x, v, x_ex, v_ex, err_x

    do i = 1, nsteps

        ! 1) k1 vo t_n
        k1x = v
        k1v = -x

        ! 2) Sredna tochka
        x_mid = x + 0.5d0*h*k1x
        v_mid = v + 0.5d0*h*k1v

        ! 3) k2 vo srednata tochka
        k2x = v_mid
        k2v = -x_mid

        ! 4) RK2 chekor
        x = x + h*k2x
        v = v + h*k2v

        ! Nov moment na vreme
        t = t0 + dble(i)*h

        ! Tocno resenie + greska
        x_ex = x_exact(t, x0, v0)
        v_ex = v_exact(t, x0, v0)
        err_x = abs(x - x_ex)

        write(50,'(F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6)') &
             t, x, v, x_ex, v_ex, err_x
    end do

    close(50)
    print *, 'Gotovo. CSV fajlot e rk2_midpoint_oscillator.csv'

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

end program rk2_midpoint_oscillator
