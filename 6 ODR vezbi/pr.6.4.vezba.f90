program euler_improved_oscillator
    implicit none
    ! Podobren Ojler (Heun) za harmoniski oscilator:
    !   x'' = -x
    ! Sistem:
    !   x' = v
    !   v' = -x
    !
    ! Heun:
    !   k1x = v_n
    !   k1v = -x_n
    !   x_star = x_n + h*k1x
    !   v_star = v_n + h*k1v
    !   k2x = v_star
    !   k2v = -x_star
    !   x_{n+1} = x_n + h/2*(k1x + k2x)
    !   v_{n+1} = v_n + h/2*(k1v + k2v)
    !
    ! Izlez vo CSV format za Excel:
    !   t,x_num,v_num,x_exact,v_exact,err_x

    integer :: i, nsteps
    real(8) :: t0, t, tmax, h
    real(8) :: x, v, x0, v0
    real(8) :: x_ex, v_ex, err_x
    real(8) :: k1x, k1v, k2x, k2v
    real(8) :: x_star, v_star

    print *, 'Podobren Ojler (Heun) za x'''' = -x'
    print *, 'Vnesi x0, v0:'
    read(*,*) x0, v0

    print *, 'Vnesi h i tmax:'
    read(*,*) h, tmax

    t0 = 0.0d0
    t  = t0

    nsteps = int( (tmax - t0)/h )

    x = x0
    v = v0

    open(unit=40, file='euler_improved_oscillator.csv', status='replace', action='write')

    write(40,'(A)') 't,x_num,v_num,x_exact,v_exact,err_x'

    ! Pochetna tocka
    x_ex = x_exact(t, x0, v0)
    v_ex = v_exact(t, x0, v0)
    err_x = abs(x - x_ex)

    write(40,'(F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6)') &
         t, x, v, x_ex, v_ex, err_x

    do i = 1, nsteps

        ! 1) k1 vo t_n
        k1x = v
        k1v = -x

        ! 2) Predviduvanje
        x_star = x + h*k1x
        v_star = v + h*k1v

        ! 3) k2 vo predvidenata tocka
        k2x = v_star
        k2v = -x_star

        ! 4) Korigiran chekor
        x = x + 0.5d0*h*(k1x + k2x)
        v = v + 0.5d0*h*(k1v + k2v)

        ! Nov moment na vreme
        t = t0 + dble(i)*h

        ! Tocno resenie + greska
        x_ex = x_exact(t, x0, v0)
        v_ex = v_exact(t, x0, v0)
        err_x = abs(x - x_ex)

        write(40,'(F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6)') &
             t, x, v, x_ex, v_ex, err_x
    end do

    close(40)
    print *, 'Gotovo. CSV fajlot e euler_improved_oscillator.csv'

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

end program euler_improved_oscillator
