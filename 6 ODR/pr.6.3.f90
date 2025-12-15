program euler_implicit_oscillator
    implicit none
    ! Impliciten Ojler za harmoniski oscilator:
    !   x'' = -x
    ! Sistem:
    !   x' = v
    !   v' = -x
    !
    ! Impliciten Ojler go dava sistemot:
    !   x_{n+1} = (x_n + h*v_n) / (1 + h^2)
    !   v_{n+1} = (v_n - h*x_n) / (1 + h^2)
    !
    ! Izlez vo CSV format za Excel.

    integer :: i, nsteps
    real(8) :: t0, t, tmax, h
    real(8) :: x, v, x0, v0
    real(8) :: x_ex, v_ex, err_x
    real(8) :: x_new, v_new

    print *, 'Impliciten Ojler za x'''' = -x'
    print *, 'Vnesi x0, v0:'
    read(*,*) x0, v0

    print *, 'Vnesi h i tmax:'
    read(*,*) h, tmax

    t0 = 0.0d0
    t  = t0

    nsteps = int( (tmax - t0)/h )

    x = x0
    v = v0

    open(unit=30, file='euler_implicit_oscillator.csv', status='replace', action='write')

    write(30,'(A)') 't,x_num,v_num,x_exact,v_exact,err_x'

    ! pochetna vrednost (t0)
    x_ex = x_exact(t, x0, v0)
    v_ex = v_exact(t, x0, v0)
    err_x = abs(x - x_ex)

    write(30,'(F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6)') &
         t, x, v, x_ex, v_ex, err_x

    do i = 1, nsteps

        ! Impliciten Ojler
        x_new = (x + h*v) / (1.0d0 + h*h)
        v_new = (v - h*x) / (1.0d0 + h*h)

        t = t0 + dble(i)*h
        x = x_new
        v = v_new

        x_ex = x_exact(t, x0, v0)
        v_ex = v_exact(t, x0, v0)
        err_x = abs(x - x_ex)

        write(30,'(F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6)') &
             t, x, v, x_ex, v_ex, err_x
    end do

    close(30)
    print *, 'Gotovo. CSV fajlot e euler_implicit_oscillator.csv'

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

end program euler_implicit_oscillator
