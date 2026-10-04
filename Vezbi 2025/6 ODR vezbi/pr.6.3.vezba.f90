program euler_implicit_oscillator_exercise
    implicit none
    ! Impliciten Ojler za x'' = -x, so reshavanje na sistemot:
    ! x_{n+1} = (x_n + h*v_n) / (1 + h^2)
    ! v_{n+1} = (v_n - h*x_n) / (1 + h^2)

    ! TODO: deklariraj gi site promenlivi
    ! integer: i, nsteps
    ! real(8): t0, t, tmax, h
    ! real(8): x, v, x0, v0, x_ex, v_ex, err_x
    ! real(8): x_new, v_new

    print *, 'Impliciten Ojler za x'''' = -x'
    print *, 'Vnesi x0, v0:'
    ! TODO: read x0, v0

    print *, 'Vnesi h i tmax:'
    ! TODO: read h, tmax

    ! TODO: t0 = 0.0d0 ; t = t0
    ! TODO: nsteps = int((tmax - t0)/h)

    ! TODO: x = x0 ; v = v0

    ! Otvori CSV fajl
    open(unit=30, file='euler_implicit_oscillator.csv', status='replace', action='write')

    ! Header za Excel
    write(30,'(A)') 't,x_num,v_num,x_exact,v_exact,err_x'

    ! Inicijalna vrednost
    ! TODO: x_ex = x_exact(t, x0, v0)
    ! TODO: v_ex = v_exact(t, x0, v0)
    ! TODO: err_x = abs(x - x_ex)
    ! TODO: write initial row (CSV): write(30,'(F12.6,'','',F12.6,...)')

    do i = 1, nsteps

        ! Impliciten Ojler:
        ! x_new = (x + h*v) / (1 + h*h)
        ! v_new = (v - h*x) / (1 + h*h)

        ! TODO: compute x_new, v_new

        ! TODO: t = t0 + dble(i)*h
        ! TODO: x = x_new
        ! TODO: v = v_new

        ! TODO: compute exact solution and error
        ! TODO: write a CSV row

    end do

    close(30)
    print *, 'Gotovo. CSV fajlot e euler_implicit_oscillator.csv'

contains

    real(8) function x_exact(t, x0, v0)
        implicit none
        real(8), intent(in):: t, x0, v0
        ! TODO: x_exact = x0*cos(t) + v0*sin(t)
    end function x_exact

    real(8) function v_exact(t, x0, v0)
        implicit none
        real(8), intent(in):: t, x0, v0
        ! TODO: v_exact = -x0*sin(t) + v0*cos(t)
    end function v_exact

end program euler_implicit_oscillator_exercise
