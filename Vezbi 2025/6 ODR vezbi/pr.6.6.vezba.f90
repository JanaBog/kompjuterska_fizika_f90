program rk2_better_oscillator_exercise
    implicit none
    ! "Podobar" RK2 za x'' = -x
    ! Sistem:
    !   x' = v
    !   v' = -x
    !
    ! Shema:
    !   k1x = v_n
    !   k1v = -x_n
    !   x2 = x_n + (2h/3)*k1x
    !   v2 = v_n + (2h/3)*k1v
    !   k2x = v2
    !   k2v = -x2
    !   x_{n+1} = x_n + h*(1/4*k1x + 3/4*k2x)
    !   v_{n+1} = v_n + h*(1/4*k1v + 3/4*k2v)

    ! TODO: deklariraj promenlivi:
    ! integer: i, nsteps
    ! real(8): t0, t, tmax, h
    ! real(8): x, v, x0, v0, x_ex, v_ex, err_x
    ! real(8): k1x, k1v, k2x, k2v
    ! real(8): x2, v2

    print *, 'Podobar RK2 za x'''' = -x'
    print *, 'Vnesi x0, v0:'
    ! TODO: read(*,*) x0, v0

    print *, 'Vnesi h i tmax:'
    ! TODO: read(*,*) h, tmax

    ! Pochhetno vreme i broj na cekori
    ! TODO: t0 = 0.0d0 ; t = t0
    ! TODO: nsteps = int( (tmax - t0)/h )

    ! Pochetna sostojba
    ! TODO: x = x0
    ! TODO: v = v0

    ! Otvori CSV fajl
    open(unit=60, file='rk2_better_oscillator.csv', status='replace', action='write')

    ! Header
    write(60,'(A)') 't,x_num,v_num,x_exact,v_exact,err_x'

    ! Pochetna tocka (t0)
    ! TODO: x_ex = x_exact(t, x0, v0)
    ! TODO: v_ex = v_exact(t, x0, v0)
    ! TODO: err_x = abs(x - x_ex)
    ! TODO: write(60,'(F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6)') ...

    ! Glavna petlja
    do i = 1, nsteps

        ! 1) k1 vo t_n
        ! TODO: k1x = v
        ! TODO: k1v = -x

        ! 2) Tochka (x2, v2) za k2
        ! TODO: x2 = x + (2.0d0/3.0d0)*h*k1x
        ! TODO: v2 = v + (2.0d0/3.0d0)*h*k1v

        ! 3) k2 vo (x2, v2)
        ! TODO: k2x = v2
        ! TODO: k2v = -x2

        ! 4) RK2 chekor
        ! TODO: x = x + h*(0.25d0*k1x + 0.75d0*k2x)
        ! TODO: v = v + h*(0.25d0*k1v + 0.75d0*k2v)

        ! Nov moment na vreme
        ! TODO: t = t0 + dble(i)*h

        ! Tocno resenie + greska
        ! TODO: x_ex = x_exact(t, x0, v0)
        ! TODO: v_ex = v_exact(t, x0, v0)
        ! TODO: err_x = abs(x - x_ex)

        ! CSV red
        ! TODO: write(60,'(F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6)') ...

    end do

    close(60)
    print *, 'Gotovo. CSV fajlot e rk2_better_oscillator.csv'

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

end program rk2_better_oscillator_exercise
