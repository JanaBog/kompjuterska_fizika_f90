
program euler_improved_oscillator_exercise
    implicit none
    ! Podobren Ojler (Heun) za x'' = -x
    ! Sistem:
    !   x' = v
    !   v' = -x
    !
    ! Chekor:
    !   k1x = v_n
    !   k1v = -x_n
    !   x_star = x_n + h*k1x
    !   v_star = v_n + h*k1v
    !   k2x = v_star
    !   k2v = -x_star
    !   x_{n+1} = x_n + h/2*(k1x + k2x)
    !   v_{n+1} = v_n + h/2*(k1v + k2v)

    ! TODO: deklariraj gi promenlivite:
    ! integer: i, nsteps
    ! real(8): t0, t, tmax, h
    ! real(8): x, v, x0, v0, x_ex, v_ex, err_x
    ! real(8): k1x, k1v, k2x, k2v
    ! real(8): x_star, v_star
    ! mozesh i x_new, v_new ako sakash da gi razdvoish

    print *, 'Podobren Ojler (Heun) za x'''' = -x'
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

    ! Otvori CSV fajl za Excel
    open(unit=40, file='euler_improved_oscillator.csv', status='replace', action='write')

    ! Header
    write(40,'(A)') 't,x_num,v_num,x_exact,v_exact,err_x'

    ! Pochetna tocka (t0)
    ! TODO: x_ex = x_exact(t, x0, v0)
    ! TODO: v_ex = v_exact(t, x0, v0)
    ! TODO: err_x = abs(x - x_ex)
    ! TODO: zapishi eden CSV red:
    !       write(40,'(F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6)') ...

    ! Glavna petlja
    do i = 1, nsteps

        ! 1) k1
        ! TODO: k1x = v
        ! TODO: k1v = -x

        ! 2) Predviduvanje
        ! TODO: x_star = x + h*k1x
        ! TODO: v_star = v + h*k1v

        ! 3) k2
        ! TODO: k2x = v_star
        ! TODO: k2v = -x_star

        ! 4) Korigiran chekor
        ! TODO: x = x + 0.5d0*h*(k1x + k2x)
        ! TODO: v = v + 0.5d0*h*(k1v + k2v)

        ! Nov moment na vreme
        ! TODO: t = t0 + dble(i)*h

        ! Tocno resenie i greska
        ! TODO: x_ex = x_exact(t, x0, v0)
        ! TODO: v_ex = v_exact(t, x0, v0)
        ! TODO: err_x = abs(x - x_ex)

        ! CSV red
        ! TODO: write(40,'(F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6)') ...

    end do

    close(40)
    print *, 'Gotovo. CSV fajlot e euler_improved_oscillator.csv'

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

end program euler_improved_oscillator_exercise
