program verlet_oscillator_exercise
    implicit none
    ! Klasichen (position) Verlet za x'' = -x
    ! a(x) = -x
    !
    ! Pochhetno:
    !   x0, v0 zadadeni
    !   a0 = -x0
    !   x1 = x0 + h*v0 + 0.5*h*h*a0
    !
    ! Potoa za n >= 1:
    !   a_n = -x_n
    !   x_{n+1} = 2*x_n - x_{n-1} + h*h*a_n
    !
    ! Aproksimacija na brzina:
    !   v_n ≈ (x_n - x_{n-1}) / h

    ! TODO: deklariraj promenlivi:
    ! integer :: i, nsteps
    ! real(8) :: t0, t, tmax, h
    ! real(8) :: x0, v0
    ! real(8) :: x_prev, x_curr, x_new
    ! real(8) :: a_prev, a_curr
    ! real(8) :: v_num, x_ex, v_ex, err_x

    print *, 'Klasichen Verlet za x'''' = -x'
    print *, 'Vnesi x0, v0:'
    ! TODO: read(*,*) x0, v0

    print *, 'Vnesi h i tmax:'
    ! TODO: read(*,*) h, tmax

    ! Pochhetno vreme i broj na cekori
    ! TODO: t0 = 0.0d0
    ! TODO: t  = t0
    ! TODO: nsteps = int( (tmax - t0)/h )

    ! Pochhetna pozicija
    ! TODO: x_prev = x0
    ! Pochetna akceleracija a0 = -x0
    ! TODO: a_prev = -x_prev

    ! Prvata pozicija so Taylor (2-ri red):
    ! x_curr = x0 + h*v0 + 0.5*h*h*a0
    ! TODO: x_curr = x_prev + h*v0 + 0.5d0*h*h*a_prev

    ! Otvori CSV fajl
    open(unit=80, file='verlet_oscillator.csv', status='replace', action='write')
    write(80,'(A)') 't,x_num,v_num,x_exact,v_exact,err_x'

    ! --- Red za t = 0 ---
    ! Numerichki: x = x0, v = v0
    ! TODO: t  = t0
    ! TODO: v_num = v0
    ! TODO: x_ex = x_exact(t, x0, v0)
    ! TODO: v_ex = v_exact(t, x0, v0)
    ! TODO: err_x = abs(x_prev - x_ex)
    ! TODO: write CSV red za t0 (so x_prev i v_num)

    ! --- Red za prvata Verlet tocka t = h (so x_curr) ---
    ! Aproksimacija na v1:
    !   v1 ≈ (x1 - x0) / h
    ! TODO: t = t0 + h
    ! TODO: v_num = (x_curr - x_prev)/h
    ! TODO: x_ex = x_exact(t, x0, v0)
    ! TODO: v_ex = v_exact(t, x0, v0)
    ! TODO: err_x = abs(x_curr - x_ex)
    ! TODO: write CSV red za t = h (so x_curr i v_num)

    ! Sega imame:
    !   x_prev = x0 (za t0)
    !   x_curr = x1 (za t = h)
    ! I kje generirame x_new za t = 2h, 3h, ...

    ! Glavna petlja za i = 2..nsteps
    do i = 2, nsteps

        ! Tekovna akceleracija a_curr = -x_curr
        ! TODO: a_curr = -x_curr

        ! Verlet formula:
        ! x_new = 2*x_curr - x_prev + h*h*a_curr
        ! TODO: x_new = 2.0d0*x_curr - x_prev + h*h*a_curr

        ! Vreme za indeks i
        ! t_i = t0 + i*h
        ! TODO: t = t0 + dble(i)*h

        ! Aproksimacija na brzina vo t_i:
        ! v_i ≈ (x_i - x_{i-1})/h = (x_curr - x_prev)/h
        ! TODO: v_num = (x_curr - x_prev)/h

        ! Tocno resenie i greska za x_curr
        ! TODO: x_ex = x_exact(t, x0, v0)
        ! TODO: v_ex = v_exact(t, x0, v0)
        ! TODO: err_x = abs(x_curr - x_ex)

        ! Zapis za tockata (t_i, x_curr, v_num)
        ! TODO: write(80,'(F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6)') ...

        ! Podgotovka za sledniot cekor
        ! x_prev <= x_curr, x_curr <= x_new
        ! TODO: x_prev = x_curr
        ! TODO: x_curr = x_new

    end do

    close(80)
    print *, 'Gotovo. CSV fajlot e verlet_oscillator.csv'

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

end program verlet_oscillator_exercise
