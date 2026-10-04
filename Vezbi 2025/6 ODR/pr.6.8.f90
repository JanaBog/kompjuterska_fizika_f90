program verlet_oscillator
    implicit none
    ! Klasichen (position) Verlet za harmoniski oscilator:
    !   x'' = -x
    ! so a(x) = -x.
    !
    ! Chekor:
    !   a_n = -x_n
    !   x_{n+1} = 2*x_n - x_{n-1} + h^2*a_n
    !
    ! Pochhetno:
    !   x0, v0
    !   a0 = -x0
    !   x1 = x0 + h*v0 + 0.5*h^2*a0  (Taylor od 2-ri red)
    !
    ! Brzina (za izlez) ja aproksimirame kako:
    !   v_n ≈ (x_n - x_{n-1}) / h
    !
    ! Izlez vo CSV:
    !   t,x_num,v_num,x_exact,v_exact,err_x

    integer :: i, nsteps
    real(8) :: t0, t, tmax, h
    real(8) :: x0, v0
    real(8) :: x_prev, x_curr, x_new
    real(8) :: a_prev, a_curr
    real(8) :: v_num, x_ex, v_ex, err_x

    print *, 'Klasichen Verlet za x'''' = -x'
    print *, 'Vnesi x0, v0:'
    read(*,*) x0, v0

    print *, 'Vnesi h i tmax:'
    read(*,*) h, tmax

    ! Pochhetno vreme i broj na cekori
    t0 = 0.0d0
    t  = t0
    nsteps = int( (tmax - t0)/h )

    ! Pochhetna pozicija i akceleracija
    x_prev = x0
    a_prev = -x_prev

    ! Prva pozicija x1 so Taylor (2-ri red)
    x_curr = x_prev + h*v0 + 0.5d0*h*h*a_prev

    ! Otvori CSV fajl
    open(unit=80, file='verlet_oscillator.csv', status='replace', action='write')
    write(80,'(A)') 't,x_num,v_num,x_exact,v_exact,err_x'

    ! --- Red za t = 0 ---
    t     = t0
    v_num = v0
    x_ex  = x_exact(t, x0, v0)
    v_ex  = v_exact(t, x0, v0)
    err_x = abs(x_prev - x_ex)

    write(80,'(F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6)') &
         t, x_prev, v_num, x_ex, v_ex, err_x

    ! --- Red za t = h, so x_curr ---
    t     = t0 + h
    v_num = (x_curr - x_prev)/h
    x_ex  = x_exact(t, x0, v0)
    v_ex  = v_exact(t, x0, v0)
    err_x = abs(x_curr - x_ex)

    write(80,'(F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6)') &
         t, x_curr, v_num, x_ex, v_ex, err_x

    ! Sega generirame natamoso (t = 2h, 3h, ..., nsteps*h)
    do i = 2, nsteps

        ! Tekovna akceleracija a_curr = -x_curr
        a_curr = -x_curr

        ! Verlet formula
        x_new = 2.0d0*x_curr - x_prev + h*h*a_curr

        ! Vreme t_i
        t = t0 + dble(i)*h

        ! Aproksimacija na brzina vo t_i
        v_num = (x_curr - x_prev)/h

        ! Tocno resenie i greska za x_curr
        x_ex  = x_exact(t, x0, v0)
        v_ex  = v_exact(t, x0, v0)
        err_x = abs(x_curr - x_ex)

        write(80,'(F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6,'','',F12.6)') &
             t, x_curr, v_num, x_ex, v_ex, err_x

        ! Update na dvata posledni polozhbi
        x_prev = x_curr
        x_curr = x_new
    end do

    close(80)
    print *, 'Gotovo. CSV fajlot e verlet_oscillator.csv'

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

end program verlet_oscillator
