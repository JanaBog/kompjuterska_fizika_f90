! ============================================================
!  Програм: sor_method.f90
!  Цел: Решавање на систем A*x = b
!       со Successive Over-Relaxation (SOR) метод
! ============================================================

program sor_method
    implicit none
    integer, parameter :: n = 2, max_iter = 100
    real(8), parameter :: eps = 1.0d-6   ! критериум за запирање
    real(8), parameter :: omega = 1.3d0  ! релаксациски фактор (ω)
    real(8) :: A(n,n), b(n), x(n), x_old(n)
    real(8) :: sum, error, x_gs
    integer :: i, j, k

    ! ------------------------------------------------------------
    ! Пример систем:
    !   4x1 - 1x2 = 3
    !  -2x1 + 6x2 = 9
    ! ------------------------------------------------------------
    A = reshape([4.0d0, -1.0d0, &
                -2.0d0,  6.0d0], shape(A))
    b = [3.0d0, 9.0d0]

    ! Почетно приближување
    x = 0.0d0

    print *, "-------------------------------------------"
    print *, "SOR metod (omega =", omega, ")"
    print *, "-------------------------------------------"

    do k = 1, max_iter
        x_old = x   ! зачувај ги старите вредности

        ! Итерација за секоја компонента x_i
        do i = 1, n
            ! Прво пресметај ја Гаус-Зајдел поправката (x_gs)
            sum = 0.0d0
            do j = 1, n
                if (j /= i) then
                    sum = sum + A(i,j)*x(j)
                end if
            end do
            x_gs = (b(i) - sum) / A(i,i)

            ! Потоа примени SOR формула:
            ! x_new = (1 - ω)*x_old + ω*x_gs
            x(i) = (1.0d0 - omega)*x(i) + omega*x_gs
        end do

        ! Проверка на конвергенција (максимална разлика)
        error = maxval(abs(x - x_old))

        print *, "Iteracija", k, ": x1 =", x(1), " x2 =", x(2), " greska =", error

        if (error < eps) exit
    end do

    print *, "-------------------------------------------"
    print *, "krajno resenie:"
    do i = 1, n
        print *, "x(", i, ") =", x(i)
    end do
    print *, "-------------------------------------------"

end program sor_method
