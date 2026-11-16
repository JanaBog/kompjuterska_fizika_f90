program verlet_basic
    implicit none
    integer :: i
    real(8) :: t, h, x_old, x, x_new, v0

    h = 0.1d0
    x_old = 1.0d0
    v0 = 0.0d0

    ! x'' = -x; pocetni: x(0)=1, v(0)=0
    print *, 0.0d0, x_old
    x = x_old + h*v0 - 0.5d0*h*h*x_old
    print *, h, x

    do i = 2, 10
        x_new = 2.0d0*x - x_old - h*h*x
        t = i*h
        print *, t, x_new
        x_old = x
        x = x_new
    end do
end program verlet_basic
