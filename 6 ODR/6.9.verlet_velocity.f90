program verlet_velocity
    implicit none
    integer :: i
    real(8) :: t, h, x, v, a, a_new

    h = 0.1d0
    t = 0.0d0
    x = 1.0d0
    v = 0.0d0
    a = -x

    print *, t, x, v
    do i = 1, 10
        x = x + h*v + 0.5d0*h*h*a
        a_new = -x
        v = v + 0.5d0*h*(a + a_new)
        a = a_new
        t = t + h
        print *, t, x, v
    end do
end program verlet_velocity
