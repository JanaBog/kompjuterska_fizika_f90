program rk4
    implicit none
    integer :: i
    real(8) :: t, y, h, k1, k2, k3, k4

    h = 0.1d0
    t = 0.0d0
    y = 1.0d0

    print *, t, y
    do i = 1, 10
        k1 = -y
        k2 = -(y + 0.5d0*h*k1)
        k3 = -(y + 0.5d0*h*k2)
        k4 = -(y + h*k3)
        y = y + h*(k1 + 2*k2 + 2*k3 + k4)/6.0d0
        t = t + h
        print *, t, y
    end do
end program rk4
