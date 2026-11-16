program rk2_midpoint
    implicit none
    integer :: i
    real(8) :: t, y, h, k1, k2

    h = 0.1d0
    t = 0.0d0
    y = 1.0d0

    print *, t, y
    do i = 1, 10
        k1 = -y
        k2 = -(y + 0.5d0*h*k1)
        y = y + h*k2
        t = t + h
        print *, t, y
    end do
end program rk2_midpoint
