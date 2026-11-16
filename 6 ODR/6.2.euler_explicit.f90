program euler_explicit
    implicit none
    integer :: i
    real(8) :: t, y, h

    h = 0.1d0
    t = 0.0d0
    y = 1.0d0

    print *, t, y
    do i = 1, 10
    !y' = -y
        y = y - h*y
        t = t + h
        print *, t, y
    end do
end program euler_explicit
