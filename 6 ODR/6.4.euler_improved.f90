program euler_improved
    implicit none
    integer :: i
    real(8) :: t, y, h, y_predict

    h = 0.1d0
    t = 0.0d0
    y = 1.0d0

    print *, t, y
    do i = 1, 10
        y_predict = y - h*y
        y = y + 0.5d0*h*(-y - y_predict)
        t = t + h
        print *, t, y
    end do
end program euler_improved
