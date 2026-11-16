program taylor2
    implicit none
    integer :: i
    real(8) :: t, y, h

    h = 0.1d0
    t = 0.0d0
    y = 1.0d0

    print *, t, y
    do i = 1, 10
        ! y' = -y; y'' = y
        y = y - h*y + 0.5d0*h*h*y
        t = t + h
        print *, t, y
    end do
    
    ! Da se dodade sporedba so analitickoto resenie
end program taylor2
