program random_test
    implicit none
    integer :: i, j, k
    real :: x, y
    !I = int_1^10 x^2 dy
    k = 1000000
    y = 0
    do i = 1, k
      !print *, (5*rand() + 2)
      x = 9*rand() + 1
      y = y + x**2
    end do
    print *, (y*9)/k
    
    !y = a + bx
    ! b go zgolemuva intervalot (0, 1), intervalot e 1, (0,b)
    ! a go pomestuva intervalot (0+a, 1+a)
    ! zaedno y = a + bx
    
    ! Ako e od 1 do 10, intervalot e 10-1 = 9, pomestuvanjeto e 1

    

end program random_test
