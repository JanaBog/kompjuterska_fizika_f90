
program improved_euler
    implicit none

    integer :: i, n
    real(8) :: t, h, x, v, m, k

    m = 1.0d0
    k = 4.0d0
    h = 0.1d0
    n = 50

    t = 0.0d0
    x = 1.0d0
    v = 0.0d0
    
    ! Ravenka: mx'' = -kx
    
    ! Sistem: 
    ! x' = v
    ! v' = -(k/m)x

    open(10, file='euler.dat', status='replace')
    write(10,*) t, x, v

    do i = 1, n

        call euler_step(x, v, h, m, k)

        t = t + h
        write(10,*) t, x, v

    end do

    close(10)

end program improved_euler


subroutine euler_step(x, v, h, m, k)
    implicit none

    real(8), intent(inout) :: x, v
    real(8), intent(in) :: h, m, k
    real(8) :: x_pred, v_pred
    real(8) :: x_new, v_new

    ! Predviduvanje
    x_pred = x + h*v
    v_pred = v - h*(k/m)*x

    ! Korekcija
    x_new = x + 0.5d0*h*(v + v_pred)
    v_new = v - 0.5d0*h*(k/m)*(x + x_pred)

    x = x_new
    v = v_new

end subroutine euler_step
