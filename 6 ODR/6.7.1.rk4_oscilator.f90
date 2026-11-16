
program rk4_oscillator
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

    open(10, file='rk4.dat', status='replace')
    write(10,*) t, x, v

    do i = 1, n

        call rk4_step(x, v, h, m, k)

        t = t + h
        write(10,*) t, x, v

    end do

    close(10)

end program rk4_oscillator


subroutine rk4_step(x, v, h, m, k)
    implicit none

    real(8), intent(inout) :: x, v
    real(8), intent(in) :: h, m, k
    real(8) :: k1x, k2x, k3x, k4x
    real(8) :: k1v, k2v, k3v, k4v
    real(8) :: x_new, v_new

    ! Prva procenka
    k1x = v
    k1v = -(k/m)*x

    ! Vtora procenka
    k2x = v + 0.5d0*h*k1v
    k2v = -(k/m)*(x + 0.5d0*h*k1x)

    ! Treta procenka
    k3x = v + 0.5d0*h*k2v
    k3v = -(k/m)*(x + 0.5d0*h*k2x)

    ! Cetvrta procenka
    k4x = v + h*k3v
    k4v = -(k/m)*(x + h*k3x)

    ! Novi vrednosti
    x_new = x + h*(k1x + 2*k2x + 2*k3x + k4x)/6.0d0
    v_new = v + h*(k1v + 2*k2v + 2*k3v + k4v)/6.0d0

    x = x_new
    v = v_new

end subroutine rk4_step
