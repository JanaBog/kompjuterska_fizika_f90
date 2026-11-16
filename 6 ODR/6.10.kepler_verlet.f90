program kepler_verlet
    implicit none
    integer :: i
    real(8) :: h, t, x, y, vx, vy, r, ax, ay
    real(8) :: r_new, ax_new, ay_new

    ! GM = 1; kruzna orbita so r=1 i v=1
    h = 0.01d0
    t = 0.0d0
    x = 1.0d0
    y = 0.0d0
    vx = 0.0d0
    vy = 1.0d0

    open(10, file='kepler.dat', status='replace')
    write(10,*) t, x, y

    do i = 1, 628
        r = sqrt(x*x + y*y)
        ax = -x/r**3
        ay = -y/r**3

        x = x + h*vx + 0.5d0*h*h*ax
        y = y + h*vy + 0.5d0*h*h*ay

        r_new = sqrt(x*x + y*y)
        ax_new = -x/r_new**3
        ay_new = -y/r_new**3

        vx = vx + 0.5d0*h*(ax + ax_new)
        vy = vy + 0.5d0*h*(ay + ay_new)
        t = t + h
        write(10,*) t, x, y
    end do

    close(10)
end program kepler_verlet
