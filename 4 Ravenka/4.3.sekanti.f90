program sekanti
  implicit none
  integer :: i
  real(8) :: x0, x1, x2
  real(8), parameter :: eps = 1.0d-8

  x0 = 1.0d0
  x1 = 2.0d0
  do i = 1, 100
    if (abs(f(x1)) < eps) exit
    if (abs(f(x1)-f(x0)) < 1.0d-12) stop 'Nula vo imenitel!'

    x2 = x1 - f(x1)*(x1-x0)/(f(x1)-f(x0))
    x0 = x1
    x1 = x2
  end do

  print *, 'Koren = ', x1, ' Iteracii = ', i-1

contains
  real(8) function f(x)
    real(8), intent(in) :: x
    f = x**3 - x - 2.0d0
  end function f
end program sekanti
