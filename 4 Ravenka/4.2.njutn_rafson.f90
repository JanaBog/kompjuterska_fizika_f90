program njutn_rafson
  implicit none
  integer :: i
  real(8) :: x
  real(8), parameter :: eps = 1.0d-8

  x = 1.5d0
  do i = 1, 100
    if (abs(f(x)) < eps) exit
    if (abs(df(x)) < 1.0d-12) stop 'Premal izvod!'
    x = x - f(x)/df(x)
  end do

  print *, 'Koren = ', x, ' Iteracii = ', i-1

contains
  real(8) function f(x)
    real(8), intent(in) :: x
    f = x**3 - x - 2.0d0
  end function f

  real(8) function df(x)
    real(8), intent(in) :: x
    df = 3.0d0*x**2 - 1.0d0
  end function df
end program njutn_rafson
