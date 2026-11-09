program prepolovuvanje
  implicit none
  integer :: i
  real(8) :: a, b, c
  real(8), parameter :: eps = 1.0d-8

  a = 1.0d0
  b = 2.0d0
  if (f(a)*f(b) >= 0.0d0) stop 'Nema promena na znak!'

  do i = 1, 100
    c = (a+b)/2.0d0
    if (abs(f(c)) < eps .or. (b-a)/2.0d0 < eps) exit

    if (f(a)*f(c) < 0.0d0) then
      b = c
    else
      a = c
    end if
  end do

  print *, 'Koren = ', c, ' Iteracii = ', i

contains
  real(8) function f(x)
    real(8), intent(in) :: x
    f = x**3 - x - 2.0d0
  end function f
end program prepolovuvanje
