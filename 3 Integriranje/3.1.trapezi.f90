
program trapezi
  implicit none
  integer :: i, n
  real(8) :: a, b, h, s

  a = 0.0d0
  b = 1.0d0
  n = 10
  h = (b-a)/n

  s = (f(a)+f(b))/2.0d0

  do i = 1, n-1
     s = s + f(a+i*h)
  end do

  print *, 'Trapezi:', h*s

contains
  real(8) function f(x)
    real(8), intent(in) :: x
    f = 1.0d0/(1.0d0+x*x)
  end function f
end program trapezi
