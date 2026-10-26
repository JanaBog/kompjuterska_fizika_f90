
program simpson
  implicit none
  integer :: i, n
  real(8) :: a, b, h, s

  a = 0.0d0
  b = 1.0d0
  n = 10
  if (mod(n,2) /= 0) stop 'n mora da e paren!'
  h = (b-a)/n

  s = f(a) + f(b)

  do i = 1, n-1
     if (mod(i,2) == 0) then
        s = s + 2.0d0*f(a+i*h)
     else
        s = s + 4.0d0*f(a+i*h)
     end if
  end do

  print *, 'Simpson:', h*s/3.0d0

contains
  real(8) function f(x)
    real(8), intent(in) :: x
    f = 1.0d0/(1.0d0+x*x)
  end function f
end program simpson
