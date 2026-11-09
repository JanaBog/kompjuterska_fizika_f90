program brute_force_znak
  implicit none
  integer :: i, n
  real(8) :: a, b, h, x1, x2

  a = 0.5d0
  b = 10.0d0
  n = 200
  h = (b-a)/n

  do i = 1, n
    x1 = a + (i-1)*h
    x2 = a + i*h
    if (f(x1)*f(x2) < 0.0d0) then
      print *, 'Koren blisku do ', (x1+x2)/2.0d0
    end if
  end do

contains
  real(8) function f(x)
    real(8), intent(in) :: x
    f = sin(x)
  end function f
end program brute_force_znak
