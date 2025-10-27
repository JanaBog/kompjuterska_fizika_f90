program simpson_rule
  implicit none
  integer :: n, i
  real(8) :: a, b, h, s, s_odd, s_even, exact


  a = 0d0; b = 1d0
  exact = 0.7468241328124271d0
  n = 100                     ! мора да е ПАРЕН број
  if (mod(n,2)/=0) stop 'Simpson: n мора да е парен.'

  h = (b - a)/n
  s_odd  = 0d0   ! збир на f(x_1), f(x_3), ...
  s_even = 0d0   ! збир на f(x_2), f(x_4), ...

  do i = 1, n-1
     if (mod(i,2)==1) then
        s_odd  = s_odd  + f(a + i*h)
     else
        s_even = s_even + f(a + i*h)
     end if
  end do

  s = (h/3d0) * ( f(a) + 4d0*s_odd + 2d0*s_even + f(b) )

  print *, 'SIMPSON: I ≈ ', s
  print *, 'Грешка |I - I_exact| = ', abs(s - exact)
contains
  function f(x) result(y)
    implicit none
    real(8), intent(in) :: x
    real(8) :: y
    y = exp(-x*x)
  end function
end program
