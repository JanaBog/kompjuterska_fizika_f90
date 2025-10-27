program trap_rule
  implicit none
  integer :: n, i
  real(8) :: a, b, h, x, s, exact

  ! Функција за тест: f(x)=e^{-x^2}, интеграл на [0,1] ~ 0.7468241328124271

  a = 0d0; b = 1d0
  exact = 0.7468241328124271d0   ! референтна вредност (за споредба)
  n = 100                        ! број на подинтервали (слободно сменете)

  h = (b - a)/n
  s = 0.5d0*( f(a) + f(b) )
  do i = 1, n-1
    x = a + i*h
    s = s + f(x)
  end do
  s = s*h

  print *, 'TRAPEZ: I ≈ ', s
  print *, 'Грешка |I - I_exact| = ', abs(s - exact)
contains
  function f(x) result(y)
    implicit none
    real(8), intent(in) :: x
    real(8) :: y
    y = exp(-x*x)
  end function
end program
