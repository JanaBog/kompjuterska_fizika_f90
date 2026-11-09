program brute_force_minimum
  implicit none
  integer :: i, n
  real(8) :: a, b, h, x, xbest, fmin

  a = 0.0d0
  b = 2.0d0
  n = 200
  h = (b-a)/n
  fmin = huge(1.0d0)

  do i = 0, n
    x = a + i*h
    if (abs(f(x)) < fmin) then
      fmin = abs(f(x))
      xbest = x
    end if
  end do

  print *, 'Kandidat za koren = ', xbest
  print *, '|f(x)| = ', fmin

contains
  real(8) function f(x)
    real(8), intent(in) :: x
    f = (x-1.0d0)**2
  end function f
end program brute_force_minimum
