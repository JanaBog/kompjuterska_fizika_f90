program romberg_integration
  implicit none
  integer, parameter :: maxk = 5   ! број колони (R(1,1) ... R(maxk,maxk))
  real(8) :: a, b, exact, R(maxk,maxk)
  integer :: k, j

  a = 0d0; b = 1d0
  exact = 0.7468241328124271d0

  ! R(k,1): Трапез со n = 2^(k-1) подинтервали
  R(1,1) = trap_n(a,b,1)
  do k = 2, maxk
     R(k,1) = trap_n(a,b, 2**(k-1))
     do j = 2, k
        R(k,j) = R(k,j-1) + ( R(k,j-1) - R(k-1,j-1) ) / ( 4d0**(j-1) - 1d0 )
     end do
  end do

  print *, 'ROMBERG: I ≈ ', R(maxk,maxk)
  print *, 'Грешка |I - I_exact| = ', abs(R(maxk,maxk) - exact)

contains
  function f(x) result(y)
    implicit none
    real(8), intent(in) :: x
    real(8) :: y
    y = exp(-x*x)
  end function

  function trap_n(a,b,n) result(integ)
    implicit none
    real(8), intent(in) :: a,b
    integer, intent(in) :: n
    real(8) :: integ, h, x
    integer :: i
    h = (b - a)/n
    integ = 0.5d0*( f(a) + f(b) )
    do i = 1, n-1
      x = a + i*h
      integ = integ + f(x)
    end do
    integ = integ*h
  end function
end program
