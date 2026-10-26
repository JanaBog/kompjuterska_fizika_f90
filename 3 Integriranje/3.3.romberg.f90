
program romberg
  implicit none
  integer, parameter :: m = 5
  integer :: i, j, k, n
  real(8) :: a, b, h, s, r(m,m)

  a = 0.0d0
  b = 1.0d0
  r = 0.0d0
  r(1,1) = (b-a)*(f(a)+f(b))/2.0d0

  do k = 2, m
     n = 2**(k-1)
     h = (b-a)/n
     s = 0.0d0

     do i = 1, n-1, 2
        s = s + f(a+i*h)
     end do

     r(k,1) = r(k-1,1)/2.0d0 + h*s

     do j = 2, k
        r(k,j) = r(k,j-1) + &
          (r(k,j-1)-r(k-1,j-1))/(4.0d0**(j-1)-1.0d0)
     end do
  end do

  print *, 'Romberg:', r(m,m)

contains
  real(8) function f(x)
    real(8), intent(in) :: x
    f = 1.0d0/(1.0d0+x*x)
  end function f
end program romberg
