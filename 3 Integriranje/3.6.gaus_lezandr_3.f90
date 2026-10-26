
program gaus_lezandr_3
  implicit none
  real(8) :: a, b, c, d, t, integral

  a = 0.0d0
  b = 1.0d0

  c = (a+b)/2.0d0
  d = (b-a)/2.0d0
  t = sqrt(3.0d0/5.0d0)

  integral = d*( &
       (5.0d0/9.0d0)*f(c-d*t) + &
       (8.0d0/9.0d0)*f(c)     + &
       (5.0d0/9.0d0)*f(c+d*t) )

  print *, 'Gaus-Lezandr 3:', integral

contains
  real(8) function f(x)
    real(8), intent(in) :: x
    f = 1.0d0/(1.0d0+x*x)
  end function f

end program gaus_lezandr_3
