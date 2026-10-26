
program adaptiven_simpson
  implicit none
  real(8) :: a, b, eps

  a = 0.0d0
  b = 1.0d0
  eps = 1.0d-8

  print *, 'Adaptiven Simpson:', adapt(a,b,eps)

contains
  real(8) function f(x)
    real(8), intent(in) :: x
    f = 1.0d0/(1.0d0+x*x)
  end function f

  real(8) function S(a,b)
    real(8), intent(in) :: a, b
    S = (b-a)*(f(a)+4.0d0*f((a+b)/2.0d0)+f(b))/6.0d0
  end function S

  recursive real(8) function adapt(a,b,eps) result(v)
    real(8), intent(in) :: a, b, eps
    real(8) :: m, s1, s2

    m = (a+b)/2.0d0
    s1 = S(a,b)
    s2 = S(a,m) + S(m,b)

    if (abs(s2-s1)/15.0d0 < eps) then
       v = s2 + (s2-s1)/15.0d0
    else
       v = adapt(a,m,eps/2.0d0) + adapt(m,b,eps/2.0d0)
    end if
  end function adapt
end program adaptiven_simpson
