program ekstremi
  implicit none
  integer :: i
  real(8) :: a, b, x
  real(8), parameter :: eps = 1.0d-8

  ! Ekstrem na g(x) = x**3 - 3*x: barame g'(x) = 0.
  a = 0.5d0
  b = 2.0d0
  if (dg(a)*dg(b) >= 0.0d0) stop 'Nema promena na znak!'

  do i = 1, 100
    x = (a+b)/2.0d0
    if (abs(dg(x)) < eps .or. (b-a)/2.0d0 < eps) exit

    if (dg(a)*dg(x) < 0.0d0) then
      b = x
    else
      a = x
    end if
  end do

  print *, 'Ekstrem vo x = ', x
  print *, 'g(x) = ', g(x)
  if (ddg(x) > 0.0d0) print *, 'Minimum'
  if (ddg(x) < 0.0d0) print *, 'Maksimum'

contains
  real(8) function g(x)
    real(8), intent(in) :: x
    g = x**3 - 3.0d0*x
  end function g

  real(8) function dg(x)
    real(8), intent(in) :: x
    dg = 3.0d0*x**2 - 3.0d0
  end function dg

  real(8) function ddg(x)
    real(8), intent(in) :: x
    ddg = 6.0d0*x
  end function ddg
end program ekstremi
