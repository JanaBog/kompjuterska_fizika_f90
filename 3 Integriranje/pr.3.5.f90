program gauss_legendre_demo
  implicit none
  integer :: n, i
  integer, dimension(3) :: ns = (/2, 4, 8/)
  real(8) :: a, b, integ, exact

  a = 0d0; b = 1d0
  exact = 0.7468241328124271d0
  integ = 0d0

  do i = 1, 3
   n = ns(i)
     integ = gauss_legendre_n(f,a,b,n)
     print '(a,i1,a,f20.15,a,f20.15)', 'GAUSS-',n,': I ≈ ', integ, '  err=', abs(integ-exact)
  end do

contains
  function f(x) result(y)
    implicit none
    real(8), intent(in) :: x
    real(8) :: y
    y = exp(-x*x)
  end function

  function gauss_legendre_n(fun,a,b,n) result(integ)
    implicit none
    interface
      function fun(x) result(y)
        implicit none; real(8), intent(in) :: x
        real(8) :: y
      end function
    end interface
    real(8), intent(in) :: a,b
    integer, intent(in) :: n
    real(8) :: integ, xm, xl, t
    real(8), allocatable :: x(:), w(:)
    integer :: i

    select case(n)
    case(2)
       allocate(x(2), w(2))
       x = (/ -0.5773502691896257d0,  0.5773502691896257d0 /)
       w = (/  1.0d0,                1.0d0               /)
    case(4)
       allocate(x(4), w(4))
       x = (/ -0.8611363115940526d0, -0.3399810435848563d0, 0.3399810435848563d0, 0.8611363115940526d0 /)
       w = (/  0.3478548451374539d0,  0.6521451548625461d0, 0.6521451548625461d0, 0.3478548451374539d0 /)
    case(8)
       allocate(x(8), w(8))
       x = (/ -0.9602898564975363d0, -0.7966664774136267d0, -0.5255324099163290d0, -0.1834346424956498d0, &
               0.1834346424956498d0,  0.5255324099163290d0,  0.7966664774136267d0,  0.9602898564975363d0 /)
       w = (/  0.1012285362903763d0,  0.2223810344533745d0,  0.3137066458778873d0,  0.3626837833783620d0, &
              0.3626837833783620d0,  0.3137066458778873d0,  0.2223810344533745d0,  0.1012285362903763d0 /)
    case default
       stop 'Поддржани n = 2, 4 или 8.'
    end select

    xm = 0.5d0*(a+b)    ! центар
    xl = 0.5d0*(b-a)    ! скалирање
    integ = 0d0
    do i=1, size(x)
       t = xm + xl*x(i)
       integ = integ + w(i)*fun(t)
    end do
    integ = integ*xl
    deallocate(x,w)
  end function
end program
