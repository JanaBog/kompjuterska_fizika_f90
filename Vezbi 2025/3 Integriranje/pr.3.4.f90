program adaptive_simpson_demo
  implicit none
  real(8) :: a, b, eps, I, exact

  a = 0d0; b = 1d0
  eps = 1d-8                   ! баран толеранс
  exact = 0.7468241328124271d0

  ! почетна Симпсонова проценка
  call integrate_adaptive(a,b,eps,I)
  print *, 'ADAPTIVE SIMPSON: I ≈ ', I
  print *, 'Грешка |I - I_exact| = ', abs(I - exact)

contains
  function f(x) result(y)
    implicit none
    real(8), intent(in) :: x
    real(8) :: y
    y = exp(-x*x)
  end function

  subroutine integrate_adaptive(a,b,eps,I)
    implicit none
    real(8), intent(in) :: a,b,eps
    real(8), intent(out) :: I
    real(8) :: c, h, fa, fb, fc, S
    c = 0.5d0*(a+b); h = b - a
    fa = f(a); fb = f(b); fc = f(c)
    S  = (h/6d0)*(fa + 4d0*fc + fb)
    I  = adaptS(f,a,b,eps,fa,fb,fc,S,20)   ! 20 нивоа се доволни за демонстрација
  end subroutine

  recursive function adaptS(f,a,b,eps,fa,fb,fc,S,depth) result(I)
    implicit none
    interface
      function f(x) result(y)
        implicit none; real(8), intent(in) :: x
        real(8) :: y
      end function
    end interface
    real(8), intent(in) :: a,b,eps,fa,fb,fc,S
    integer, intent(in) :: depth
    real(8) :: I, c, h, fd, fe, Sleft, Sright

    c = 0.5d0*(a+b); h = b - a
    fd = f(0.5d0*(a+c))
    fe = f(0.5d0*(c+b))
    Sleft  = (h/12d0)*(fa + 4d0*fd + fc)
    Sright = (h/12d0)*(fc + 4d0*fe + fb)

    if (depth<=0 .or. abs(Sleft+Sright - S) <= 15d0*eps) then
       I = Sleft + Sright + (Sleft+Sright - S)/15d0
    else
       I = adaptS(f,a,c,eps/2d0,fa,fc,fd,Sleft, depth-1) + &
           adaptS(f,c,b,eps/2d0,fc,fb,fe,Sright,depth-1)
    end if
  end function
end program
