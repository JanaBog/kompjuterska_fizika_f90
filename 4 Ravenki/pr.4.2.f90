program newton_primer
  implicit none
  ! --- Параметри на проблемот ---
  real(8), parameter :: m = 70.0d0
  real(8), parameter :: k = 15.0d0
  real(8), parameter :: eps_f = 1.0d-12
  real(8), parameter :: eps_t = 1.0d-12
  integer,  parameter :: itmax = 50

  ! --- Променливи за итерации ---
  real(8) :: t, f, fp, tnew, t_exact
  integer :: it

  ! --- Аналитичка проверка ---
  t_exact = (m/k) * log(2.0d0)

  ! --- Почетна претпоставка (разумно близу) ---
  t = (m/k)

  print *, '--- Njutn-Rafson ---'
  print '(A,1X,ES12.4)', 't_exact =', t_exact
  print '(A)', ' iter          t              f(t)'

  do it = 1, itmax

     f  = f_val(t,  m, k)      
     fp = fp_val(t, m, k)      

     print '(I5,1X,ES16.8,1X,ES16.8)', it, t, f

     ! Критериуми за запирање по |f|
     if (abs(f) < eps_f) exit

     if (abs(fp) < 1.0d-18) then
        print *, 'Izvodot e premal, rizik od divergencija.'
        exit
     end if

     tnew = t - f/fp

     if (abs(tnew - t) < eps_t) then
        t = tnew
        exit
     end if
     t = tnew
  end do

  call report(t, t_exact)

contains
  real(8) function f_val(t, m, k)
    implicit none
    real(8), intent(in) :: t, m, k
    f_val = exp( - (k/m)*t ) - 0.5d0
  end function f_val

  real(8) function fp_val(t, m, k)
    implicit none
    real(8), intent(in) :: t, m, k
    fp_val = - (k/m) * exp( - (k/m)*t )
  end function fp_val

  subroutine report(t_num, t_ex)
    implicit none
    real(8), intent(in) :: t_num, t_ex
    real(8) :: abs_err, rel_err
    abs_err = abs(t_num - t_ex)
    rel_err = abs_err / max(1.0d-99, abs(t_ex))
    print '(A,1X,ES12.4)', 't_num   =', t_num
    print '(A,1X,ES12.4)', 'abs_err =', abs_err
    print '(A,1X,ES12.4)', 'rel_err =', rel_err
  end subroutine report
end program newton_primer
