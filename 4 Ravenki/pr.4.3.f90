program secant_primer
  implicit none
  ! --- Параметри на проблемот ---
  real(8), parameter :: m = 70.0d0
  real(8), parameter :: k = 15.0d0
  real(8), parameter :: eps_f = 1.0d-12
  real(8), parameter :: eps_t = 1.0d-12
  integer,  parameter :: itmax = 100

  ! --- Променливи за итерации ---
  real(8) :: t0, t1, t2, f0, f1, t_exact
  integer :: it

  ! --- Аналитичка проверка ---
  t_exact = (m/k) * log(2.0d0)

  ! --- Две почетни точки (разумни) ---
  t0 = 0.0d0
  t1 = (m/k)

  print *, '--- Секанти ---'
  print '(A,1X,ES12.4)', 't_exact =', t_exact
  print '(A)', ' iter          t              f(t)'

  do it = 1, itmax
     ! 1) Вредности на функцијата
     f0 = f_val(t0, m, k)
     f1 = f_val(t1, m, k)

     ! 2) Печатење на чекор (со тековната оцена t1)
     print '(I5,1X,ES16.8,1X,ES16.8)', it, t1, f1

     ! 3) Критериум по |f(t1)|
     if (abs(f1) < eps_f) exit

     ! 4) Заштита: избегни делење со нула
     if (abs(f1 - f0) < 1.0d-18) then
        print *, 'f1 и f0 се премногу блиски; стопирање за стабилност.'
        exit
     end if

     ! 5) Секантна формула
     t2 = t1 - f1 * (t1 - t0) / (f1 - f0)

     ! 6) Критериум по |Δt|
     if (abs(t2 - t1) < eps_t) then
        t1 = t2
        exit
     end if

     ! 7) Поместување на парот точки (напредување на секантата)
     t0 = t1
     t1 = t2
  end do

  call report(t1, t_exact)

contains
  real(8) function f_val(t, m, k)
    implicit none
    real(8), intent(in) :: t, m, k
    f_val = exp( - (k/m)*t ) - 0.5d0
  end function f_val

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
end program secant_primer
