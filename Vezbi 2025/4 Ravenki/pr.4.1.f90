program bisekcija_primer
  implicit none
  ! --- Параметри на проблемот ---
  real(8), parameter :: m = 70.0d0           ! маса [kg]
  real(8), parameter :: k = 15.0d0           ! коеф. отпор [kg/s]
  real(8), parameter :: eps_f = 1.0d-12      ! толеранција за |f(t)|
  real(8), parameter :: eps_t = 1.0d-12      ! толеранција за |Δt|
  integer,  parameter :: itmax = 200         ! макс. број итерации

  ! --- Променливи за итерации ---
  real(8) :: a, b, fa, fb, mid, fmid, old_mid, t_exact
  integer :: it

  ! --- Аналитичко решение за проверка ---
  t_exact = (m/k) * log(2.0d0)

  ! --- Иницијален интервал [a,b] со услов за менување знак ---
  a = 0.0d0
  b = 10.0d0
  fa = f_val(a, m, k)          
  fb = f_val(b, m, k)          
  
  ! Ако нема менување на знак, постепено проширувај b
  do while (fa*fb > 0.0d0)
     b = 2.0d0 * b
     fb = f_val(b, m, k)
     if (b > 1.0d6) then
        print *, 'Не можам да најдам интервал со менување знак.'
        stop
     end if
  end do

  print *, '--- Бисекција ---'
  print '(A,1X,ES12.4)', 't_exact =', t_exact
  print '(A)', ' iter       t_mid            f(t_mid)'

  old_mid = huge(1.0d0)
  do it = 1, itmax
     ! 1) Средина на интервалот
     mid = 0.5d0 * (a + b)

     ! 2) Вредност на функцијата во средината
     fmid = f_val(mid, m, k)

     ! 3) Печатење на чекор
     print '(I5,1X,ES16.8,1X,ES16.8)', it, mid, fmid

     ! 4) Критериуми за запирање: |f| и |Δt|
     if (abs(fmid) < eps_f) exit
     if (abs(mid - old_mid) < eps_t) exit

     ! 5) Избор на половина со менување на знак
     if (fa * fmid < 0.0d0) then
        b = mid
        fb = fmid
     else
        a = mid
        fa = fmid
     end if

     old_mid = mid
  end do

  call report(mid, t_exact)

contains
  ! Функција f(t) = exp(-(k/m)*t) - 1/2
  real(8) function f_val(t, m, k)
    implicit none
    real(8), intent(in) :: t, m, k
    f_val = exp( - (k/m)*t ) - 0.5d0
  end function f_val

  ! Извештај: решение и грешки
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
end program
