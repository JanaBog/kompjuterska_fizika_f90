program primer1_1
  implicit none
  ! ---------------------------------------------------------------
  ! Вежба 1 – Апроксимација на π со различна прецизност
  !
  ! Цел:
  !   Да се разбере како различните типови (REAL, REAL(8), DOUBLE PRECISION)
  !   ја претставуваат истата вредност со различна точност.
  !
  ! Теорија:
  !   REAL (single precision)   → околу 7 точни цифри (32-битен број)
  !   REAL(8) / DOUBLE PREC.    → околу 15-16 точни цифри (64-битен број)
  !
  ! Користиме приближна вредност 22/7 и ја споредуваме со вистинската π = 4*arctan(1).
  !
  ! Прашања:
  !   • Колкава е разликата меѓу 22/7 и π во секоја прецизност?
  !   • Дали REAL(8) и DOUBLE PRECISION даваат ист резултат?
  !   • Што се менува ако користиме REAL(16) (ако компајлерот го поддржува)?
  !   • Што значи „точност“ во контекст на овие резултати?
  ! ---------------------------------------------------------------

  real :: pi_single, true_pi_single, diff_single
  real(8) :: pi_real8, true_pi_real8, diff_real8
  double precision :: pi_double, true_pi_double, diff_double

  ! Приближна вредност (22/7)
  pi_single = 22.0 / 7.0
  pi_real8  = 22.0d0 / 7.0d0
  pi_double = 22.0d0 / 7.0d0

  ! Вистинска вредност на π
  true_pi_single = 4.0 * atan(1.0)
  true_pi_real8  = 4.0d0 * datan(1.0d0)
  true_pi_double = 4.0d0 * datan(1.0d0)

  ! Апсолутна грешка
  diff_single = abs(pi_single - true_pi_single)
  diff_real8  = abs(pi_real8 - true_pi_real8)
  diff_double = abs(pi_double - true_pi_double)

  ! ---------------------------------------------------------------
  ! Запишување во фајл
  ! ---------------------------------------------------------------
  open(unit=10, file='pi_results.txt', status='replace', action='write')

  write(10,*) ' РЕЗУЛТАТИ:'
  write(10,*) ' -----------'
  write(10,'(A,F20.15)') 'REAL (single) приближно π = ', pi_single
  write(10,'(A,F20.15)') 'REAL(8)      приближно π = ', pi_real8
  write(10,'(A,F20.15)') 'DOUBLE PREC. приближно π = ', pi_double
  write(10,*)
  write(10,'(A,F20.15)') 'Вистинска π (REAL single)   = ', true_pi_single
  write(10,'(A,F20.15)') 'Вистинска π (REAL(8))       = ', true_pi_real8
  write(10,'(A,F20.15)') 'Вистинска π (DOUBLE PREC.)  = ', true_pi_double
  write(10,*)
  write(10,*) ' АПСОЛУТНА ГРЕШКА:'
  write(10,*) ' ------------------'
  write(10,'(A,ES15.8)') 'REAL (single)     -> ', diff_single
  write(10,'(A,ES15.8)') 'REAL(8)           -> ', diff_real8
  write(10,'(A,ES15.8)') 'DOUBLE PRECISION  -> ', diff_double

  close(10)

  print *, 'Резултатите се запишани во фајлот "pi_results.txt".'

end program primer1_1
