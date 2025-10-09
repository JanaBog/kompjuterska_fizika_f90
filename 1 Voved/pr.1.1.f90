program primer1_1
  implicit none
  ! ---------------------------------------------------------------
  ! Vezba 1 – Aproksimacija na pi so razlichna preciznost
  !
  ! Cel:
  !   Da se razbere kako razlicnite tipovi (real, real(8), DOUBLE PRECISION)
  !   ja pretstavuvaat istata vrednost so razlichna tochnost. 
  !
  ! Objasnuvanje:
  !   REAL (single precision)   - okolu 7 tochni cifri (32-biten broj)
  !   REAL(8) / DOUBLE PREC.    - okolu 15-16 tochni cifri (64-biten broj)
  !
  ! Koristime priblizna vrednost 22/7 i ja sporeduvame so vistinskata pi = 4*arctan(1).
  !
  ! Prashanja:
  !   - Kolkava e razlikata megju 22/7 i pi vo sekoja preciznost?
  !   - Dali real(8) i double precision davaat razlichen rezultat?
  !   - Shto se menuva ako koristime real(16), ako e vozmozno?
  !   - Shto znachi "tochnost" vo kontekst na ovie rezultati?
  ! ---------------------------------------------------------------

  real :: pi_single, true_pi_single, diff_single
  real(8) :: pi_real8, true_pi_real8, diff_real8
  double precision :: pi_double, true_pi_double, diff_double

  ! Priblizna vrednost (22/7)
  pi_single = 22.0 / 7.0
  pi_real8  = 22.0d0 / 7.0d0
  pi_double = 22.0d0 / 7.0d0

  ! Vistinska vrednost na pi
  true_pi_single = 4.0 * atan(1.0)
  true_pi_real8  = 4.0d0 * datan(1.0d0)
  true_pi_double = 4.0d0 * datan(1.0d0)

  ! Apsolutna greshka
  diff_single = abs(pi_single - true_pi_single)
  diff_real8  = abs(pi_real8 - true_pi_real8)
  diff_double = abs(pi_double - true_pi_double)

  ! ---------------------------------------------------------------
  ! Zapishuvanje vo fajl
  ! ---------------------------------------------------------------
  open(unit=10, file='pi_results.txt', status='replace', action='write')

  write(10,*) ' Rezultati:'
  write(10,*) ' -----------'
  write(10,'(A,F20.15)') 'REAL (single) priblizno pi = ', pi_single
  write(10,'(A,F20.15)') 'REAL(8)      priblizno pi = ', pi_real8
  write(10,'(A,F20.15)') 'DOUBLE PREC. priblizno pi = ', pi_double
  write(10,*)
  write(10,'(A,F20.15)') 'Vistinska pi (REAL single)   = ', true_pi_single
  write(10,'(A,F20.15)') 'Vistinska pi (REAL(8))       = ', true_pi_real8
  write(10,'(A,F20.15)') 'Vistinska pi (DOUBLE PREC.)  = ', true_pi_double
  write(10,*)
  write(10,*) ' Apsolutna greshka:'
  write(10,*) ' ------------------'
  write(10,'(A,ES15.8)') 'REAL (single)     -> ', diff_single
  write(10,'(A,ES15.8)') 'REAL(8)           -> ', diff_real8
  write(10,'(A,ES15.8)') 'DOUBLE PRECISION  -> ', diff_double

  close(10)

  print *, 'Rezultatite se zapishani vo fajlot "pi_results.txt".'

end program primer1_1
