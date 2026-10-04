program diff_second_derivative_three_point
  implicit none
  ! -------------------------------
  ! 0) Deklaracii
  ! -------------------------------
  integer            :: flag, oup
  character(len=256) :: fname
  real(8)            :: x0, h, xm, xp, fm, f0, fp, d2

  ! -------------------------------
  ! 1) Naslov
  ! -------------------------------
  print *, 'PROGRAM: Vtor izvod vo tri tocki (centralna razlika)'
  print *, 'Funkcija: y(x) = x^2'

  ! -------------------------------
  ! 2) Izbor na izlez
  ! -------------------------------
  print *, 'Izberi vid na izlez:'
  print *, ' 1) Ekran'
  print *, ' 2) Vo tekst fajl'
  read  *, flag
  if (flag == 2) then
     print *, 'Vnesi ime na fajl (na pr. "rezultat_vtor_izvod.txt"):'
     read  *, fname
     oup = 10
     open(unit=oup, file=trim(fname), status='replace', action='write')
  else
     oup = 6   ! standard output
  end if

  ! -------------------------------
  ! 3) Vlez od korisnik
  ! -------------------------------
  print *, 'Vnesi x0 i h (na pr. 1.0  0.1):'
  read  *, x0, h

  ! -------------------------------
  ! 4) Guards + tocki
  ! -------------------------------
  if (abs(h) < 1.0d-15) then
     write(oup, '(A)') 'GRESHKA: |h| < 1e-15  → premnogu mal cekor. Programot se zapira.'
     if (oup /= 6) close(oup)
     stop
  end if

  xm = x0 - h
  xp = x0 + h

  ! -------------------------------
  ! 5) f(x0±h) i f(x0) od funkcija
  ! -------------------------------
  fm = f(xm)
  f0 = f(x0)
  fp = f(xp)

  ! -------------------------------
  ! 6) Vtor izvod (tri tocki)
  ! -------------------------------
  d2 = (fp - 2.0d0*f0 + fm) / (h*h)

  ! -------------------------------
  ! 7) Prikaz na site vrednosti
  ! -------------------------------
  write(oup, '(A,1X,F24.16)') 'x0                   =', x0
  write(oup, '(A,1X,F24.16)') 'h                    =', h
  write(oup, '(A,1X,F24.16)') 'xm = x0 - h          =', xm
  write(oup, '(A,1X,F24.16)') 'xp = x0 + h          =', xp
  write(oup, '(A,1X,F24.16)') 'f(xm)                =', fm
  write(oup, '(A,1X,F24.16)') 'f(x0)                =', f0
  write(oup, '(A,1X,F24.16)') 'f(xp)                =', fp
  write(oup, '(A,1X,F24.16)') 'd2 = [f(x+h)-2f(x)+f(x-h)]/h^2 =', d2

  if (oup /= 6) then
     write(oup, '(A)') '--- KRAJ: Zapis vo fajl uspeshen.'
     close(oup)
     print *, 'Gotovo. Rezultatite se zapishani vo: ', trim(fname)
  else
     print *, 'Gotovo.'
  end if

contains

  ! Test-funkcija: y(x) = x^2  (tocen vtor izvod: 2)
  real(8) function f(x)
    implicit none
    real(8), intent(in) :: x
    f = x*x
  end function f

end program diff_second_derivative_three_point
