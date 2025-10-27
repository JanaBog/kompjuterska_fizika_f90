program primer_2_2
  implicit none
  ! -------------------------------
  ! 0) Deklaracii
  ! -------------------------------
  integer            :: flag, oup
  character(len=256) :: fname
  real(8)            :: x0, h, xm, xp, f0m, f0p, d1

  ! -------------------------------
  ! 1) Naslov
  ! -------------------------------
  print *, 'PROGRAM: Priblizno diferenciranje vo tri tocki (centralna razlika)'
  print *, 'Funkcija: y(x) = x^2'

  ! -------------------------------
  ! 2) Izbor na izlez
  ! -------------------------------
  print *, 'Izberi vid na izlez:'
  print *, ' 1) Ekran'
  print *, ' 2) Vo tekst fajl'
  read  *, flag
  if (flag == 2) then
     print *, 'Vnesi ime na fajl (na pr. "rezultat_tri_tocki.txt"):'
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
  ! 5) f(x0±h) od funkciја
  ! -------------------------------
  f0m = f(xm)
  f0p = f(xp)

  ! -------------------------------
  ! 6) Tri tocki (centralna)
  ! -------------------------------
  d1 = (f0p - f0m) / (2.0d0*h)

  ! -------------------------------
  ! 7) Prikaz na site vrednosti
  ! -------------------------------
  write(oup, '(A,1X,F24.16)') 'x0               =', x0
  write(oup, '(A,1X,F24.16)') 'h                =', h
  write(oup, '(A,1X,F24.16)') 'xm = x0 - h      =', xm
  write(oup, '(A,1X,F24.16)') 'xp = x0 + h      =', xp
  write(oup, '(A,1X,F24.16)') 'f(xm)            =', f0m
  write(oup, '(A,1X,F24.16)') 'f(xp)            =', f0p
  write(oup, '(A,1X,F24.16)') 'd1 = [f(xp)-f(xm)]/(2h) =', d1

  if (oup /= 6) then
     write(oup, '(A)') '--- KRAJ: Zapis vo fajl uspeshen.'
     close(oup)
     print *, 'Gotovo. Rezultatite se zapishani vo: ', trim(fname)
  else
     print *, 'Gotovo.'
  end if

contains

  ! Test-funkcija: y(x) = x^2
  real(8) function f(x)
    implicit none
    real(8), intent(in) :: x
    f = x*x
  end function f

end program

