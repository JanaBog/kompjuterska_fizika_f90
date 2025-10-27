program primer_2_1
  implicit none
  ! -------------------------------
  ! 0) Deklaracii
  ! -------------------------------
  integer            :: flag, oup
  character(len=256) :: fname
  real(8)            :: x0, x1, h, f0, f1, d1

  ! -------------------------------
  ! 1) Naslov
  ! -------------------------------
  print *, 'PROGRAM: Priblizno diferenciranje vo dve tocki (forward diff)'
  print *, 'Funkcija: y(x) = x^2'

  ! -------------------------------
  ! 2) Izbor na izlez
  ! -------------------------------
  print *, 'Izberi vid na izlez:'
  print *, ' 1) Ekran'
  print *, ' 2) Vo tekst fajl'
  read  *, flag
  if (flag == 2) then
     print *, 'Vnesi ime na fajl (na pr. "rezultat.txt"):'
     read  *, fname
     oup = 10
     open(unit=oup, file=trim(fname), status='replace', action='write')
  else
     oup = 6   ! standard output
  end if

  ! -------------------------------
  ! 3) Vlez od korisnik
  ! -------------------------------
  print *, 'Vnesi x0 i x1 (na pr. 0.0  0.1):'
  read  *, x0, x1

  ! -------------------------------
  ! 4) Ravnenki + guards
  ! -------------------------------
  h = x1 - x0
  if (abs(h) < 1.0d-15) then
     write(oup, '(A)') 'GRESHKA: |h| < 1e-15  → premnogu mal cekor. Programot se zapira.'
     if (oup /= 6) close(oup)
     stop
  end if

  ! -------------------------------
  ! 5) f0, f1 od nadvoresna funkcija
  ! -------------------------------
  f0 = f(x0)         ! y(x0) = x0^2
  f1 = f(x1)         ! y(x1) = x1^2
  d1 = (f1 - f0) / h ! dve tocki: forward razlika

  ! -------------------------------
  ! 6) Prikaz na S I T E vrednosti
  ! -------------------------------
  write(oup, '(A,1X,F24.16)') 'x0              =', x0
  write(oup, '(A,1X,F24.16)') 'x1              =', x1
  write(oup, '(A,1X,F24.16)') 'h = x1 - x0     =', h
  write(oup, '(A,1X,F24.16)') 'f0 = f(x0)      =', f0
  write(oup, '(A,1X,F24.16)') 'f1 = f(x1)      =', f1
  write(oup, '(A,1X,F24.16)') 'd1 = (f1-f0)/h  =', d1

  if (oup /= 6) then
     write(oup, '(A)') '--- KRAJ: Zapis vo fajl uspeshen.'
     close(oup)
     print *, 'Gotovo. Rezultatite se zapishani vo: ', trim(fname)
  else
     print *, 'Gotovo.'
  end if

contains

  ! y(x) = x^2 (ednostavna test-funkcija)
  real(8) function f(x)
    implicit none
    real(8), intent(in) :: x
    f = x*x
  end function f

end program
