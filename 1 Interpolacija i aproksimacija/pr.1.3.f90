program primer1_3
  implicit none
  integer :: n, i, j, lu
  real(8) :: xq, y_interp, p
  real(8), allocatable :: xx(:), yy(:), c(:)
  character(len=128) :: fname
  real(8), parameter :: eps = 1.0d-12

  ! ---------------------------------------------------------------
  ! Cel:
  !   Primena na NJutnov INTERPOLACIONEN polinom vrz zadadeni tocki 
  !   (x_i, y_i) vcitani od fajl, i oceneta vrednost 
  !   vo proizvolen x = xq.
  !
  ! Teorija (kratko):
  !   - Kolicnik od konecni razliki so koeficientite a_k
  !     vo NJutnovata forma:
  !       P_n(x) = a_1
  !                + a_2*(x - x_1)
  !                + a_3*(x - x_1)*(x - x_2)
  !                + ...
  !                + a_n*prod_{j=1..n-1}(x - x_j)
  !
  ! Format na fajl:
  !   red 1: n
  !   redovi 2..n+1: x_i  y_i   (po eden par po red)
  !
  ! Primer fajl "tocki.dat" (sin(x) vo tri tocki):
  !   3
  !   0.0        0.0
  !   1.5707963  1.0
  !   3.1415927  0.0
  !
  ! Sto ke vidime:
  !   - interpolirana vrednost P_n(xq)
  !   - za xq vo jazol: P_n(x_i) = y_i (tocno)
  !   - nadvor od intervalot mozni oscilacii 
  ! ---------------------------------------------------------------

  print *, "Ime na fajl so tocki [default: sinus.dat]:"
  read '(A)', fname
  if (len_trim(fname) == 0) fname = 'sinus.dat'

  ! Otvori fajl so bezbedno dodelena logicka edinica (newunit)
  open(newunit=lu, file=trim(fname), status='old', action='read')
  read(lu,*) n
  if (n <= 0) then
     print *, "Greska: n mora da bide >= 1."
     close(lu); stop
  end if

  allocate(xx(n), yy(n), c(n))
  do i = 1, n
     read(lu,*) xx(i), yy(i)
  end do
  close(lu)


  print *, "Vnesi x za koj se bara interpolirana vrednost:"
  read *, xq


  ! ---------------------------------------------------------------
  ! 1) Izgradi tabela na kolicnici na konecni razliki (rav. 1.19)
  ! ---------------------------------------------------------------
  c = yy
  do j = 2, n            ! j = red na razlika (2..n)
     do i = n, j, -1     ! presmetaj odgore kon dole (vo mesto)
        c(i) = (c(i) - c(i-1)) / (xx(i) - xx(i-j+1))
     end do
  end do

  ! ---------------------------------------------------------------
  ! 2) Evaluacija P_n(xq) (rav. 1.20)
  ! ---------------------------------------------------------------
  p = c(n)
  do i = n-1, 1, -1
     p = c(i) + (xq - xx(i)) * p
  end do
  y_interp = p

  print *, "-------------------------------------------"
  print *, "Fajl:", trim(fname), "  n =", n
  print *, "Za x =", xq
  print *, "Newton P_n(x) =", y_interp
  print *, "-------------------------------------------"

  ! ---------------------------------------------------------------
  ! Zadachi:
  !
  ! 1) Testiraj:
  !      x =  pi/4  (~0.785398)  -> sin =  sqrt(2)/2 ~  0.707106
  !      x = 3*pi/4 (~2.356194)  -> sin =  sqrt(2)/2 ~  0.707106
  !    Sporedi P_n(x) so vistinskata sin(x).
  !
  ! 2) Ekstrapolacija:
  !    x = 5*pi/4 (~3.926990) -> sin = -sqrt(2)/2 ~ -0.707106
  !    Zabelezhi greska i eventualni oscilacii.
  !
  ! 3) Dodadi nova tochka vo fajlot (na pr. 3.0, 0.14112), zgolemi n,
  !    i povtorno presmetaj. Dali greskata se menuva? Kade i kolku?
  !
  ! 4) Redosled na tockite:
  !    Promeni go redosledot na redovite vo fajlot i sporedi rezultat.
  !    (Polinomot treba da e IST, no koeficientite c(k) ke bidat razlicno
  !     raspredeleni — to a e normalno, bidejki zavisat od redosledot.)
  !
  ! 5) Kvadratna funkcija:
  !    Napravi 'kvadrat.dat':
  !      3
  !      0.0  1.0
  !      2.0  4.0
  !      4.0  16.0
  !    Testiraj x=0.5 i x=1.5 — treba da sovpadne
  !    so vistinskata f(x) (bidejki polinomot e od ist red).
  ! ---------------------------------------------------------------

  deallocate(xx, yy, c)
end program
