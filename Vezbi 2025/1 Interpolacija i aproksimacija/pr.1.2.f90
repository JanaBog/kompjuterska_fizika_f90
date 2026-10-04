program primer1_2
  implicit none
  integer :: n, i, j
  real(8) :: xq, y_interp, L
  real(8), allocatable :: xx(:), yy(:)
  character(len=128) :: fname

  ! ---------------------------------------------------------------
  ! Napravete fajl "sinus.dat" so sodrzina:
  !        3
  !        0.0   0.0
  !        1.0   0.84147
  !        2.0   0.90930
  !      (ovie vrednosti se za funkcijata sin(x))
  !
  ! Format na fajl:
  !   red 1: n
  !   redovi 2..n+1: x_i  y_i   (po eden par po red)
  !
  ! Sto ke vidime:
  !   - interpolirana vrednost vo proizvolen x (vnesen od tastatura);
  ! ---------------------------------------------------------------

  print *, "Ime na fajl so tocki [default: sinus.dat]:"
  read '(A)', fname
  if (len_trim(fname) == 0) fname = 'sinus.dat'

  ! Otvori fajl i vcitaj n i tockite
  open(10, file=trim(fname), status='old', action='read')
  read(10,*) n
  allocate(xx(n), yy(n))
  do i = 1, n
     read(10,*) xx(i), yy(i)
  end do
  close(10)

  print *, "Vnesi x za koj se bara interpolirana vrednost:"
  read *, xq

  ! ---------------------------------------------------------------
  ! Lagranzova interpolacija:
  !   P_n(x) = sum_i [ y_i * L_i(x) ],
  !   L_i(x) = product_{j!=i} (x - x_j)/(x_i - x_j)
  ! ---------------------------------------------------------------
  y_interp = 0.0d0
  do i = 1, n
     L = 1.0d0
     do j = 1, n
        if (j /= i) L = L * (xq - xx(j)) / (xx(i) - xx(j))
     end do
     y_interp = y_interp + yy(i) * L
  end do

  print *, "-------------------------------------------"
  print *, "Fajl:", trim(fname), "  n =", n
  print *, "Za x =", xq
  print *, "Interpolirano y =", y_interp
  print *, "-------------------------------------------"

  ! ---------------------------------------------------------------
  ! Zadachi (so "lesni" referentni vrednosti i kratko vreme na cas):
  !
  ! 1) Proverete ja interpoliranata vrednost za x = 1.57 (pi/2). 
  !    Sporedete ja interpoliranata vrednost so vistinskata sin(x) = 1.
  !    
  ! 2) Ekstrapolacija nadvor od intervalot [0, pi]:
  !    Testirajte:
  !      x = 5*pi/4 (~3.926990) -> sin = -sqrt(2)/2 ~ -0.707106
  !    Zabelezhete dali polinomot oscilira i kolkava e greskata.
  !
  ! 3) Redosled na tockite:
  !    Promenete go redosledot na redovite vo fajlot.
  !    Dali rezultatot se menuva? (Treba DA NE.)
  !
  ! 4) Dodadete ushte edna tochka (3.0, 0.14112)
  !    Sto se slucuva so greskata?
  !
  ! 5) Druga funkcija (kvadratna):
  !    Kreirajte nova datoteka 'kvadrat.dat' so tocki od f(x) = x^2
  !      Primer: n=3; (0,0), (2,4), (4,16)
  !    Proverete deka interpolacijata gi vrakja ISTITE vrednosti
  !    vo vnesenite tocki (tocna interpolacija) i testirajte drugi
  !    vrednosti (na pr. x=3, x=5) kade polinomot treba da vrati
  !    isto kako vistinskata f(x) (bidejki e od ist red).
  ! ---------------------------------------------------------------

  deallocate(xx, yy)
end program 
