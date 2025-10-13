program primer1_5
  implicit none
  integer :: n, i, lu
  real(8) :: a, b, xq, yq
  real(8) :: Sx, Sy, Sxx, Sxy, denom, rss, rms
  real(8), allocatable :: xx(:), yy(:)
  character(len=128) :: fname
  real(8), parameter :: eps = 1.0d-12

  ! ---------------------------------------------------------------
  ! Cel:
  !   LINEARNA APROKSIMACIJA so NAJMALI KVADRATI
  !   (fit na prava y ≈ a*x + b) nad tocki (x_i, y_i) vcitani od fajl.
  !
  !   Model so bazisni funkcii (rav. 1.52):
  !          y(x) ≈ Σ a_k * phi_k(x).
  !          Za prava: phi_1(x)=x, phi_0(x)=1  →  y ≈ a*x + b.
  !
  !   Kriterium na najmali kvadrati (rav. 1.53):
  !          S(a,b) = sum_{i=1..n} ( y_i - a*x_i - b )^2  -> min.
  !          Uslovi za minimum: dS/da = 0, dS/db = 0  -> "normalni ravenki".
  !
  !   (matricno, analogno na 1.60 za povisok red)
  !          Za linearen model ova se sveduva na 2x2 sistem so zatvoreni formuli podolu.
  !
  ! Format na fajl:
  !   red 1: n
  !   redovi 2..n+1: x_i  y_i
  ! ---------------------------------------------------------------

  print *, "Ime na fajl so tocki [default: podatoci.dat]:"
  read '(A)', fname
  if (len_trim(fname) == 0) fname = 'podatoci.dat'

  open(newunit=lu, file=trim(fname), status='old', action='read')
  read(lu,*) n
  if (n < 2) then
     print *, "Greska: n mora da bide >= 2."
     close(lu); stop
  end if

  allocate(xx(n), yy(n))
  do i = 1, n
     read(lu,*) xx(i), yy(i)
  end do
  close(lu)

  ! ---------------------------------------------------------------
  ! Postavuvanje na uslov za minimum (rav. 1.53)
  !   Za da se najde najdobro a i b se bara:
  !      dS/da = 0 ,  dS/db = 0
  !
  !   Sto vodi do normalnite ravenki (analogno na 1.60):
  !      a*sum(x_i^2) + b*sum(x_i) = sum(x_i*y_i)
  !      a*sum(x_i)   + b*n    = sum(y_i)
  !
  !   Od ovie ravenki sledat zatvorenite formuli
  !   koi se koristat podolu vo kodot.
  ! ---------------------------------------------------------------
  Sx = 0.0d0; Sy = 0.0d0; Sxx = 0.0d0; Sxy = 0.0d0
  do i = 1, n
     Sx  = Sx  + xx(i)
     Sy  = Sy  + yy(i)
     Sxx = Sxx + xx(i)*xx(i)
     Sxy = Sxy + xx(i)*yy(i)
  end do

  ! ---------------------------------------------------------------
  ! Reshenie na normalnite ravenki (zatvoreni formuli)
  !   (Dobieni od (1.53) -> 2x2 sistem; analogno na (1.60))
  !
  !        a = ( n*Sxy - Sx*Sy ) / ( n*Sxx - (Sx)^2 )
  !        b = ( Sy - a*Sx ) / n
  ! ---------------------------------------------------------------
  denom = n*Sxx - Sx*Sx
  if (abs(denom) <= eps) then
     print *, "Greska: degeneriran sistem (verojatno site x_i isti)."
     deallocate(xx, yy); stop
  end if

  a = ( n*Sxy - Sx*Sy ) / denom
  b = ( Sy - a*Sx ) / real(n,8)

  ! ---------------------------------------------------------------
  !   RMS (root-mean-square) na rezidualite E od rav. 1.56:
  !        E = sum( sqrt ((a*x_i + b))^2 / (n - 2) - y_i)
  ! ---------------------------------------------------------------
  rss = 0.0d0
  do i = 1, n
     rss = rss + ( yy(i) - (a*xx(i) + b) )**2
  end do
  if (n > 2) then
     rms = sqrt( rss / real(n-2,8) )
  else
     rms = 0.0d0
  end if

  ! ---------------------------------------------------------------
  ! Izlez
  ! ---------------------------------------------------------------
  print *, "-------------------------------------------"
  print *, "Fajl:", trim(fname), "  n =", n
  print *, "Linear LS fit: y ≈ a*x + b"
  print *, "a =", a
  print *, "b =", b
  print *, "rms =", rms
  print *, "-------------------------------------------"

  deallocate(xx, yy)
end program 
