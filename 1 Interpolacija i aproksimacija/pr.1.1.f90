program primer1_1
  implicit none
  integer :: n, i
  real(8) :: x, term, suma

  ! ---------------------------------------------------------------
  ! Cel:
  !   Da se pokaze kako funkcijata moze da se priblizi so
  !   nejziniot Tejlorov polinom okolu tochkata x0 = 0.
  !
  !   Primer: f(x) = sin(x)
  !   f(x) = f(x0) + f'(x0)*(x - x0) + f''(x0)*(x - x0)^2 / 2! + ...
  !   
  !   Za x0 = 0:
  !     sin(x) = x - x^3/3! + x^5/5! - x^7/7! + ...
  !
  !   Ova e lokalna aproksimacija: e najtochna blisku do x0 (tuka 0),
  !   a tochnosta opagja koga x e podaleku od nulata.
  !
  ! Sto ke vidime:
  !   - kako so zgolemuvanje na brojot na clenovi n,
  !     aproksimacijata se popravuva;
  !   - kako greskata zavisi od rastojanieto na x od x0 = 0.
  ! ---------------------------------------------------------------

  print *, "Vnesi x (vo radijani):"
  read *, x

  print *, "Vnesi broj na clenovi n (n >= 1):"
  read *, n

  ! ---------------------------------------------------------------
  !   Ke gi sobereme prvite n clenovi od redot:
  !
  !   Ako momentalniot clen e:
  !       T_i = (-1)^i * x^(2*i+1) / (2*i+1)!
  !
  !   Togas sledniot clen e:
  !       T_{i+1} = - T_i * x^2 / [ (2*i+2) * (2*i+3) ]
  ! ---------------------------------------------------------------

  suma  = 0.0
  term = x      

  do i = 0, n-1
     suma = suma + term
     if (i < n-1) then
        term = - term * x * x / real( (2*i+2) * (2*i+3) )
     end if
  end do

  ! ---------------------------------------------------------------
  ! Pecatenje na rezultatite i sporedba so vistinskiot sin(x)
  ! ---------------------------------------------------------------
  print *, "-------------------------------------------"
  print *, "Tejlor okolu x0 = 0 so", n, " clenovi:"
  print *, "f_Taylor =", suma
  print *, "sin(x)    =", sin(x)
  print *, "Greska    =", abs(suma - sin(x))
  print *, "-------------------------------------------"

  ! ---------------------------------------------------------------
  ! Zadachi:
  !
  ! 1) Promenete n: probajte n = 1, 2, 3, 5, 10.
  !    - Zabelezhete kako se menuva greskata.
  !
  ! 2) Promenete x: probajte x = 0.1, 1.0, 3.14.
  !    - Kade e najtochna aproksimacijata?
  !
  ! 3) Kombinirajte:
  !    - Za x = 3.14 probajte n = 5 nasproti n = 15.
  !    - Dali povisok n sekogash pomaga, i do koja mera?
  ! ---------------------------------------------------------------

end program

