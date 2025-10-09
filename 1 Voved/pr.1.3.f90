program primer1_3
  implicit none
  ! ---------------------------------------------------------------
  ! Opseg na celobrojni vrednosti (integer)
  !
  ! Del a) 
  !   Pochnuvame so n = -1 i vo sekoj chekor go udvojuvame n = n*2.
  !   Prikazuvame na ekran i, n i -n. Ova ilustrira:
  !     - rast na n do minimalniot integer (asimetrichen opseg)
  !     - deka -n ne mozhe da stane +|min| (nepostoechki broj)
  !     - po natamoshno udvojuvanje nastanuva wrap/overflow.
  !
  ! Del b) Prikaz vo k-bitna reprezentacija
  !   Za K = 8:  max = 2^(K-1) - 1 = 127
  !              min = -2^(K-1)    = -128
  ! ---------------------------------------------------------------

  integer :: i
  integer :: n          
  integer :: m, isum    
  integer, parameter :: K = 8  

  ! ------------------------- Del a) -------------------------------
  n = -1
  print '(A)', 'Del a) Udvojuvanje na n:'
  print *, 'huge(INTEGER) =', huge(n)   ! ochekuvano 2147483647 za 32-biten INT
  print '(A)', '   i                  n                  -n'
  do i = 1, 33
     n = n * 2
     write (*,'(I4,1X,I20,3X,I20)') i, n, -n
  end do

  print *
  ! ------------------------- Del b) -------------------------------
  ! K-bitna reprezentacija so znak:
  !   Suma = 1 + 2 + 4 + ... + 2^(K-2) = 2^(K-1) - 1  -> max
  !   Za K = 8: 1+2+4+8+16+32+64 = 127
  !
  ! Potoa so negativnite stepeni:
  !   -1 -2 -4 - ... - 2^(K-2) = - (2^(K-1) - 1)
  !   i ushte minus: -2^(K-1)  -> min

  ! Pozitivna suma (max)
  m    = 1
  isum = 1
  do i = 1, K-2
    m    = m * 2
    isum = isum + m
  end do
  print *, 'max (2^(K-1)-1) = ', isum       ! 127 za K=8
  print *, 'max + 1  -> wrap -> min = -2^(K-1)  (-128 za K=8)' 
  
  ! Negativna suma
  m    = -1
  isum = -1
  do i = 1, K-2
    m    = m * 2
    isum = isum + m
  end do
  print *, 'min (-2^(K-1))    = ', isum - 1  ! -128
  print *, 'min + 1 -> -127'


end program primer1_3
