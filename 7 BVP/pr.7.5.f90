program zad5_tridiag_eigen_inverse_iteration
  implicit none
  integer :: n, i, it, itmax
  real(8) :: L, a, b, h, eig, eig_old, tol
  real(8), allocatable :: lower(:), diag(:), upper(:)
  real(8), allocatable :: x(:), y(:)
  real(8) :: num, den
  integer :: unit
  character(len=200) :: fname

  ! Problem: -psi'' = E psi, psi(0)=psi(L)=0
  L = 1.0d0
  a = 0.0d0
  b = L

  write(*,*) 'Zad5: Eigenvalues of tridiagonal A from -psi'''' = E*psi (Dirichlet)'
  write(*,*) 'Vnesi N (broj na VNATRESNI tochki, N>=10):'
  read(*,*) n
  if (n < 10) then
     write(*,*) 'Greska: N treba da e barem 10.'
     stop
  end if

  write(*,*) 'Vnesi tolerancija tol (primer 1e-10):'
  read(*,*) tol
  if (tol <= 0d0) then
     write(*,*) 'Greska: tol mora da bide pozitiven.'
     stop
  end if

  itmax = 2000
  h = (b-a)/dble(n+1)

  allocate(diag(n), upper(n-1), lower(n-1), x(n), y(n))

  ! A psi = E psi, A = (1/h^2)*tridiag(-1,2,-1)
  do i = 1, n
     diag(i) = 2d0/(h*h)
  end do
  do i = 1, n-1
     upper(i) = -1d0/(h*h)
     lower(i) = -1d0/(h*h)
  end do

  ! Poceten vektor (ne nula)
  do i = 1, n
     x(i) = sin(dble(i)*3.141592653589793d0/dble(n+1))
  end do
  call normalize_vec(n, x)

  eig_old = 0d0

  do it = 1, itmax
     ! Inverse iteration bez shift: resi A*y = x
     call thomas_tridiag(n, lower, diag, upper, x, y)

     call normalize_vec(n, y)
     x = y

     ! Rayleigh quotient: eig = (x^T A x)/(x^T x)
     call rayleigh_tridiag(n, lower, diag, upper, x, eig)

     if (it > 1 .and. abs(eig - eig_old) < tol) exit
     eig_old = eig
  end do

  write(*,*) 'Najmala sopstvena vrednost (pribl.) E ~ ', eig
  write(*,*) 'Iteracii: ', it

  ! Snimi eigenvector (so granici 0)
  write(*,*) 'Vnesi ime na CSV fajl (primer: "zad5.csv"):'
  read(*,'(A)') fname
  unit = 14
  open(unit=unit, file=trim(fname), status='replace', action='write')
  write(unit,'(A)') 'i,x,psi'

  write(unit,'(I0,A,F0.8,A,F0.12)') 0, ',', a, ',', 0d0
  do i = 1, n
     write(unit,'(I0,A,F0.8,A,F0.12)') i, ',', a + dble(i)*h, ',', x(i)
  end do
  write(unit,'(I0,A,F0.8,A,F0.12)') n+1, ',', b, ',', 0d0

  close(unit)
  write(*,*) 'CSV zapisano vo: ', trim(fname)

contains

  subroutine normalize_vec(n, v)
    implicit none
    integer, intent(in) :: n
    real(8), intent(inout) :: v(n)
    real(8) :: s
    integer :: i
    s = 0d0
    do i = 1, n
       s = s + v(i)*v(i)
    end do
    s = sqrt(s)
    if (s > 0d0) v = v/s
  end subroutine normalize_vec

  subroutine rayleigh_tridiag(n, a, b, c, x, eig)
    implicit none
    integer, intent(in) :: n
    real(8), intent(in) :: a(n-1), b(n), c(n-1), x(n)
    real(8), intent(out):: eig
    integer :: i
    real(8) :: ax_i, num, den

    num = 0d0
    den = 0d0
    do i = 1, n
       ax_i = b(i)*x(i)
       if (i > 1) ax_i = ax_i + a(i-1)*x(i-1)
       if (i < n) ax_i = ax_i + c(i)*x(i+1)
       num = num + x(i)*ax_i
       den = den + x(i)*x(i)
    end do
    eig = num/den
  end subroutine rayleigh_tridiag

  subroutine thomas_tridiag(n, a, b, c, d, x)
    implicit none
    integer, intent(in) :: n
    real(8), intent(in) :: a(n-1), b(n), c(n-1), d(n)
    real(8), intent(out):: x(n)
    real(8), allocatable :: cp(:), dp(:)
    integer :: i
    real(8) :: m

    allocate(cp(n-1), dp(n))

    if (abs(b(1)) < 1.0d-14) then
       write(*,*) 'Greska: b(1)=0 vo Thomas.'
       stop
    end if

    cp(1) = c(1)/b(1)
    dp(1) = d(1)/b(1)

    do i = 2, n-1
       m = b(i) - a(i-1)*cp(i-1)
       if (abs(m) < 1.0d-14) then
          write(*,*) 'Greska: pivot ~ 0 vo Thomas.'
          stop
       end if
       cp(i) = c(i)/m
       dp(i) = (d(i) - a(i-1)*dp(i-1))/m
    end do

    m = b(n) - a(n-1)*cp(n-1)
    if (abs(m) < 1.0d-14) then
       write(*,*) 'Greska: pivot ~ 0 vo Thomas (posledno).'
       stop
    end if
    dp(n) = (d(n) - a(n-1)*dp(n-1))/m

    x(n) = dp(n)
    do i = n-1, 1, -1
       x(i) = dp(i) - cp(i)*x(i+1)
    end do

    deallocate(cp, dp)
  end subroutine thomas_tridiag

end program zad5_tridiag_eigen_inverse_iteration
