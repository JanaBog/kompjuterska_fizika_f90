program zad3_fd_numerov_poisson
  implicit none
  integer :: n, i
  real(8) :: L, rho0, a, b, h
  real(8), allocatable :: diag(:), upper(:), lower(:), rhs(:), phi(:)
  integer :: unit
  character(len=200) :: fname

  ! Problem: phi'' = -rho0, phi(0)=0, phi(L)=0
  L = 1.0d0
  rho0 = 1.0d0
  a = 0.0d0
  b = L

  write(*,*) 'Zad3: FD + Numerov za Poisson: phi'''' = -rho0, phi(0)=0, phi(L)=0'
  write(*,*) 'Vnesi N (broj na VNATRESNI tochki, N>1):'
  read(*,*) n
  if (n <= 1) then
     write(*,*) 'Greska: N mora da bide > 1.'
     stop
  end if

  h = (b-a)/dble(n+1)  ! N vnatresni tochki

  allocate(diag(n), upper(n-1), lower(n-1), rhs(n), phi(n))

  ! Numerov za y'' = s(x):
  ! (y_{i-1} - 2 y_i + y_{i+1}) = h^2/12 * ( s_{i-1} + 10 s_i + s_{i+1} )
  !
  ! Ovde s(x) = -rho0 (konstanta), pa desnata strana e:
  ! h^2/12 * (-rho0 -10rho0 -rho0) = -h^2*rho0
  !
  ! Zatoa doagjame do klasicen sistem:
  ! -phi_{i-1} + 2 phi_i - phi_{i+1} = h^2 * rho0

  do i = 1, n
     diag(i) = 2d0
     rhs(i)  = h*h*rho0
  end do
  do i = 1, n-1
     upper(i) = -1d0
     lower(i) = -1d0
  end do

  ! Dirichlet granici: phi0=0, phi_{n+1}=0 -> samo bi dodale vo rhs, ama se 0.

  call thomas_tridiag(n, lower, diag, upper, rhs, phi)

  write(*,*) 'Vnesi ime na CSV fajl (primer: "zad3.csv"):'
  read(*,'(A)') fname
  unit = 12
  open(unit=unit, file=trim(fname), status='replace', action='write')

  write(unit,'(A)') 'i,x,phi'
  write(*,'(A)')    ' i        x           phi'

  ! Pecati i granicni tochki
  write(*,'(I3,3X,F10.6,3X,F12.8)') 0, a, 0d0
  write(unit,'(I0,A,F0.8,A,F0.12)') 0, ',', a, ',', 0d0

  do i = 1, n
     write(*,'(I3,3X,F10.6,3X,F12.8)') i, a + dble(i)*h, phi(i)
     write(unit,'(I0,A,F0.8,A,F0.12)') i, ',', a + dble(i)*h, ',', phi(i)
  end do

  write(*,'(I3,3X,F10.6,3X,F12.8)') n+1, b, 0d0
  write(unit,'(I0,A,F0.8,A,F0.12)') n+1, ',', b, ',', 0d0

  close(unit)
  write(*,*) 'Gotovo. CSV zapisano vo: ', trim(fname)

contains

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

end program zad3_fd_numerov_poisson
