program zad4_eigen_shoot_numerov_bisection
  implicit none
  integer :: n, istep, maxit
  real(8) :: L, a, b, h
  real(8) :: lam1, lam2, lam, f1, f2, fmid, tol
  real(8), allocatable :: y(:)
  integer :: unit
  character(len=200) :: fname

  ! Problem: y'' + lambda y = 0, y(0)=0, y(L)=0
  L = 1.0d0
  a = 0.0d0
  b = L

  write(*,*) 'Zad4: Eigen shooting (Numerov + bisection): y'''' + lambda*y = 0'
  write(*,*) 'Vnesi N (broj na tochki, N>=20 preporaka):'
  read(*,*) n
  if (n < 20) then
     write(*,*) 'Greska: N treba da e barem 20.'
     stop
  end if

  write(*,*) 'Vnesi interval za lambda [lam1 lam2] (primer: 1 20):'
  read(*,*) lam1, lam2
  if (lam1 <= 0d0 .or. lam2 <= lam1) then
     write(*,*) 'Greska: treba 0 < lam1 < lam2.'
     stop
  end if

  write(*,*) 'Vnesi tolerancija tol (primer 1e-8):'
  read(*,*) tol
  if (tol <= 0d0) then
     write(*,*) 'Greska: tol mora da bide pozitiven.'
     stop
  end if

  maxit = 200
  h = (b-a)/dble(n-1)
  allocate(y(n))

  call numerov_end_value(n, h, lam1, f1, y)
  call numerov_end_value(n, h, lam2, f2, y)

  if (f1*f2 > 0d0) then
     write(*,*) 'Nema promena na znak na y(L) vo dadeniot interval.'
     write(*,*) 'Probaj drug interval.'
     stop
  end if

  istep = 0
  do while (abs(lam2-lam1) > tol .and. istep < maxit)
     lam = 0.5d0*(lam1+lam2)
     call numerov_end_value(n, h, lam, fmid, y)

     if (f1*fmid <= 0d0) then
        lam2 = lam
        f2 = fmid
     else
        lam1 = lam
        f1 = fmid
     end if
     istep = istep + 1
  end do

  lam = 0.5d0*(lam1+lam2)
  call numerov_end_value(n, h, lam, fmid, y)

  write(*,*) 'Najdena lambda ~ ', lam
  write(*,*) 'y(L) = ', fmid, '  iter=', istep

  write(*,*) 'Vnesi ime na CSV fajl (primer: "zad4.csv"):'
  read(*,'(A)') fname
  unit = 13
  open(unit=unit, file=trim(fname), status='replace', action='write')
  write(unit,'(A)') 'i,x,y'

  ! Normalizacija (opcionalno) – samo za vizuelizacija
  call normalize_l2(n, h, y)

  call write_profile(unit, n, h, y)

  close(unit)
  write(*,*) 'CSV zapisano vo: ', trim(fname)

contains

  subroutine numerov_end_value(n, h, lam, yL, y)
    implicit none
    integer, intent(in) :: n
    real(8), intent(in) :: h, lam
    real(8), intent(out):: yL
    real(8), intent(out):: y(n)
    integer :: i
    real(8) :: g, c0, c1, c2

    ! y'' = -lam*y  => q = lam (konst)
    g = (h*h)*lam/12d0

    ! Pocetni uslovi: y(0)=0, y'(0)=1 -> numerov bara y(1)
    y(1) = 0d0
    y(2) = 0.01d0   ! mala vrednost (skala ne e vazna za eigenvalue)

    do i = 2, n-1
       c0 = 1d0 + g
       c1 = 2d0*(1d0 - 5d0*g)
       c2 = 1d0 + g
       y(i+1) = (c1*y(i) - c0*y(i-1)) / c2
    end do

    yL = y(n)
  end subroutine numerov_end_value

  subroutine normalize_l2(n, h, y)
    implicit none
    integer, intent(in) :: n
    real(8), intent(in) :: h
    real(8), intent(inout) :: y(n)
    integer :: i
    real(8) :: s
    s = 0d0
    do i = 1, n
       s = s + y(i)*y(i)
    end do
    s = sqrt(s*h)
    if (s > 0d0) y = y/s
  end subroutine normalize_l2

  subroutine write_profile(unit, n, h, y)
    implicit none
    integer, intent(in) :: unit, n
    real(8), intent(in) :: h
    real(8), intent(in) :: y(n)
    integer :: i
    real(8) :: x
    do i = 1, n
       x = dble(i-1)*h
       write(unit,'(I0,A,F0.8,A,F0.12)') i-1, ',', x, ',', y(i)
    end do
  end subroutine write_profile

end program zad4_eigen_shoot_numerov_bisection
