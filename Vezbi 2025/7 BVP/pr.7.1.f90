program zad1_shoot_linear
  implicit none
  integer :: n, i
  real(8) :: L, f0, a, b, alpha, beta, h, x
  real(8) :: u1, u2, v1, v2, z, y, yp
  integer :: unit
  character(len=200) :: fname

  ! Проблем: y'' = -f0, y(0)=0, y(L)=0
  L = 1.0d0
  f0 = 1.0d0
  a = 0.0d0
  b = L
  alpha = 0.0d0
  beta  = 0.0d0

  write(*,*) 'Zad1: Shooting (linear BVP): y'''' = -f0, y(0)=0, y(L)=0'
  write(*,*) 'Vnesi N (broj na podintervali, N>0):'
  read(*,*) n
  if (n <= 0) then
     write(*,*) 'Greska: N mora da bide pozitiven.'
     stop
  end if

  h = (b - a) / dble(n)

  write(*,*) 'Vnesi ime na CSV fajl (primer: "zad1.csv", ne ja zaboravaj ekstenzijata):'
  read(*,'(A)') fname
  unit = 10
  open(unit=unit, file=trim(fname), status='replace', action='write')

  write(unit,'(A)') 'i,x,y,yp'
  write(*,'(A)')    ' i        x            y            y'' '

  ! ---- 1) Resi u: u''=-f0, u(a)=alpha, u'(a)=0
  ! ---- 2) Resi v: v''=0   , v(a)=0    , v'(a)=1
  u1 = alpha
  u2 = 0.0d0
  v1 = 0.0d0
  v2 = 1.0d0

  ! Skladirame posledna tocka (b) za u(b) i v(b) -> za koeficient z
  do i = 1, n
     x = a + dble(i-1)*h
     call rk4_step_linear(x, h, f0, u1, u2, v1, v2)
  end do

  if (abs(v1) < 1.0d-14) then
     write(*,*) 'Greska: v(b) ~ 0, ne moze da se formira linearna kombinacija.'
     stop
  end if

  z = (beta - u1) / v1

  ! Povtorno integrirame za da ispecatime tabela (u,v po pat) i resenie y=u+z*v
  u1 = alpha; u2 = 0.0d0
  v1 = 0.0d0; v2 = 1.0d0

  ! i=0
  x  = a
  y  = u1 + z*v1
  yp = u2 + z*v2
  write(*,'(I3,3X,F10.6,3X,F12.8,3X,F12.8)') 0, x, y, yp
  write(unit,'(I0,A,F0.8,A,F0.12,A,F0.12)') 0, ',', x, ',', y, ',', yp

  do i = 1, n
     x = a + dble(i-1)*h
     call rk4_step_linear(x, h, f0, u1, u2, v1, v2)
     x  = a + dble(i)*h
     y  = u1 + z*v1
     yp = u2 + z*v2
     write(*,'(I3,3X,F10.6,3X,F12.8,3X,F12.8)') i, x, y, yp
     write(unit,'(I0,A,F0.8,A,F0.12,A,F0.12)') i, ',', x, ',', y, ',', yp
  end do

  close(unit)
  write(*,*) 'Gotovo. CSV zapisano vo: ', trim(fname)

contains

  subroutine rk4_step_linear(x, h, f0, u1, u2, v1, v2)
    implicit none
    real(8), intent(in) :: x, h, f0
    real(8), intent(inout) :: u1, u2, v1, v2
    real(8) :: k1u1, k1u2, k2u1, k2u2, k3u1, k3u2, k4u1, k4u2
    real(8) :: k1v1, k1v2, k2v1, k2v2, k3v1, k3v2, k4v1, k4v2
    real(8) :: t

    ! u' = u2
    ! u'' = -f0
    ! v' = v2
    ! v'' = 0

    t = x + 0.5d0*h

    k1u1 = h*u2
    k1u2 = h*(-f0)

    k2u1 = h*(u2 + 0.5d0*k1u2)
    k2u2 = h*(-f0)

    k3u1 = h*(u2 + 0.5d0*k2u2)
    k3u2 = h*(-f0)

    k4u1 = h*(u2 + k3u2)
    k4u2 = h*(-f0)

    u1 = u1 + (k1u1 + 2d0*k2u1 + 2d0*k3u1 + k4u1)/6d0
    u2 = u2 + (k1u2 + 2d0*k2u2 + 2d0*k3u2 + k4u2)/6d0

    k1v1 = h*v2
    k1v2 = h*(0d0)

    k2v1 = h*(v2 + 0.5d0*k1v2)
    k2v2 = h*(0d0)

    k3v1 = h*(v2 + 0.5d0*k2v2)
    k3v2 = h*(0d0)

    k4v1 = h*(v2 + k3v2)
    k4v2 = h*(0d0)

    v1 = v1 + (k1v1 + 2d0*k2v1 + 2d0*k3v1 + k4v1)/6d0
    v2 = v2 + (k1v2 + 2d0*k2v2 + 2d0*k3v2 + k4v2)/6d0
  end subroutine rk4_step_linear

end program zad1_shoot_linear
