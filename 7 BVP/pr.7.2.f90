program zad2_shoot_nonlinear
  implicit none
  integer :: n, k, mmax, i
  real(8) :: L, AA, a, b, h, x
  real(8) :: tol, t, F, zL
  real(8) :: y, yp, z, zp
  integer :: unit
  character(len=200) :: fname

  ! Проблем: y'' = -y^3, y(0)=0, y(L)=A
  L = 1.0d0
  AA = 0.5d0
  a = 0.0d0
  b = L

  write(*,*) 'Zad2: Nonlinear shooting + Newton: y'''' = -y^3, y(0)=0, y(L)=A'
  write(*,*) 'Vnesi N (N>1):'
  read(*,*) n
  if (n <= 1) then
     write(*,*) 'Greska: N mora da bide > 1.'
     stop
  end if

  write(*,*) 'Vnesi tolerancija eps (primer 1e-8):'
  read(*,*) tol
  if (tol <= 0d0) then
     write(*,*) 'Greska: eps mora da bide pozitiven.'
     stop
  end if

  write(*,*) 'Vnesi max iteracii M (primer 20):'
  read(*,*) mmax
  if (mmax <= 0) then
     write(*,*) 'Greska: M mora da bide pozitiven.'
     stop
  end if

  h = (b-a)/dble(n)

  ! Pocetna pretpostavka za t = y'(0) (gruba linearn. aproks.)
  t = AA / L

  write(*,*) 'Pocetno t = ', t

  do k = 1, mmax
     call integrate_y_and_sens(n, a, h, t, y, yp, z, zp)  ! vraka vrednosti na kraj (x=L)
     F  = y - AA
     zL = z   ! z(L)=dy/dt

     write(*,'(A,I3,A,1PE12.4,A,1PE12.4,A,1PE12.4)') 'iter=',k,'  y(L)-A=',F,'  z(L)=',zL,'  t=',t

     if (abs(F) <= tol) exit
     if (abs(zL) < 1.0d-14) then
        write(*,*) 'Prekin: z(L) ~ 0, Newton e nestabilen.'
        stop
     end if

     t = t - F / zL
  end do

  if (abs(F) > tol) then
     write(*,*) 'NE konvergira vo dadeni iteracii.'
  else
     write(*,*) 'Konvergira: t = ', t, '  y(L)=', y
  end if

  ! Snimanje profil y(x), y'(x)
  write(*,*) 'Vnesi ime na CSV fajl (primer: "zad2.csv"):'
  read(*,'(A)') fname
  unit = 11
  open(unit=unit, file=trim(fname), status='replace', action='write')
  write(unit,'(A)') 'i,x,y,yp'

  y  = 0d0
  yp = t
  z  = 0d0
  zp = 1d0

  write(*,'(A)') ' i        x            y            y'' '
  write(*,'(I3,3X,F10.6,3X,F12.8,3X,F12.8)') 0, a, y, yp
  write(unit,'(I0,A,F0.8,A,F0.12,A,F0.12)') 0, ',', a, ',', y, ',', yp

  do i = 1, n
     x = a + dble(i-1)*h
     call rk4_step_nonlinear(x, h, y, yp, z, zp)
     write(*,'(I3,3X,F10.6,3X,F12.8,3X,F12.8)') i, a + dble(i)*h, y, yp
     write(unit,'(I0,A,F0.8,A,F0.12,A,F0.12)') i, ',', a + dble(i)*h, ',', y, ',', yp
  end do

  close(unit)
  write(*,*) 'Gotovo. CSV zapisano vo: ', trim(fname)

contains

  subroutine integrate_y_and_sens(n, a, h, t, yL, ypL, zL, zpL)
    implicit none
    integer, intent(in) :: n
    real(8), intent(in) :: a, h, t
    real(8), intent(out) :: yL, ypL, zL, zpL
    integer :: i
    real(8) :: x, y, yp, z, zp

    y  = 0d0
    yp = t
    z  = 0d0
    zp = 1d0

    do i = 1, n
       x = a + dble(i-1)*h
       call rk4_step_nonlinear(x, h, y, yp, z, zp)
    end do

    yL  = y
    ypL = yp
    zL  = z
    zpL = zp
  end subroutine integrate_y_and_sens

  subroutine rk4_step_nonlinear(x, h, y, yp, z, zp)
    implicit none
    real(8), intent(in) :: x, h
    real(8), intent(inout) :: y, yp, z, zp
    real(8) :: k1y, k1yp, k1z, k1zp
    real(8) :: k2y, k2yp, k2z, k2zp
    real(8) :: k3y, k3yp, k3z, k3zp
    real(8) :: k4y, k4yp, k4z, k4zp

    ! y'  = yp
    ! yp' = -y^3
    ! z'  = zp
    ! zp' = d/dt(yp') = d/dy(-y^3)*z + d/dyp( ... )*zp = (-3y^2)*z

    k1y  = h*yp
    k1yp = h*(-y**3)
    k1z  = h*zp
    k1zp = h*(-3d0*y*y*z)

    k2y  = h*(yp + 0.5d0*k1yp)
    k2yp = h*(-(y + 0.5d0*k1y)**3)
    k2z  = h*(zp + 0.5d0*k1zp)
    k2zp = h*(-3d0*(y + 0.5d0*k1y)**2*(z + 0.5d0*k1z))

    k3y  = h*(yp + 0.5d0*k2yp)
    k3yp = h*(-(y + 0.5d0*k2y)**3)
    k3z  = h*(zp + 0.5d0*k2zp)
    k3zp = h*(-3d0*(y + 0.5d0*k2y)**2*(z + 0.5d0*k2z))

    k4y  = h*(yp + k3yp)
    k4yp = h*(-(y + k3y)**3)
    k4z  = h*(zp + k3zp)
    k4zp = h*(-3d0*(y + k3y)**2*(z + k3z))

    y  = y  + (k1y  + 2d0*k2y  + 2d0*k3y  + k4y )/6d0
    yp = yp + (k1yp + 2d0*k2yp + 2d0*k3yp + k4yp)/6d0
    z  = z  + (k1z  + 2d0*k2z  + 2d0*k3z  + k4z )/6d0
    zp = zp + (k1zp + 2d0*k2zp + 2d0*k3zp + k4zp)/6d0
  end subroutine rk4_step_nonlinear

end program zad2_shoot_nonlinear
