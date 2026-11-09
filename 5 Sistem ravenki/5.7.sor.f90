program sor
  implicit none
  integer, parameter :: n = 3
  real(8) :: a(n,n), b(n), x(n), old(n), s, omega
  integer :: i, j, k

  a(1,:) = (/4d0, 1d0, 0d0/)
  a(2,:) = (/5d0, 7d0, 1d0/)
  a(3,:) = (/0d0, 1d0, 6d0/)
  b = (/6d0, 22d0, 20d0/)
  x = 0d0
  omega = 1.1d0

  do k = 1, 1000
    old = x
    do i = 1, n
      s = b(i)
      do j = 1, n
        if (j /= i) s = s - a(i,j)*x(j)
      end do
      x(i) = (1d0-omega)*x(i) + omega*s/a(i,i)
    end do
    if (maxval(abs(x-old)) < 1d-10) exit
  end do

  print *, 'x = ', x
  print *, 'Iteracii = ', k
end program sor
