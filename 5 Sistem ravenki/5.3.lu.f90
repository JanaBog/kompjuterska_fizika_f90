program lu
  implicit none
  integer, parameter :: n = 3
  real(8) :: a(n,n), l(n,n), u(n,n), b(n), y(n), x(n)
  integer :: i, j, k, p

  a(1,:) = (/4d0, 1d0, 0d0/)
  a(2,:) = (/5d0, 7d0, 1d0/)
  a(3,:) = (/0d0, 1d0, 6d0/)
  b = (/6d0, 22d0, 20d0/)

  ! Doolittle: a = l*u, dijagonalata na l e 1
  l = 0d0
  u = 0d0
  do k = 1, n
    l(k,k) = 1d0
    do j = k, n
      u(k,j) = a(k,j)
      do p = 1, k-1
        u(k,j) = u(k,j) - l(k,p)*u(p,j)
      end do
    end do
    if (abs(u(k,k)) < 1d-12) stop 'Potreben e pivot'
    do i = k+1, n
      l(i,k) = a(i,k)
      do p = 1, k-1
        l(i,k) = l(i,k) - l(i,p)*u(p,k)
      end do
      l(i,k) = l(i,k)/u(k,k)
    end do
  end do

  ! l*y = b (zamena nanapred)
  do i = 1, n
    y(i) = b(i)
    do j = 1, i-1
      y(i) = y(i) - l(i,j)*y(j)
    end do
  end do

  ! u*x = y (zamena nanazad)
  do i = n, 1, -1
    x(i) = y(i)
    do j = i+1, n
      x(i) = x(i) - u(i,j)*x(j)
    end do
    x(i) = x(i)/u(i,i)
  end do

  print *, 'x = ', x
end program lu
