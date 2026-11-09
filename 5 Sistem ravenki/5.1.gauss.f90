program gauss
  implicit none
  integer, parameter :: n = 3
  real(8) :: a(n,n), b(n), x(n), m
  integer :: i, j, k

  a(1,:) = (/4d0, 1d0, 0d0/)
  a(2,:) = (/5d0, 7d0, 1d0/)
  a(3,:) = (/0d0, 1d0, 6d0/)
  b = (/6d0, 22d0, 20d0/)

  ! Eliminacija nanapred
  do k = 1, n-1
    if (abs(a(k,k)) < 1d-12) stop 'Nulta pivotska vrednost'
    do i = k+1, n
      m = a(i,k)/a(k,k)
      do j = k, n
        a(i,j) = a(i,j) - m*a(k,j)
      end do
      b(i) = b(i) - m*b(k)
    end do
  end do

  ! Zamena nanazad
  if (abs(a(n,n)) < 1d-12) stop 'Nulta pivotska vrednost'
  x(n) = b(n)/a(n,n)
  do i = n-1, 1, -1
    x(i) = b(i)
    do j = i+1, n
      x(i) = x(i) - a(i,j)*x(j)
    end do
    x(i) = x(i)/a(i,i)
  end do

  print *, 'x = ', x
end program gauss
