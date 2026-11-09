program tridiagonal
  implicit none
  integer, parameter :: n = 3
  real(8) :: a(n), d(n), c(n), b(n), x(n), m
  integer :: i

  ! a: dolna, d: glavna, c: gorna dijagonala
  a = (/0d0, 5d0, 1d0/)
  d = (/4d0, 7d0, 6d0/)
  c = (/1d0, 1d0, 0d0/)
  b = (/6d0, 22d0, 20d0/)

  ! Eliminacija nanapred (Thomas)
  do i = 2, n
    m = a(i)/d(i-1)
    d(i) = d(i) - m*c(i-1)
    b(i) = b(i) - m*b(i-1)
  end do

  ! Zamena nanazad
  x(n) = b(n)/d(n)
  do i = n-1, 1, -1
    x(i) = (b(i) - c(i)*x(i+1))/d(i)
  end do

  print *, 'x = ', x
end program tridiagonal
