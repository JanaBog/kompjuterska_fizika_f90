program gauss_pivot
  implicit none
  integer, parameter :: n = 3
  real(8) :: a(n,n), b(n), x(n), m, temp, row(n)
  integer :: i, j, k, p

  a(1,:) = (/4d0, 1d0, 0d0/)
  a(2,:) = (/5d0, 7d0, 1d0/)
  a(3,:) = (/0d0, 1d0, 6d0/)
  b = (/6d0, 22d0, 20d0/)

  do k = 1, n-1
    ! Baranje najgolem pivot vo kolonata k
    p = k
    do i = k+1, n
      if (abs(a(i,k)) > abs(a(p,k))) p = i
    end do
    if (abs(a(p,k)) < 1d-12) stop 'Singularna matrica'

    ! Zamena na redovi (i vo b)
    row = a(k,:)
    a(k,:) = a(p,:)
    a(p,:) = row
    temp = b(k)
    b(k) = b(p)
    b(p) = temp

    do i = k+1, n
      m = a(i,k)/a(k,k)
      do j = k, n
        a(i,j) = a(i,j) - m*a(k,j)
      end do
      b(i) = b(i) - m*b(k)
    end do
  end do

  if (abs(a(n,n)) < 1d-12) stop 'Singularna matrica'
  x(n) = b(n)/a(n,n)
  do i = n-1, 1, -1
    x(i) = b(i)
    do j = i+1, n
      x(i) = x(i) - a(i,j)*x(j)
    end do
    x(i) = x(i)/a(i,i)
  end do

  print *, 'x = ', x
end program gauss_pivot
