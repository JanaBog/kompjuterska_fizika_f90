program least_squares
   implicit none

   integer :: i
   integer, parameter :: n = 4

   real(8) :: x(n), y(n)
   real(8) :: Sx, Sy, Sxx, Sxy
   real(8) :: a, b
   real(8) :: g, r
   real(8) :: rss
   real(8) :: denom

   x = (/ 0.0d0, 1.0d0, 2.0d0, 3.0d0 /)
   y = (/ 1.0d0, 2.0d0, 2.0d0, 4.0d0 /)

   Sx  = 0.0d0
   Sy  = 0.0d0
   Sxx = 0.0d0
   Sxy = 0.0d0

   ! Potrebni sumi
   do i = 1, n

      Sx  = Sx  + x(i)
      Sy  = Sy  + y(i)
      Sxx = Sxx + x(i)*x(i)
      Sxy = Sxy + x(i)*y(i)

   end do

   print *, "Sx  =", Sx
   print *, "Sy  =", Sy
   print *, "Sxx =", Sxx
   print *, "Sxy =", Sxy

   ! Reshenie na normalnite ravenki
   denom = real(n,8)*Sxx - Sx*Sx

   a = (Sy*Sxx - Sx*Sxy) / denom
   b = (real(n,8)*Sxy - Sx*Sy) / denom

   print *, "g(x) = a + b*x"
   print *, "a =", a
   print *, "b =", b

   ! Ostatoci i suma na kvadrati
   rss = 0.0d0

   do i = 1, n

      g = a + b*x(i)
      r = y(i) - g

      rss = rss + r*r

      print *, "x =", x(i), &
               " y =", y(i), &
               " g =", g, &
               " r =", r

   end do

   print *, "RSS =", rss

end program least_squares
