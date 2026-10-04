program newton
   implicit none

   integer :: i, j
   integer, parameter :: n = 4

   real(8) :: x(n), y(n), c(n)
   real(8) :: xq, p

   x = (/ 1.0d0, 2.0d0, 3.0d0, 4.0d0 /)
   y = (/ 2.0d0, 4.0d0, 3.0d0, 5.0d0 /)

   ! Pocetni vrednosti
   c = y

   ! Kolicnici od konecni razliki
   do j = 2, n

      do i = n, j, -1

         c(i) = (c(i) - c(i-1)) / &
                (x(i) - x(i-j+1))

      end do

   end do

   print *, "Koeficienti:"
   do i = 1, n
      print *, i, c(i)
   end do

   print *, "Vnesi x:"
   read *, xq

   ! Evaluacija na Newtonoviot polinom
   p = c(n)

   do i = n-1, 1, -1
      p = c(i) + (xq - x(i))*p
   end do

   print *, "x =", xq
   print *, "P(x) =", p

end program newton
