program lagrange
   implicit none

   integer :: i, j
   integer, parameter :: n = 3

   real(8) :: x(n), y(n)
   real(8) :: xq, L, y_interp

   x = (/ 1.0d0, 2.0d0, 3.0d0 /)
   y = (/ 2.0d0, 4.0d0, 3.0d0 /)

   print *, "Vnesi x za interpolacija:"
   read *, xq

   y_interp = 0.0d0

   do i = 1, n

      L = 1.0d0

      do j = 1, n

         if (j /= i) then
            L = L * (xq - x(j)) / (x(i) - x(j))
         end if

      end do

      y_interp = y_interp + y(i)*L

   end do

   print *, "x =", xq
   print *, "P(x) =", y_interp

end program lagrange
