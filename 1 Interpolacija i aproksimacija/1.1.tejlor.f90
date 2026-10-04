program tejlor_sin
   implicit none

   integer :: i, n
   real(8) :: x, term, suma, greska

   print *, "Vnesi x:"
   read *, x

   print *, "Vnesi broj na clenovi n:"
   read *, n

   suma = 0.0d0
   term = x

   do i = 0, n-1

      suma = suma + term

      if (i < n-1) then
         term = -term*x*x / real((2*i+2)*(2*i+3),8)
      end if

   end do

   greska = abs(suma - sin(x))

   print *, "Taylor =", suma
   print *, "sin(x) =", sin(x)
   print *, "Greska =", greska

end program tejlor_sin
