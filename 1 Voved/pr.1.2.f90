program primer1_2
  implicit none
  integer :: i
  real :: x, y, y_prev
  !real(8) :: x, y, y_prev
  logical :: underflow_seen, overflow_seen

  ! ---------------------------------------------------------------
  ! Цел:
  !   Да се покаже што се случува со реални броеви кога ќе излезат
  !   од опсегот на претставување (overflow и underflow).
  !
  ! Почнуваме од Y=1 и секојпат го удвојуваме (Y=Y*2).
  ! X = 1/Y станува сè помал. Кога X ќе стане нула → UNDERFLOW.
  ! Кога Y ќе стане бесконечен (Inf) → OVERFLOW.
  !
  ! Што ќе се случи ако наместо real користиме real(8)? Провери. 
  ! ---------------------------------------------------------------

  y = 1.0
  x = 1.0 / y
  underflow_seen = .false.
  overflow_seen = .false.
  
  print '(A)', '   i            X=1/Y             Y'
  write (*,'(I4,1X,ES14.7,1X,ES14.7)') 0, x, y
  
  
  do i = 1, 5000
     y_prev = y
     y = y * 2.0
     x = 1.0 / y

     write (*,'(I4,1X,ES14.7,1X,ES14.7)') i, x, y

     ! Проверка за overflow (Y престанува да расте или станува негативен)
     if (.not. overflow_seen .and. (y <= 0.0 .or. y/2.0 /= y_prev)) then
        print *, '→ OVERFLOW: Y излегол од опсег на чекор', i
        overflow_seen = .true.
     end if
     
     ! Проверка за underflow (X = 0.0)
     if (.not. underflow_seen .and. x == 0.0) then
        print *, '→ UNDERFLOW: X падна на нула на чекор', i
        underflow_seen = .true.
     end if
     
     if (underflow_seen .and. overflow_seen) then
        exit
     end if
  end do

end program primer1_2
