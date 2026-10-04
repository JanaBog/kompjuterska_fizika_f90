program primer1_2
  implicit none
  integer :: i
  real :: x, y, y_prev
  !real(8) :: x, y, y_prev
  logical :: underflow_seen, overflow_seen

  ! ---------------------------------------------------------------
  ! Cel:
  !   Da se pokaze shto se sluchuva so realni broevi koga kje izlezat
  !   od opsegot na pretstavuvanje (overflow i underflow).
  !
  ! Pochnuvame od y=1 i go udvojuvame y = y*2.
  ! Togash, x = 1/y stanuva se pomal. Koga x kje stane nula - underflow. 
  ! Koga y kje stane beskonecnhost - overflow.
  !
  ! - Shto kje se sluchi ako namesto real koristime real(8)? Proveri.
  ! - Shto kje bide razlichno ako y go prepolovuvame y = y/2? Proveri. 
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

     ! Proverka za overflow (y prestanuva da raste ili stanuva negativen)
     if (.not. overflow_seen .and. (y <= 0.0 .or. y/2.0 /= y_prev)) then
        print *, 'OVERFLOW: y izlegol od opseg na chekor ', i
        overflow_seen = .true.
     end if
     
     ! Proverka za underflow (X = 0.0)
     if (.not. underflow_seen .and. x == 0.0) then
        print *, 'UNDERFLOW: x padna na nula na chekor', i
        underflow_seen = .true.
     end if
     
     if (underflow_seen .and. overflow_seen) then
        exit
     end if
  end do

end program primer1_2
