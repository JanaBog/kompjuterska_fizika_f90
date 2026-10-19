
program vezba_2_diferenciranje
    implicit none

    integer :: i, ios
    real(8) :: x, h, h_tek
    real(8) :: d_forward, d_central, d_second, d_half, d_rich
    real(8) :: exact_first, exact_second
    real(8) :: err_forward, err_central, err_second, err_rich
    real(8) :: y_extrap, err_extrap

    print *, 'Vnesi x i poceten cekor h (na primer: 1.0 0.2):'
    read(*, *, iostat=ios) x, h

    if (ios /= 0) stop 'Greska pri vnesuvanjeto.'
    if (h <= 0.0d0) stop 'Cekorot h mora da bide pozitiven.'

    ! Analiticki izvodi za f(x) = sin(x)
    exact_first  = cos(x)
    exact_second = -sin(x)

    ! Datoteka za rezultatite
    open(unit=10, file='rezultati_diferenciranje.csv', &
         status='replace', action='write', iostat=ios)

    if (ios /= 0) stop 'Ne moze da se otvori CSV datotekata.'

    write(10,'(A)') 'h,forward,central,second,richardson,' // &
                    'error_forward,error_central,error_second,error_richardson,' // &
                    'linear_extrap,error_extrap'

    print *, 'Funkcija: f(x)=sin(x)'
    print '(A,F14.10)', 'Tocen prv izvod  = ', exact_first
    print '(A,F14.10)', 'Tocen vtor izvod = ', exact_second
    print *
    print '(A)', '       h       err-forward   err-central    err-second      err-rich'

    ! Poceten cekor
    h_tek = h

    ! Povtoruvanje za sest razlicni cekori
    do i = 1, 6

        ! ==========================================
        ! 1. PRV IZVOD VO DVE TOCKI
        ! ==========================================

        d_forward = (f(x+h_tek) - f(x))/h_tek

        ! ==========================================
        ! 2. PRV IZVOD VO TRI TOCKI
        ! ==========================================

        d_central = (f(x+h_tek) - f(x-h_tek))/(2.0d0*h_tek)

        ! ==========================================
        ! 3. VTOR IZVOD VO TRI TOCKI
        ! ==========================================

        d_second = (f(x+h_tek)-2.0d0*f(x)+f(x-h_tek)) &
                   /(h_tek*h_tek)

        ! ==========================================
        ! 4. LINEARNA EKSTRAPOLACIJA
        ! ==========================================

        ! Procena na f(x+h) od f(x) i f(x-h)
        ! Tockite se na ednakvi rastojanija

        y_extrap = 2.0d0*f(x) - f(x-h_tek)

        err_extrap = abs(y_extrap - f(x+h_tek))

        ! ==========================================
        ! 5. RICHARDSONOVA EKSTRAPOLACIJA
        ! ==========================================

        ! Centralna razlika so cekor h/2

        d_half = (f(x+h_tek/2.0d0)-f(x-h_tek/2.0d0)) &
                 /h_tek

        ! Richardsonovo podobruvanje

        d_rich = (4.0d0*d_half - d_central)/3.0d0

        ! ==========================================
        ! 6. APSOLUTNI GRESKI
        ! ==========================================

        err_forward = abs(d_forward - exact_first)
        err_central = abs(d_central - exact_first)
        err_second  = abs(d_second  - exact_second)
        err_rich    = abs(d_rich    - exact_first)

        ! ==========================================
        ! 7. PRIKAZ NA REZULTATITE
        ! ==========================================

        print '(5(1X,ES14.6))', h_tek, err_forward, &
             err_central, err_second, err_rich

        ! Zapis vo CSV datoteka

        write(10,'(*(ES22.13,:,","))') &
             h_tek, d_forward, d_central, &
             d_second, d_rich, &
             err_forward, err_central, err_second, &
             err_rich, y_extrap, err_extrap

        ! Namaluvanje na cekorot
        h_tek = h_tek / 2.0d0

    end do

    close(10)

    print *
    print *, 'Rezultatite se zapisani vo rezultati_diferenciranje.csv'

contains

    ! ==========================================
    ! DEFINICIJA NA FUNKCIJATA
    ! ==========================================

    real(8) function f(t)
        implicit none
        real(8), intent(in) :: t

        f = sin(t)

    end function f

end program vezba_2_diferenciranje
