#define _pr 8

module const
    public
    real(_pr), parameter :: eps = 1d-6
    real(_pr), parameter :: pi = 4*atan(1.q0)
end module const

program output
    use const
    implicit none

    procedure(term), pointer :: fptr

    fptr => term
    print *, "Dumb:     ", dsum(fptr)

    fptr => kterm
    print *, "Kummer's: ", ksum(fptr)

contains
    pure function term(int)
        integer(kind=_pr), intent(in) :: int
        real(_pr) :: term
        term = 1.q0/(int**2+1)
    end function term

    pure function kterm(int)
        integer(kind=_pr), intent(in) :: int
        real(_pr) :: kterm
        kterm = 1.q0/((int**2+1)*int**4)
    end function kterm

    function dsum(fptr)
        procedure(term), pointer, intent(in) :: fptr
        real(_pr) :: dsum, f
        integer(kind=_pr) :: i, n

        dsum = 0
        i = 0
        do while ((fptr(i)-eps) .ge. 0)
            dsum = dsum + fptr(i)
            i = i + 1
        end do
    end function dsum

    function ksum(fptr)
        procedure(term), pointer, intent(in) :: fptr
        real(_pr) :: ksum
        integer(kind=_pr) :: i

        ksum = 0
        i = 1
        do while ((fptr(i)-eps) .ge. 0)
            ksum = ksum + fptr(i)
            i = i + 1
        end do
        ksum = ksum + (pi)**2/6 - (pi)**4/90 + 1
    end function ksum
end program output


