!********************************************************************************
!>
!  Test for [[stringsort_module]].

    program test

    use string_sort_module

    implicit none

    character(len=3),dimension(11) :: str

    write(*,*) ''
    write(*,*) '----Case Insensitive----'
    write(*,*) ''
    write(*,*) 'recursive:'
    call initialize()
    call lexical_sort_recursive(str,case_sensitive=.false.)
    write(*,'(*(5X,A3/))') str
    if (.not. list_is_sorted(str,case_sensitive=.false.)) error stop 'Error: list is not sorted.'

    write(*,*) ''
    write(*,*) 'nonrecursive:'
    call initialize()
    call lexical_sort_nonrecursive(str,case_sensitive=.false.)
    write(*,'(*(5X,A3/))') str

    write(*,*) ''
    write(*,*) '----Case Sensitive----'
    write(*,*) ''
    write(*,*) 'recursive:'
    call initialize()
    call lexical_sort_recursive(str,case_sensitive=.true.)
    write(*,'(*(5X,A3/))') str

    write(*,*) ''
    write(*,*) 'nonrecursive:'
    call initialize()
    call lexical_sort_nonrecursive(str,case_sensitive=.true.)
    write(*,'(*(5X,A3/))') str

    write(*,*) ''
    write(*,*) 'tests...'
    write(*,*) 'aab' < 'aaz'
    write(*,*) 'aaz' < 'aab'
    write(*,*) 'Aab' < 'aaz'
    write(*,*) 'aab' < 'Aaz'

    contains

    subroutine initialize()

    implicit none

        str(1)  = 'bbb'
        str(2)  = 'Yyy'
        str(3)  = 'cCc'
        str(4)  = 'ooo'
        str(5)  = 'aaa'
        str(6)  = 'ppp'
        str(7)  = 'qqq'
        str(8)  = 'Ccc'
        str(9)  = 'Xxx'
        str(10) = 'vvv'
        str(11) = 'aAa'

    end subroutine initialize

    end program test
!********************************************************************************
