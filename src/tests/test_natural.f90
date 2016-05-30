!********************************************************************************
!>
!  Test for [[string_sort_module]] natural sorting routines.

    program test_natural

    use string_sort_module
    use iso_fortran_env, only: ip => INT32 ! integer precision
    use iso_fortran_env, only: ip2 => INT64

    implicit none

    character(len=30),dimension(35) :: str

    write(*,*) ''
    write(*,*) 'huge(1_INT32) = ', huge(1_ip)
    write(*,*) 'huge(1_INT64) = ', huge(1_ip2)
    write(*,*) ''

    write(*,*) ''
    write(*,*) '----Case Insensitive----'
    write(*,*) ''
    write(*,*) 'normal:'
    call initialize()
    call lexical_sort_recursive(str,case_sensitive=.false.)
    write(*,'(*(5X,A/))') str
    if (.not. list_is_sorted(str,case_sensitive=.false.,natural=.false.)) &
            error stop 'Error: list is not sorted.'

    write(*,*) ''
    write(*,*) 'natural:'
    call initialize()
    call lexical_sort_natural_recursive(str,case_sensitive=.false.)
    write(*,'(*(5X,A/))') str
    if (.not. list_is_sorted(str,case_sensitive=.false.,natural=.true.)) &
            error stop 'Error: list is not sorted.'

    write(*,*) ''
    write(*,*) '----Case Sensitive----'
    write(*,*) ''
    write(*,*) 'normal:'
    call initialize()
    call lexical_sort_recursive(str,case_sensitive=.true.)
    write(*,'(*(5X,A/))') str
    if (.not. list_is_sorted(str,case_sensitive=.true.,natural=.false.)) &
            error stop 'Error: list is not sorted.'

    write(*,*) ''
    write(*,*) 'natural:'
    call initialize()
    call lexical_sort_natural_recursive(str,case_sensitive=.true.)
    write(*,'(*(5X,A/))') str
    if (.not. list_is_sorted(str,case_sensitive=.true.,natural=.true.)) &
            error stop 'Error: list is not sorted.'

    contains

    subroutine initialize()

    !! Test case from [here](http://www.davekoelle.com/alphanum.html).

    implicit none

    str = [ 'Callisto Morphamax          ',&
            'Xiph Xlater 40              ',&
            'Alpha 200                   ',&
            'Xiph Xlater 5               ',&
            'Callisto Morphamax 600      ',&
            '1000X Radonius Maximus      ',&
            'Callisto Morphamax 7000     ',&
            'Allegia 500 Clasteron       ',&
            'Allegia 51 Clasteron        ',&
            'Alpha 2                     ',&
            'Xiph Xlater 300             ',&
            'Xiph Xlater 2000            ',&
            'Alpha 2A-8000               ',&
            'Callisto Morphamax 5000     ',&
            '30X Radonius                ',&
            '10X Radonius                ',&
            'Callisto Morphamax 700      ',&
            'Alpha 100                   ',&
            'Xiph Xlater 5000            ',&
            '40X Radonius                ',&
            'Alpha 2A                    ',&
            '200X Radonius               ',&
            'Callisto Morphamax 6000 SE2 ',&
            'Allegia 6R Clasteron        ',&
            'Xiph Xlater 10000           ',&
            'Xiph Xlater 500             ',&
            'Xiph Xlater 58              ',&
            '20X Radonius Prime          ',&
            '20X Radonius                ',&
            'xiph Xlater 50              ',&
            'allegia 50 Clasteron        ',&
            'Callisto Morphamax 6000 SE  ',&
            'allegia 50B Clasteron       ',&
            'alpha 2A-900                ',&
            'Callisto Morphamax 500      ' ]

    end subroutine initialize

    end program test_natural
!********************************************************************************
