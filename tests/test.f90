!********************************************************************************
!>
!  Test for [[string_sort_module]].

    program test

    use string_sort_module

    implicit none


    ! integer,parameter :: n = 8 !! number of strings to sort
    ! character(len=30),dimension(n),parameter :: strings_to_sort = &
    ! [ 'Callisto Morphamax          ',&
    ! 'Callisto Morphamax 600      ',&
    ! 'Callisto Morphamax 7000     ',&
    ! 'Callisto Morphamax 5000     ',&
    ! 'Callisto Morphamax 700      ',&
    ! 'Callisto Morphamax 6000 SE2 ',&
    ! 'Callisto Morphamax 6000 SE  ',&
    ! 'Callisto Morphamax 500      ' ]

    integer,parameter :: n = 35 !! number of strings to sort
    character(len=30),dimension(n),parameter :: strings_to_sort = &
                                      [ 'Callisto Morphamax          ',&
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
        !! Test case from [here](http://www.davekoelle.com/alphanum.html).

    character(len=30),dimension(n) :: str !! copy of `strings_to_sort` for sorting

    write(*,*) ''
    write(*,*) '----Case Insensitive----'
    write(*,*) ''
    write(*,*) 'recursive:'
    str = strings_to_sort
    call lexical_sort_recursive(str,case_sensitive=.false.)
    write(*,'(*(5X,A/))') str
    if (.not. list_is_sorted(str,case_sensitive=.false.,natural=.false.)) &
            error stop 'Error: list is not sorted.'

    ! this fails with gfortran 11
    write(*,*) ''
    write(*,*) 'nonrecursive:'
    str = strings_to_sort
    call lexical_sort_nonrecursive(str,case_sensitive=.false.)
    write(*,'(*(5X,A/))') str
    if (.not. list_is_sorted(str,case_sensitive=.false.,natural=.false.)) &
            error stop 'Error: list is not sorted.'

    write(*,*) ''
    write(*,*) '----Case Sensitive----'
    write(*,*) ''
    write(*,*) 'recursive:'
    str = strings_to_sort
    call lexical_sort_recursive(str,case_sensitive=.true.)
    write(*,'(*(5X,A/))') str
    if (.not. list_is_sorted(str,case_sensitive=.true.,natural=.false.)) &
            error stop 'Error: list is not sorted.'

    write(*,*) ''
    write(*,*) 'nonrecursive:'
    str = strings_to_sort
    call lexical_sort_nonrecursive(str,case_sensitive=.true.)
    write(*,'(*(5X,A/))') str
    if (.not. list_is_sorted(str,case_sensitive=.true.,natural=.false.)) &
            error stop 'Error: list is not sorted.'

    write(*,*) ''
    write(*,*) 'tests...'
    write(*,*) 'aab' < 'aaz'
    write(*,*) 'aaz' < 'aab'
    write(*,*) 'Aab' < 'aaz'
    write(*,*) 'aab' < 'Aaz'

    write(*,*) 'Alpha 2' < 'Alpha 200'

    end program test
!********************************************************************************
