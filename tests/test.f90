!********************************************************************************
!>
!  Test for [[string_sort_module]].

    program test

    use string_sort_module

    implicit none

    !character(len=14),dimension(101) :: str
    character(len=30),dimension(35) :: str

    write(*,*) ''
    write(*,*) '----Case Insensitive----'
    write(*,*) ''
    write(*,*) 'recursive:'
    call initialize()
    call lexical_sort_recursive(str,case_sensitive=.false.)
    write(*,'(*(5X,A/))') str
    if (.not. list_is_sorted(str,case_sensitive=.false.,natural=.false.)) &
            error stop 'Error: list is not sorted.'

    write(*,*) ''
    write(*,*) 'nonrecursive:'
    call initialize()
    call lexical_sort_nonrecursive(str,case_sensitive=.false.)
    write(*,'(*(5X,A/))') str
    if (.not. list_is_sorted(str,case_sensitive=.false.,natural=.false.)) &
            error stop 'Error: list is not sorted.'

    write(*,*) ''
    write(*,*) '----Case Sensitive----'
    write(*,*) ''
    write(*,*) 'recursive:'
    call initialize()
    call lexical_sort_recursive(str,case_sensitive=.true.)
    write(*,'(*(5X,A/))') str
    if (.not. list_is_sorted(str,case_sensitive=.true.,natural=.false.)) &
            error stop 'Error: list is not sorted.'

    write(*,*) ''
    write(*,*) 'nonrecursive:'
    call initialize()
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

    ! subroutine initialize()
    !
    ! implicit none
    !
    !     !random dictionary words
    !
    !     str = [ 'roll          ',&
    !             'crash         ',&
    !             'wind          ',&
    !             'deafening     ',&
    !             'decorate      ',&
    !             'meddle        ',&
    !             'Spray         ',&
    !             'shrill        ',&
    !             'hilarious     ',&
    !             'present       ',&
    !             'planes        ',&
    !             'elastic       ',&
    !             'massive       ',&
    !             'tooth         ',&
    !             'cry           ',&
    !             'unwieldy      ',&
    !             'apparatus     ',&
    !             'relax         ',&
    !             'uttermost     ',&
    !             'loose         ',&
    !             'questionable  ',&
    !             'utopian       ',&
    !             'fix           ',&
    !             'challenge     ',&
    !             'flashy        ',&
    !             'amuck         ',&
    !             'rule          ',&
    !             'cattle        ',&
    !             'reduce        ',&
    !             'handsomely    ',&
    !             'representative',&
    !             'ratty         ',&
    !             'curious       ',&
    !             'strange       ',&
    !             'daughter      ',&
    !             'cup           ',&
    !             'wrist         ',&
    !             'party         ',&
    !             'dreary        ',&
    !             'bath          ',&
    !             'arch          ',&
    !             'mountainous   ',&
    !             'whip          ',&
    !             'paste         ',&
    !             'exuberant     ',&
    !             'enter         ',&
    !             'irritate      ',&
    !             'teeny-tiny    ',&
    !             'telephone     ',&
    !             'queue         ',&
    !             'hope          ',&
    !             'grubby        ',&
    !             'thoughtless   ',&
    !             'blot          ',&
    !             'recess        ',&
    !             'imperfect     ',&
    !             'change        ',&
    !             'shade         ',&
    !             'slow          ',&
    !             'oranges       ',&
    !             'society       ',&
    !             'fretful       ',&
    !             'peel          ',&
    !             'kindhearted   ',&
    !             'grade         ',&
    !             'island        ',&
    !             'name          ',&
    !             'seemly        ',&
    !             'Premium       ',&
    !             'poison        ',&
    !             'hope          ',&
    !             'grate         ',&
    !             'few           ',&
    !             'drop          ',&
    !             'plastic       ',&
    !             'sudden        ',&
    !             'cooing        ',&
    !             'confuse       ',&
    !             'wealthy       ',&
    !             'quirky        ',&
    !             'reminiscent   ',&
    !             'muddle        ',&
    !             'decide        ',&
    !             'wait          ',&
    !             'railway       ',&
    !             'yoke          ',&
    !             'arrive        ',&
    !             'loud          ',&
    !             'rustic        ',&
    !             'suit          ',&
    !             'torpid        ',&
    !             'level         ',&
    !             'whole         ',&
    !             'flow          ',&
    !             'bore          ',&
    !             'savory        ',&
    !             'cheer         ',&
    !             'rude          ',&
    !             'upbeat        ',&
    !             'drip          ',&
    !             'basketball    ']
    !
    ! end subroutine initialize

    end program test
!********************************************************************************
