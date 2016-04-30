!********************************************************************************
!>
!  Test for [[stringsort_module]].

    program test

    use string_sort_module

    implicit none

    character(len=14),dimension(101) :: str

    write(*,*) ''
    write(*,*) '----Case Insensitive----'
    write(*,*) ''
    write(*,*) 'recursive:'
    call initialize()
    call lexical_sort_recursive(str,case_sensitive=.false.)
    write(*,'(*(5X,A/))') str
    if (.not. list_is_sorted(str,case_sensitive=.false.)) &
            error stop 'Error: list is not sorted.'

    write(*,*) ''
    write(*,*) 'nonrecursive:'
    call initialize()
    call lexical_sort_nonrecursive(str,case_sensitive=.false.)
    write(*,'(*(5X,A/))') str
    if (.not. list_is_sorted(str,case_sensitive=.false.)) &
            error stop 'Error: list is not sorted.'

    write(*,*) ''
    write(*,*) '----Case Sensitive----'
    write(*,*) ''
    write(*,*) 'recursive:'
    call initialize()
    call lexical_sort_recursive(str,case_sensitive=.true.)
    write(*,'(*(5X,A/))') str
    if (.not. list_is_sorted(str,case_sensitive=.true.)) &
            error stop 'Error: list is not sorted.'

    write(*,*) ''
    write(*,*) 'nonrecursive:'
    call initialize()
    call lexical_sort_nonrecursive(str,case_sensitive=.true.)
    write(*,'(*(5X,A/))') str
    if (.not. list_is_sorted(str,case_sensitive=.true.)) &
            error stop 'Error: list is not sorted.'

    write(*,*) ''
    write(*,*) 'tests...'
    write(*,*) 'aab' < 'aaz'
    write(*,*) 'aaz' < 'aab'
    write(*,*) 'Aab' < 'aaz'
    write(*,*) 'aab' < 'Aaz'

    contains

    subroutine initialize()

    implicit none

        !random dictionary words

        str = [ 'roll          ',&
                'crash         ',&
                'wind          ',&
                'deafening     ',&
                'decorate      ',&
                'meddle        ',&
                'Spray         ',&
                'shrill        ',&
                'hilarious     ',&
                'present       ',&
                'planes        ',&
                'elastic       ',&
                'massive       ',&
                'tooth         ',&
                'cry           ',&
                'unwieldy      ',&
                'apparatus     ',&
                'relax         ',&
                'uttermost     ',&
                'loose         ',&
                'questionable  ',&
                'utopian       ',&
                'fix           ',&
                'challenge     ',&
                'flashy        ',&
                'amuck         ',&
                'rule          ',&
                'cattle        ',&
                'reduce        ',&
                'handsomely    ',&
                'representative',&
                'ratty         ',&
                'curious       ',&
                'strange       ',&
                'daughter      ',&
                'cup           ',&
                'wrist         ',&
                'party         ',&
                'dreary        ',&
                'bath          ',&
                'arch          ',&
                'mountainous   ',&
                'whip          ',&
                'paste         ',&
                'exuberant     ',&
                'enter         ',&
                'irritate      ',&
                'teeny-tiny    ',&
                'telephone     ',&
                'queue         ',&
                'hope          ',&
                'grubby        ',&
                'thoughtless   ',&
                'blot          ',&
                'recess        ',&
                'imperfect     ',&
                'change        ',&
                'shade         ',&
                'slow          ',&
                'oranges       ',&
                'society       ',&
                'fretful       ',&
                'peel          ',&
                'kindhearted   ',&
                'grade         ',&
                'island        ',&
                'name          ',&
                'seemly        ',&
                'Premium       ',&
                'poison        ',&
                'hope          ',&
                'grate         ',&
                'few           ',&
                'drop          ',&
                'plastic       ',&
                'sudden        ',&
                'cooing        ',&
                'confuse       ',&
                'wealthy       ',&
                'quirky        ',&
                'reminiscent   ',&
                'muddle        ',&
                'decide        ',&
                'wait          ',&
                'railway       ',&
                'yoke          ',&
                'arrive        ',&
                'loud          ',&
                'rustic        ',&
                'suit          ',&
                'torpid        ',&
                'level         ',&
                'whole         ',&
                'flow          ',&
                'bore          ',&
                'savory        ',&
                'cheer         ',&
                'rude          ',&
                'upbeat        ',&
                'drip          ',&
                'basketball    ']

    end subroutine initialize

    end program test
!********************************************************************************
