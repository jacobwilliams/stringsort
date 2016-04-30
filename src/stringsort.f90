!*****************************************************************************************
!> author: Jacob Williams
!  license: BSD
!
!  String sorting routines.

    module string_sort_module

    implicit none

    private

    character(len=*),parameter :: lowercase_letters = 'abcdefghijklmnopqrstuvwxyz'
    character(len=*),parameter :: uppercase_letters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

    public :: lexical_sort_recursive
    public :: lexical_sort_nonrecursive
    public :: list_is_sorted

    contains
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns lowercase version of the string.

    pure elemental function lower(str) result(lcase)

    implicit none

    character(len=*),intent(in) :: str
    character(len=(len(str)))   :: lcase

    integer :: i,n

    n = len_trim(str)

    if (n>0) then
        do concurrent (i=1:n)
            lcase(i:i) = lowercase_char(str(i:i))
        end do
    else
        lcase = ''
    end if

    end function lower
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns lowercase version of the character.

    pure elemental function lowercase_char(c) result(lcase)

    implicit none

    character(len=1),intent(in) :: c
    character(len=1)            :: lcase

    integer :: j

    j = index( uppercase_letters,c )
    lcase = merge(lowercase_letters(j:j),c,j>0)

    end function lowercase_char
!*****************************************************************************************

!********************************************************************************
!>
!  Returns true if the s1 < s2 in a lexical sense (can be case sensitive).

    pure logical function lexical_lt(s1,s2,case_sensitive)

    implicit none

    character(len=*),intent(in) :: s1
    character(len=*),intent(in) :: s2
    logical,intent(in)          :: case_sensitive

    if (case_sensitive) then
        lexical_lt = s1 < s2
    else
        lexical_lt = lower(s1) < lower(s2)
    end if

    end function lexical_lt
!********************************************************************************

!********************************************************************************
!>
!  Returns true if the s1 > s2 in a lexical sense (can be case sensitive).

    pure logical function lexical_gt(s1,s2,case_sensitive)

    implicit none

    character(len=*),intent(in) :: s1
    character(len=*),intent(in) :: s2
    logical,intent(in)          :: case_sensitive

    if (case_sensitive) then
        lexical_gt = s1 > s2
    else
        lexical_gt = lower(s1) > lower(s2)
    end if

    end function lexical_gt
!********************************************************************************

!********************************************************************************
!>
!  Returns true if the s1 == s2 in a lexical sense (can be case sensitive).

    pure logical function lexical_eq(s1,s2,case_sensitive)

    implicit none

    character(len=*),intent(in) :: s1
    character(len=*),intent(in) :: s2
    logical,intent(in)          :: case_sensitive

    if (case_sensitive) then
        lexical_eq = s1 == s2
    else
        lexical_eq = lower(s1) == lower(s2)
    end if

    end function lexical_eq
!********************************************************************************

!********************************************************************************
!>
!  Returns true if the s1 <= s2 in a lexical sense (can be case sensitive).

    pure logical function lexical_le(s1,s2,case_sensitive)

    implicit none

    character(len=*),intent(in) :: s1
    character(len=*),intent(in) :: s2
    logical,intent(in)          :: case_sensitive

    lexical_le = lexical_lt(s1,s2,case_sensitive) .or. &
                 lexical_eq(s1,s2,case_sensitive)

    end function lexical_le
!********************************************************************************

!********************************************************************************
!>
!  Returns true if the s1 >= s2 in a lexical sense (can be case sensitive).

    pure logical function lexical_ge(s1,s2,case_sensitive)

    implicit none

    character(len=*),intent(in) :: s1
    character(len=*),intent(in) :: s2
    logical,intent(in)          :: case_sensitive

    lexical_ge = lexical_gt(s1,s2,case_sensitive) .or. &
                 lexical_eq(s1,s2,case_sensitive)

    end function lexical_ge
!********************************************************************************

!********************************************************************************
!>
!  Swap two character strings.

    pure elemental subroutine swap(s1,s2)

    implicit none

    character(len=*),intent(inout) :: s1
    character(len=*),intent(inout) :: s2

    character(len=len(s1)) :: tmp

    tmp = s1
    s1  = s2
    s2  = tmp

    end subroutine swap
!********************************************************************************

!*****************************************************************************************
!>
!  Sorts a character array `str` in increasing order.
!
!  Uses a non-recursive quicksort, reverting to insertion sort on arrays of
!  size \(\le 20\). Dimension of `stack` limits array size to about \(2^{32}\).
!
!### License
!  * [Original LAPACK license](http://www.netlib.org/lapack/LICENSE.txt)
!
!### History
!  * Based on the LAPACK routine [DLASRT](http://www.netlib.org/lapack/explore-html/df/ddf/dlasrt_8f.html).
!  * Extensively modified by Jacob Williams,Feb. 2016. Converted to
!    modern Fortran and removed the descending sort option.

    pure subroutine lexical_sort_nonrecursive(str,case_sensitive)

    implicit none

    character(len=*),dimension(:),intent(inout) :: str  !! on entry,the array to be sorted.
                                                        !! on exit,`str` has been sorted into
                                                        !! increasing order (`str(1) <= ... <= str(n)`)
    logical,intent(in) :: case_sensitive !! if true, the sort is case sensitive

    integer,parameter :: select = 20  !! max size for using insertion sort.

    integer :: endd,i,j,n,start,stkpnt
    character(len=len(str)) :: d1,d2,d3,dmnmx,tmp
    integer,dimension(2,32) :: stack

    ! number of elements to sort:
    n = size(str)

    if ( n>1 ) then

        stkpnt     = 1
        stack(1,1) = 1
        stack(2,1) = n

        do

            start  = stack(1,stkpnt)
            endd   = stack(2,stkpnt)
            stkpnt = stkpnt - 1
            if ( endd-start<=select .and. endd>start ) then

                ! do insertion sort on str( start:endd )
                insertion: do i = start + 1,endd
                    do j = i,start + 1,-1
                        if ( lexical_ge(str(j),str(j-1),case_sensitive) ) cycle insertion
                        dmnmx   = str(j)
                        str(j)   = str(j-1)
                        str(j-1) = dmnmx
                    end do
                end do insertion

            elseif ( endd-start>select ) then

                ! partition str( start:endd ) and stack parts,largest one first
                ! choose partition entry as median of 3

                d1 = str(start)
                d2 = str(endd)
                i  =(start+endd)/2
                d3 = str(i)
                if ( lexical_lt(d1,d2,case_sensitive) ) then
                    if ( lexical_lt(d3,d1,case_sensitive) ) then
                        dmnmx = d1
                    elseif ( lexical_lt(d3,d2,case_sensitive) ) then
                        dmnmx = d3
                    else
                        dmnmx = d2
                    endif
                elseif ( lexical_lt(d3,d2,case_sensitive) ) then
                    dmnmx = d2
                elseif ( lexical_lt(d3,d1,case_sensitive) ) then
                    dmnmx = d3
                else
                    dmnmx = d1
                endif

                i = start - 1
                j = endd + 1
                do
                    do
                        j = j - 1
                        if ( lexical_le(str(j),dmnmx,case_sensitive) ) exit
                    end do
                    do
                        i = i + 1
                        if ( lexical_ge(str(i),dmnmx,case_sensitive) ) exit
                    end do
                    if ( i<j ) then
                        tmp   = str(i)
                        str(i) = str(j)
                        str(j) = tmp
                    else
                        exit
                    endif
                end do
                if ( j-start>endd-j-1 ) then
                    stkpnt          = stkpnt + 1
                    stack(1,stkpnt) = start
                    stack(2,stkpnt) = j
                    stkpnt          = stkpnt + 1
                    stack(1,stkpnt) = j + 1
                    stack(2,stkpnt) = endd
                else
                    stkpnt          = stkpnt + 1
                    stack(1,stkpnt) = j + 1
                    stack(2,stkpnt) = endd
                    stkpnt          = stkpnt + 1
                    stack(1,stkpnt) = start
                    stack(2,stkpnt) = j
                endif

            endif

            if ( stkpnt<=0 ) exit

        end do

    end if

    end subroutine lexical_sort_nonrecursive
!*****************************************************************************************

!********************************************************************************
!>
!  Sorts a character array `str` in increasing order.
!  Uses a basic recursive quicksort
!  (with insertion sort for partitions with <= 20 elements).

    subroutine lexical_sort_recursive(str,case_sensitive)

    implicit none

    character(len=*),dimension(:),intent(inout) :: str
    logical,intent(in) :: case_sensitive !! if true, the sort is case sensitive

    call quicksort(1,size(str))

    contains

    !***************************************************************
    !>
    !  Sort the array, based on the lexical string comparison.

        recursive subroutine quicksort(ilow,ihigh)

        implicit none

        integer,intent(in) :: ilow
        integer,intent(in) :: ihigh

        integer :: ipivot !! pivot element
        integer :: i      !! counter
        integer :: j      !! counter

        integer,parameter :: max_size_for_insertion_sort = 20
            !! max size for using insertion sort.

        if ( ihigh-ilow<=max_size_for_insertion_sort .and. ihigh>ilow ) then

            ! do insertion sort:
            do i = ilow + 1,ihigh
                do j = i,ilow + 1,-1
                    if ( lexical_lt(str(j),str(j-1),case_sensitive) ) then
                        call swap(str(j),str(j-1))
                    else
                        exit
                    end if
                end do
            end do

        elseif ( ihigh-ilow>max_size_for_insertion_sort ) then

            ! do the normal quicksort:
            call partition(ilow,ihigh,ipivot)
            call quicksort(ilow,ipivot - 1)
            call quicksort(ipivot + 1,ihigh)

        end if

        end subroutine quicksort

    !***************************************************************
    !>
    !  Partition the array, based on the lexical string comparison.

        subroutine partition(ilow,ihigh,ipivot)

        implicit none

        integer,intent(in)  :: ilow
        integer,intent(in)  :: ihigh
        integer,intent(out) :: ipivot

        integer :: i,ip

        call swap(str(ilow),str((ilow+ihigh)/2))
        ip = ilow
        do i = ilow + 1, ihigh
            if (lexical_lt(str(i),str(ilow),case_sensitive)) then
                ip = ip + 1
                call swap(str(ip),str(i))
            end if
        end do
        call swap(str(ilow),str(ip))
        ipivot = ip

        end subroutine partition

    end subroutine lexical_sort_recursive
!********************************************************************************

!********************************************************************************
!>
!  Returns true if the list is lexically sorted in increasing order.

    logical function list_is_sorted(str,case_sensitive) result(sorted)

    implicit none

    character(len=*),dimension(:),intent(inout) :: str
    logical,intent(in) :: case_sensitive !! if true, the sort is case sensitive

    integer :: i

    sorted = .true.
    do i = 1, size(str)-1
        if (lexical_lt(str(i+1),str(i),case_sensitive)) then
            sorted = .false.
            return
        end if
    end do

    end function list_is_sorted
!********************************************************************************

!********************************************************************************
    end module string_sort_module
!********************************************************************************
