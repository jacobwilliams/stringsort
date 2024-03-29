!*****************************************************************************************
!> author: Jacob Williams
!  license: BSD
!
!  String sorting routines.

    module string_sort_module

    use iso_fortran_env, only: ip => INT32 ! integer precision

    implicit none

    private

    character(len=*),parameter :: lowercase_letters = 'abcdefghijklmnopqrstuvwxyz'
    character(len=*),parameter :: uppercase_letters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    integer,parameter :: max_size_for_insertion_sort = 20 !! max size for using insertion sort.

    type :: int_list

        !! For converting a string into a vector of integers,
        !! in order to perform "natural" sorting.
        !!
        !! Contiguous integer values are stored as an integer.
        !! Characters are stored as their ASCII value.
        !!
        !!### Example
        !!  * 'A123b' (case insensitive) => [97,123,98]
        !!  * 'A123b' (case sensitive)   => [65,123,98]

        private

        integer :: length = 0  !! number of chunks
        integer(ip),dimension(:),allocatable :: chunk !! the integer values
        logical,dimension(:),allocatable :: chunk_is_int
            !! if the corresponding entry in `chunk` represents an integer
            !! from the string. Otherwise, it is the ASCII value for a single
            !! character.
    contains
        private
        generic,public :: operator(<) => ints_lt
        procedure :: ints_lt
    end type int_list

    interface swap
        module procedure :: swap_chars
        module procedure :: swap_ints
    end interface

    public :: lexical_sort_recursive
    public :: lexical_sort_nonrecursive
    public :: lexical_sort_natural_recursive
    public :: list_is_sorted

    contains
!*****************************************************************************************

!*****************************************************************************************
!>
!  Swap two integer values.

    pure elemental subroutine swap_ints(s1,s2)

    implicit none

    integer,intent(inout) :: s1
    integer,intent(inout) :: s2

    integer :: tmp

    tmp = s1
    s1  = s2
    s2  = tmp

    end subroutine swap_ints
!*****************************************************************************************

!*****************************************************************************************
!>
!  Converts a character string into an array of integers suitable for the
!  "natural sorting" algorithm.
!
!@warning If the integer is too large to fit in an integer(ip),
!         then there will be problems.

    pure elemental subroutine string_to_int_list(str,case_sensitive,list)

    implicit none

    character(len=*),intent(in) :: str
    logical,intent(in)          :: case_sensitive
    type(int_list),intent(out)  :: list

    integer                      :: i                 !! counter
    integer                      :: n                 !! length of input str
    character(len=1)             :: c                 !! temp character
    character(len=:),allocatable :: tmp               !! for accumulating blocks of contiguous ints
    logical                      :: is_int            !! if the current character is an integer
    logical                      :: accumulating_ints !! if a block of contiguous ints is
                                                      !! being accumulated

    list%length = 0 ! actual length will be accumulated as we go
    n = len_trim(str)

    if (n>0) then

        allocate(list%chunk(n)) ! worst case: all single characters
        allocate(list%chunk_is_int(n))
        list%chunk_is_int = .false.
        accumulating_ints = .false.
        tmp = ''

        do i=1,n  ! loop through each character in the string

            c = str(i:i)
            is_int = character_is_integer(c)

            if ( is_int ) then ! is a number

                ! accumulate this character in the current int block
                accumulating_ints = .true.
                tmp = tmp//c

            else ! not a number

                if (accumulating_ints) then
                    !finish off previous int block
                    list%length = list%length + 1
                    list%chunk(list%length) = string_to_integer(tmp)
                    list%chunk_is_int(list%length) = .true.
                    accumulating_ints = .false.
                    tmp = ''
                end if

                !accumulate ascii value for current character:
                list%length = list%length + 1
                if (case_sensitive) then
                    list%chunk(list%length) = ichar(c)
                else
                    list%chunk(list%length) = ichar(lowercase_char(c))
                end if

            end if

        end do

        if (accumulating_ints) then  ! last int block
            list%length = list%length + 1
            list%chunk(list%length) = string_to_integer(tmp)
            list%chunk_is_int(list%length) = .true.
        end if

    else
        !empty string, just add one element so we can sort it:
        allocate(list%chunk(1))
        list%chunk = 0
        list%length = 1
    end if

    !resize the array:
    list%chunk = list%chunk(1:list%length)  ! Fortran 2008 LHS auto-reallocation

    end subroutine string_to_int_list
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns true if the i1 < i2 for two [[int_list]] variables.
!  Each integer in each list is compared starting from the beginning.
!  Returns true if the first non-matching i1%chunk(:) < i2%chunk(:).
!
!@note Whether or not it is a case sensitive comparison was determined
!      when the strings were converted to [[int_list]] arrays.

    pure logical function ints_lt(i1,i2)

    implicit none

    class(int_list),intent(in) :: i1
    class(int_list),intent(in) :: i2

    integer :: i !! counter

    integer,parameter :: ascii_zero = ichar('0')

    ints_lt = .false.

    do i = 1, min(i1%length, i2%length)

        if ((i1%chunk_is_int(i) .and. i2%chunk_is_int(i)) .or. &
            (.not. i1%chunk_is_int(i) .and. .not. i2%chunk_is_int(i)) ) then
            !both integers or both characters
            if (i1%chunk(i)/=i2%chunk(i)) then
                ints_lt = i1%chunk(i) < i2%chunk(i)
                return
            end if
        else
            !for [integer,character] comparisons, the actual
            !integer value doesn't matter, so we compare to '0'
            if (i1%chunk_is_int(i)) then
               ints_lt = ascii_zero < i2%chunk(i)
            else
               ints_lt = i1%chunk(i) < ascii_zero
            end if
            return
        end if

    end do

    !special case where i2 begins with i1, but is longer
    ints_lt = (i1%length<i2%length)

    end function ints_lt
!*****************************************************************************************

!*****************************************************************************************
!>
!  Convert a string to an integer.
!
!@note Based on similar routine from `JSON-Fortran`.
!
!@warning If the integer is too large to fit in an integer(ip),
!         then there will be problems.

    pure elemental function string_to_integer(str) result(ival)

    implicit none

    character(len=*),intent(in) :: str
    integer(ip)                 :: ival

    integer :: ndigits_digits,ndigits,ierr

    ! Compute how many digits we need to read
    ndigits = 2*len_trim(str)
    ndigits_digits = floor(log10(real(ndigits))) + 1

    block
        character(len=ndigits_digits) :: digits_str ! large enough to hold ndigits string
        write(digits_str,'(I0)') ndigits
        read(str,'(I'//trim(digits_str)//')',iostat=ierr) ival
        if (ierr/=0) ival = huge(1_ip) ! for errors just return a large value
    end block

    end function string_to_integer
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns true if the character represents an integer ('0','1',...,'9').

    pure elemental function character_is_integer(c) result(is_integer)

    implicit none

    character(len=1),intent(in) :: c
    logical                     :: is_integer

    is_integer = c>='0' .and. c<='9'

    end function character_is_integer
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

    if (j>0) then
        lcase = lowercase_letters(j:j)
    else
        lcase = c
    end if

    end function lowercase_char
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns true if the s1 < s2 in a lexical sense (can be case sensitive).

    pure logical function lexical_lt(s1,s2,case_sensitive)

    implicit none

    character(len=*),intent(in) :: s1
    character(len=*),intent(in) :: s2
    logical,intent(in)          :: case_sensitive

    integer          :: i  !! counter
    character(len=1) :: c1 !! character from s1
    character(len=1) :: c2 !! character from s2

    lexical_lt = .false.

    do i = 1, min(len(s1), len(s2))
        if (case_sensitive) then
            c1 = s1(i:i)
            c2 = s2(i:i)
        else
            c1 = lower(s1(i:i))
            c2 = lower(s2(i:i))
        end if
        if (c1/=c2) then
            lexical_lt = c1 < c2
            return
        end if
    end do

    !special case where s2 begins with s1, but is longer
    lexical_lt = (len(s1)<len(s2))

    end function lexical_lt
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns true if the s1 > s2 in a lexical sense (can be case sensitive).

    pure logical function lexical_gt(s1,s2,case_sensitive)

    implicit none

    character(len=*),intent(in) :: s1
    character(len=*),intent(in) :: s2
    logical,intent(in)          :: case_sensitive

    integer          :: i  !! counter
    character(len=1) :: c1 !! character from s1
    character(len=1) :: c2 !! character from s2

    lexical_gt = .false.

    do i = 1, min(len(s1), len(s2))
        if (case_sensitive) then
            c1 = s1(i:i)
            c2 = s2(i:i)
        else
            c1 = lower(s1(i:i))
            c2 = lower(s2(i:i))
        end if
        if (c1/=c2) then
            lexical_gt = c1 > c2
            return
        end if
    end do

    !special case where s2 begins with s1, but is longer
    lexical_gt = (len(s1)>len(s2))

    end function lexical_gt
!*****************************************************************************************

!*****************************************************************************************
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
!*****************************************************************************************

!*****************************************************************************************
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
!*****************************************************************************************

!*****************************************************************************************
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
!*****************************************************************************************

!*****************************************************************************************
!>
!  Swap two character strings.

    pure elemental subroutine swap_chars(s1,s2)

    implicit none

    character(len=*),intent(inout) :: s1
    character(len=*),intent(inout) :: s2

    character(len=len(s1)) :: tmp

    tmp = s1
    s1  = s2
    s2  = tmp

    end subroutine swap_chars
!*****************************************************************************************

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
            if ( endd-start<=max_size_for_insertion_sort .and. endd>start ) then

                ! do insertion sort on str( start:endd )
                insertion: do i = start + 1,endd
                    do j = i,start + 1,-1
                        if ( lexical_lt(str(j),str(j-1),case_sensitive) ) then
                            dmnmx    = str(j)
                            str(j)   = str(j-1)
                            str(j-1) = dmnmx
                        else
                            exit
                        end if
                    end do
                end do insertion

            elseif ( endd-start>max_size_for_insertion_sort ) then

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

!*****************************************************************************************
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
!*****************************************************************************************

!*****************************************************************************************
!>
!  Sorts a character array `str` in increasing order,
!  using a "natural" sorting method.
!
!  Uses a basic recursive quicksort
!  (with insertion sort for partitions with <= 20 elements).

    subroutine lexical_sort_natural_recursive(str,case_sensitive)

    implicit none

    character(len=*),dimension(:),intent(inout) :: str
    logical,intent(in) :: case_sensitive !! if true, the sort is case sensitive

    type(int_list),dimension(size(str)) :: ints !! the `str` converted into arrays of integers
    logical,dimension(size(str)) :: case_sensitive_vec !! for the elemental routine
    integer,dimension(size(str)) :: idx !! index vector for sorting
    integer :: i !! counter

    !convert vector of strings to vector of int vectors:
    case_sensitive_vec = case_sensitive
    call string_to_int_list(str,case_sensitive_vec,ints)

    idx = [(i, i=1,size(str))]
    call quicksort(1,size(str))
    str = str(idx)

    contains

    !***************************************************************
    !>
    !  Sort the index array of `str`, based on int vec comparison.

        recursive subroutine quicksort(ilow,ihigh)

        implicit none

        integer,intent(in) :: ilow
        integer,intent(in) :: ihigh

        integer :: ipivot !! pivot element
        integer :: i      !! counter
        integer :: j      !! counter

        if ( ihigh-ilow<=max_size_for_insertion_sort .and. ihigh>ilow ) then

            ! do insertion sort:
            do i = ilow + 1,ihigh
                do j = i,ilow + 1,-1
                    if ( ints(idx(j)) < ints(idx(j-1)) ) then
                        call swap(idx(j),idx(j-1))
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
    !  Partition the index array of `str`, based on int vec comparison.

        subroutine partition(ilow,ihigh,ipivot)

        implicit none

        integer,intent(in)  :: ilow
        integer,intent(in)  :: ihigh
        integer,intent(out) :: ipivot

        integer :: i,ip

        call swap(idx(ilow),idx((ilow+ihigh)/2))
        ip = ilow
        do i = ilow + 1, ihigh
            if ( ints(idx(i)) < ints(idx(ilow)) ) then
                ip = ip + 1
                call swap(idx(ip),idx(i))
            end if
        end do
        call swap(idx(ilow),idx(ip))
        ipivot = ip

        end subroutine partition

    end subroutine lexical_sort_natural_recursive
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns true if the list is lexically sorted in increasing order.

    logical function list_is_sorted(str,case_sensitive,natural) result(sorted)

    implicit none

    character(len=*),dimension(:),intent(inout) :: str
    logical,intent(in) :: case_sensitive !! if true, the sort is case sensitive
    logical,intent(in) :: natural !! if true, the sort is "natural"

    type(int_list),dimension(size(str)) :: ints !! the `str` converted into arrays of integers
    logical,dimension(size(str)) :: case_sensitive_vec !! for the elemental routine
    integer :: i !! counter

    sorted = .true.

    if (natural) then

        !convert vector of strings to vector of int vectors:
        case_sensitive_vec = case_sensitive
        call string_to_int_list(str,case_sensitive_vec,ints)

        do i = 1, size(str)-1
            if ( ints(i+1) < ints(i) ) then
                sorted = .false.
                return
            end if
        end do

    else
        do i = 1, size(str)-1
            if (lexical_lt(str(i+1),str(i),case_sensitive)) then
                sorted = .false.
                return
            end if
        end do
    end if

    end function list_is_sorted
!*****************************************************************************************

!*****************************************************************************************
    end module string_sort_module
!*****************************************************************************************
