!
! The Laboratory of Algorithms
!
! The MIT License
!
! Copyright 2011-2015 Andrey Pudov.
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the 'Software'), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in
! all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
! THE SOFTWARE.
!

module MUBubbleSort

    use MArrays
    use MBubbleSort

    use MUAsserts
    use MUParameters
    use MUReport

    implicit none
    private

    type, public :: TUBubbleSort
    contains
        procedure, nopass :: present
    end type
contains
    subroutine present()
        type(TArrays) :: arrays

        integer, parameter :: NUMBER_OF_ELEMENTS = QUADRATIC_COMPLEXITY
        integer, dimension(NUMBER_OF_ELEMENTS, 4) :: ARRAY

        character(len=24), dimension(4) :: SEQUENCES = &
                (/ 'Sorted', 'Dirty', 'Random', 'Inversed' /)

        call arrays%fillWithSequence(ARRAY(1:NUMBER_OF_ELEMENTS, 1))
        call arrays%fillWithDirtySequence(ARRAY(1:NUMBER_OF_ELEMENTS, 2))
        call arrays%fillWithRandom(ARRAY(1:NUMBER_OF_ELEMENTS, 3))
        call arrays%fillWithInversedSequence(ARRAY(1:NUMBER_OF_ELEMENTS, 4))

        call sortOriginal(ARRAY, SEQUENCES)
        call sortInversed(ARRAY, SEQUENCES)
        call sortFlagged(ARRAY, SEQUENCES)
    end subroutine

    subroutine sortOriginal(arrays, sequences)
        integer, dimension(:,:), intent(in)        :: arrays
        character(len=*), dimension(:), intent(in) :: sequences

        type(TBubbleSort)                   :: bubbleSort
        integer, dimension(size(arrays, 1)) :: copy
        integer index
        real    start

        do index = 1, size(arrays, 2)
            copy = arrays(1:size(arrays, 1), index)

            call cpu_time(start)
            call bubbleSort%sortOriginal(copy)

            call report('BubbleSort', 'Original', sequences(index), start)
            call assert_sorted(copy)
        end do

        print *, ''
    end subroutine

    subroutine sortInversed(arrays, sequences)
        integer, dimension(:,:), intent(in)        :: arrays
        character(len=*), dimension(:), intent(in) :: sequences

        type(TBubbleSort)                   :: bubbleSort
        integer, dimension(size(arrays, 1)) :: copy
        integer index
        real    start

        do index = 1, size(arrays, 2)
            copy = arrays(1:size(arrays, 1), index)

            call cpu_time(start)
            call bubbleSort%sortInversed(copy)

            call report('BubbleSort', 'Inversed', sequences(index), start)
            call assert_sorted(copy)
        end do

        print *, ''
    end subroutine

    subroutine sortFlagged(arrays, sequences)
        integer, dimension(:,:), intent(in)        :: arrays
        character(len=*), dimension(:), intent(in) :: sequences

        type(TBubbleSort)                   :: bubbleSort
        integer, dimension(size(arrays, 1)) :: copy
        integer index
        real    start

        do index = 1, size(arrays, 2)
            copy = arrays(1:size(arrays, 1), index)

            call cpu_time(start)
            call bubbleSort%sortFlagged(copy)

            call report('BubbleSort', '*Flagged', sequences(index), start)
            call assert_sorted(copy)
        end do

        print *, ''
    end subroutine
end module
