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

module MUSort

    use MArrays
    use MBubbleSort
    use MInsertionSort
    use MUAsserts
    use MUReport

    implicit none
    private

    integer, parameter :: NUMBER_OF_ELEMENTS = 100000

    type, public :: TUSort
    contains
        procedure :: present
    end type

contains
    subroutine present(instance)
        class(TUSort), intent(in) :: instance

        type(TArrays) :: arrays

        integer, dimension(NUMBER_OF_ELEMENTS, 4) :: array
        character(len=24), dimension(4) :: sequences = &
                (/ 'Sorted', 'Dirty', 'Random', 'Inversed' /)

        call arrays%fillWithSequence(array(1:NUMBER_OF_ELEMENTS, 1))
        call arrays%fillWithDirtySequence(array(1:NUMBER_OF_ELEMENTS, 2))
        call arrays%fillWithRandom(array(1:NUMBER_OF_ELEMENTS, 3))
        call arrays%fillWithInversedSequence(array(1:NUMBER_OF_ELEMENTS, 4))

        call bubbleSort(array, sequences)
        call insertionSort(array, sequences)
    end subroutine

    subroutine bubbleSort(arrays, sequences)
        integer, target, dimension(:,:), intent(in) :: arrays
        character(len=*), dimension(:), intent(in)  :: sequences

        type(TBubbleSort) :: sort
        integer, pointer  :: array(:)
        integer index
        real    start

        do index = 1, size(arrays, 2)
            array => arrays(1:size(arrays, 1), index)

            call cpu_time(start)
            call sort%sort(array)

            call report('BubbleSort', '', sequences(index), start)
            call assert_sorted(array)
        end do

        print *, ''
    end subroutine

    subroutine insertionSort(arrays, sequences)
        integer, target, dimension(:,:), intent(in) :: arrays
        character(len=*), dimension(:), intent(in)  :: sequences

        type(TInsertionSort) :: sort
        integer, pointer  :: array(:)
        integer index
        real    start

        do index = 1, size(arrays, 2)
            array => arrays(1:size(arrays, 1), index)

            call cpu_time(start)
            call sort%sort(array)

            call report('InsertionSort', '', sequences(index), start)
            call assert_sorted(array)
        end do

        print *, ''
    end subroutine
end module
