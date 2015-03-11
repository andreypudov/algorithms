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

module MUSearch

    use MArrays
    use MBinarySearch
    use MSequenceSearch
    use MUAsserts
    use MUReport

    implicit none
    private

    integer, parameter :: NUMBER_OF_ELEMENTS = 2096164

    type, public :: TUSearch
    contains
        procedure, nopass :: present
    end type

contains
    subroutine present()
        type(TArrays) :: arrays

        integer, dimension(NUMBER_OF_ELEMENTS, 4) :: array
        character(len=24), dimension(4) :: sequences = &
                (/ 'Sorted', 'Dirty', 'Random', 'Inversed' /)

        call arrays%fillWithSequence(array(1:NUMBER_OF_ELEMENTS, 1))
        call arrays%fillWithDirtySequence(array(1:NUMBER_OF_ELEMENTS, 2))
        call arrays%fillWithRandom(array(1:NUMBER_OF_ELEMENTS, 3))
        call arrays%fillWithInversedSequence(array(1:NUMBER_OF_ELEMENTS, 4))

        call binarySearch(array(1:NUMBER_OF_ELEMENTS, 1), sequences)
        call sequenceSearch(array, sequences)
    end subroutine

    subroutine binarySearch(array, sequences)
        integer, dimension(:), intent(in)           :: array
        character(len=*), dimension(:), intent(in)  :: sequences

        type(TBinarySearch) :: search
        integer index
        integer position
        real    start

        call cpu_time(start)
        position = search%search(array, array(size(array) / 2))

        call report('Search', 'BinarySearch', sequences(1), start)
        call assert_equals(array(position), array(size(array) / 2))

        print *, ''
    end subroutine

    subroutine sequenceSearch(arrays, sequences)
        integer, target, dimension(:,:), intent(in) :: arrays
        character(len=*), dimension(:), intent(in)  :: sequences

        type(TSequenceSearch) :: search
        integer, pointer    :: array(:)
        integer index
        integer position
        real    start

        do index = 1, size(arrays, 2)
            array => arrays(1:size(arrays, 1), index)

            call cpu_time(start)
            position = search%search(array, array(size(array) / 2))

            call report('Search', 'SequenceSearch', sequences(index), start)
            call assert_equals(array(position), array(size(array) / 2))
        end do

        print *, ''
    end subroutine
end module
