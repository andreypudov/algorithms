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

module MUInsertionSort

    use MArrays
    use MInsertionSort

    implicit none
    private

    type, public :: TUInsertionSort
    contains
        procedure :: present
    end type
contains
    subroutine present(instance)
        class(TUInsertionSort), intent(in) :: instance
        type(TArrays) :: arrays
        
        integer, parameter :: NUMBER_OF_ELEMENTS = 25000
        integer, dimension(NUMBER_OF_ELEMENTS, 4) :: ARRAY

        character(len=24), dimension(4) :: SEQUENCES = &
                (/ 'Sorted', 'Dirty', 'Random', 'Inversed' /)
        
        call arrays%fillWithSequence(ARRAY(1:NUMBER_OF_ELEMENTS, 1))
        call arrays%fillWithDirtySequence(ARRAY(1:NUMBER_OF_ELEMENTS, 2))
        call arrays%fillWithRandom(ARRAY(1:NUMBER_OF_ELEMENTS, 3))
        call arrays%fillWithInversedSequence(ARRAY(1:NUMBER_OF_ELEMENTS, 4))

        call sortOriginal(ARRAY, SEQUENCES)
        call sortBinary(ARRAY, SEQUENCES)
    end subroutine

    subroutine sortOriginal(arrays, sequences)
        integer, dimension(:,:), intent(in)        :: arrays
        character(len=*), dimension(:), intent(in) :: sequences

        type(TInsertionSort)                :: insertionSort
        integer, dimension(size(arrays, 1)) :: copy
        integer index        
        real    start
        
        do index = 1, size(arrays, 2)
            copy = arrays(1:size(arrays, 1), index)
            
            call cpu_time(start)
            call insertionSort%sortOriginal(copy)
            call report(copy, 'Original', sequences(index), start)
        end do

        print *, ''
    end subroutine

    subroutine sortBinary(arrays, sequences)
        integer, dimension(:,:), intent(in)        :: arrays
        character(len=*), dimension(:), intent(in) :: sequences

        type(TInsertionSort)                :: insertionSort
        integer, dimension(size(arrays, 1)) :: copy
        integer index        
        real    start
        
        do index = 1, size(arrays, 2)
            copy = arrays(1:size(arrays, 1), index)
            
            call cpu_time(start)
            call insertionSort%sortBinary(copy)
            call report(copy, '*Binary', sequences(index), start)
        end do

        print *, ''
    end subroutine

    subroutine report(array, algorithm, sequence, start)
        integer, dimension(:), intent(in) :: array
        character(len=*), intent(in)      :: algorithm
        character(len=*), intent(in)      :: sequence
        real, intent(in)                  :: start

        type(TArrays)               :: arrays
        character(len=*), parameter :: format = "(t1, a,  a12, a8, a10, a10, f0.3, a)"
        real finish

        call cpu_time(finish)
        print FORMAT, 'InsertionSort: ', algorithm, ' ', sequence, ' ', &
                finish - start, "s."

        if (arrays%isSorted(array) .ne. .true.) then
            print '(t1, a)', 'FAILED. The sequence does not sorted properly.'
            print *, array
        end if
    end subroutine
end module
