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

module MArrays

    implicit none
    private

    type, public :: TArrays
    contains
        procedure, nopass :: fillWithSequence
        procedure, nopass :: fillWithDirtySequence
        procedure, nopass :: fillWithRandom
        procedure, nopass :: fillWithInversedSequence

        procedure, nopass :: isSorted
    end type

contains
    !
    ! Fills the array with sequence from one to the last index.
    !
    subroutine fillWithSequence(array)
        integer, dimension(:), intent(in out) :: array
        integer index

        array = (/ (index, index = 1, size(array)) /)
    end subroutine

    !
    ! Fills the array with sequence from one to the last index
    ! and ten percent ramdom values.
    !
    subroutine fillWithDirtySequence(array)
        integer, dimension(:), intent(in out) :: array

        integer index
        integer step

        array = (/ (index, index = 1, size(array)) /)
        step  = size(array) / (size(array) * 0.1)

        do index = step, size(array), step
            array(index) =  array(index) * 2
        end do
    end subroutine

    !
    ! Fills the array with random numbers between zero and maximal
    ! value of type of the array.
    !
    subroutine fillWithRandom(array)
        integer, dimension(:), intent(in out) :: array
        real, dimension(size(array)) :: temporary

        call random_seed()
        call random_number(temporary)

        array = floor(temporary * huge(array))
    end subroutine

    !
    ! Fills the array with sequence from last index of array to the one.
    !
    subroutine fillWithInversedSequence(array)
        integer, dimension(:), intent(in out) :: array
        integer index

        array = (/ (index, index = size(array), 1, -1) /)
    end subroutine

    !
    ! Validates given array to be sorted.
    !
    function isSorted(array) result(sorted)
        integer, dimension(:), intent(in) :: array
        logical                           :: sorted

        integer index

        do index = 2, size(array)
            if (array(index - 1) > array(index)) then
                sorted =  .false.
                return
            end if
         end do

         sorted = .true.
    end function
end module
