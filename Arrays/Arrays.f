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
        procedure, nopass :: fillWithSequence1d
        procedure, nopass :: fillWithSequence2d

        procedure, nopass :: fillWithDirtySequence
        procedure, nopass :: fillWithRandom
        procedure, nopass :: fillWithInversedSequence

        procedure, nopass :: increaseWhenRequiredAllocatable
        procedure, nopass :: increaseWhenRequiredAllocatable2d
        procedure, nopass :: increaseWhenRequiredPointer

        procedure, nopass :: resizeAllocatable
        procedure, nopass :: resizeAllocatable2d

        procedure, nopass :: isSorted

        procedure, nopass :: print1d
        procedure, nopass :: print2d

        procedure, nopass :: swap1d

        generic :: fillWithSequence     => fillWithSequence1d, fillWithSequence2d
        generic :: increaseWhenRequired => increaseWhenRequiredAllocatable,   &
                                           increaseWhenRequiredAllocatable2d, &
                                           increaseWhenRequiredPointer

        generic :: resize => resizeAllocatable, resizeAllocatable2d

        generic :: print => print1d, print2d
        generic :: swap  => swap1d
    end type
contains
    !
    ! Fills the array with sequence from one to the last index.
    !
    subroutine fillWithSequence1d(array)
        integer, dimension(:), intent(in out) :: array
        integer index

        array = (/ (index, index = 1, size(array)) /)
    end subroutine

    subroutine fillWithSequence2d(array)
        integer, dimension(:,:), intent(in out) :: array
        integer index

        array = reshape((/ (index, index = 1, size(array)) /), shape(array))
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
    ! Increase the size of given array if limit is larger than size of array.
    !
    subroutine increaseWhenRequiredAllocatable(array, limit)
        integer, dimension(:), allocatable, intent(in out) :: array
        integer, intent(in) :: limit

        integer, dimension(:), allocatable :: temporary_array
        integer length

        ! increase array size when required
        if (limit > size(array)) then
            length = size(array) * 3 / 2
            allocate(temporary_array(length))
            temporary_array = 0
            temporary_array(1:size(array)) = array
            deallocate(array)
            call move_alloc(temporary_array, array)
        end if
    end subroutine

    subroutine increaseWhenRequiredAllocatable2d(array, limit1, limit2)
        integer, dimension(:,:), allocatable, intent(in out) :: array
        integer, intent(in) :: limit1
        integer, intent(in) :: limit2

        integer, dimension(:,:), allocatable :: temporary_array
        integer length1
        integer length2

        ! increase array size when required
        if ((limit1 > size(array, 1)) .or. (limit2 > size(array, 2))) then
            !print *, limit1, limit2
            length1 = size(array, 1) * 3 / 2
            length2 = size(array, 2) * 3 / 2

            allocate(temporary_array(length1, length2))
            temporary_array = 0
            temporary_array(1:size(array, 1), 1:size(array, 2)) = array
            deallocate(array)
            call move_alloc(temporary_array, array)
        end if
    end subroutine

    subroutine increaseWhenRequiredPointer(array, limit)
        integer, dimension(:), pointer, intent(in out) :: array
        integer, intent(in) :: limit

        integer, dimension(:), pointer :: temporary_array
        integer length

        ! increase array size when required
        if (limit > size(array)) then
            length = size(array) * 3 / 2
            allocate(temporary_array(length))
            temporary_array = 0
            temporary_array(1:size(array)) = array
            deallocate(array)
            array => temporary_array
        end if
    end subroutine

    !
    ! Reallocate array to a given length.
    !
    subroutine resizeAllocatable(array, length)
        integer, dimension(:), allocatable, intent(in out) :: array
        integer, intent(in) :: length

        integer, dimension(:), allocatable :: temporary_array

        if (length /= size(array)) then
            allocate(temporary_array(length))
            temporary_array = 0
            temporary_array(1:length) = array(1:length)
            deallocate(array)
            call move_alloc(temporary_array, array)
        end if
    end subroutine

    subroutine resizeAllocatable2d(array, length1, length2)
        integer, dimension(:,:), allocatable, intent(in out) :: array
        integer, intent(in) :: length1
        integer, intent(in) :: length2

        integer, dimension(:,:), allocatable :: temporary_array

        if ((length1 /= size(array, 1)) .or. (length2 /= size(array, 2))) then
            allocate(temporary_array(length1, length2))
            temporary_array = 0
            temporary_array(1:length1, 1:length2) = array(1:length1, 1:length2)
            deallocate(array)
            call move_alloc(temporary_array, array)
        end if
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

     !
     ! Prints given array to console.
     !
     subroutine print1d(array)
         integer, dimension(:), intent(in) :: array

         character(len=80) :: count
         character(len=80) :: length
         integer           :: maximum
         integer           :: value

         maximum = (maxval(array))
         if (maximum == 0) then
             value = 2
         else if (mod(maximum, 10) == 0) then
             value = ceiling(log10(real(maxval(array)))) + 2
         else
             value = ceiling(log10(real(maxval(array)))) + 1
         end if

         write(count,  '(i0.0)') size(array)
         write(length, '(i0.0)') value

         print '(' // trim(count) // 'i' // trim(length) // ')', array
     end subroutine

     subroutine print2d(array)
         integer, dimension(:,:), intent(in) :: array

         character(len=80) :: count
         character(len=80) :: length
         integer           :: maximum
         integer           :: value

         maximum = (maxval(array))
         if (maximum == 0) then
             value = 2
         else if (mod(maximum, 10) == 0) then
             value = ceiling(log10(real(maxval(array)))) + 2
         else
             value = ceiling(log10(real(maxval(array)))) + 1
         end if

         write(count,  '(i0.0)') size(array, 2)
         write(length, '(i0.0)') value

         print '(' // trim(count) // 'i' // trim(length) // ')', transpose(array)
     end subroutine

     subroutine swap1d(array, index, jndex)
         integer, dimension(:), intent(in out) :: array
         integer, intent(in) :: index
         integer, intent(in) :: jndex

         integer buffer

         buffer = array(index)
         array(index) = array(jndex)
         array(jndex) = buffer
     end subroutine
end module
