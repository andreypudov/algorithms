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

module MQuickSort

    use MArrayStack
    use MSort

    implicit none
    private

    type, extends(TSort), public :: TQuickSort
    contains
        procedure, nopass :: sort => sortOriginal

        procedure, nopass :: sortOriginal
        procedure, nopass :: sortStackBased
    end type

contains
    subroutine sortOriginal(array)
        integer, dimension(:), intent(in out) :: array

        call sortOriginalProc(array, 1, size(array))
    end subroutine

    recursive subroutine sortOriginalProc(array, left, right)
        integer, dimension(:), intent(in out) :: array
        integer, intent(in) :: left
        integer, intent(in) :: right

        integer pivot

        if (right > left) then
            pivot = partition(array, left, right)

            call sortOriginalProc(array, left, pivot - 1)
            call sortOriginalProc(array, pivot + 1, right)
        end if
    end subroutine

    subroutine sortStackBased(array)
        integer, dimension(:), intent(in out) :: array

        type(TArrayStack) :: stack
        integer index
        integer left
        integer right

        call stack%init()

        left  = 1
        right = size(array)

        do
            do while (right > left)
                index = partition(array, left, right)

                if (index - left > right - index) then
                    call stack%push(left)
                    call stack%push(index - 1)

                    left = index + 1
                else
                    call stack%push(index + 1)
                    call stack%push(right)

                    right = index - 1
                end if
            end do

            ! stack is empty
            if (stack%peek() == 0) then
                exit
            end if

            right = stack%pop()
            left  = stack%pop()
        end do

        call stack%destroy()
    end subroutine

    function partition(array, left, right) result(pivot)
        integer, dimension(:), intent(in out) :: array
        integer, intent(in) :: left
        integer, intent(in) :: right

        integer pivot
        integer jndex
        integer value
        integer buffer

        value = array(right)
        pivot = left - 1
        jndex = right

        do
            pivot = pivot + 1
            jndex = jndex - 1

            do while (array(pivot) < value)
                pivot = pivot + 1
            end do

            do while (array(jndex) > value)
                jndex = jndex - 1
            end do

            if (pivot >= jndex) then
                exit
            end if

            buffer       = array(pivot)
            array(pivot) = array(jndex)
            array(jndex) = buffer
        end do

        buffer       = array(pivot)
        array(pivot) = array(right)
        array(right) = buffer
    end function
end module
