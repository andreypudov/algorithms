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

module MMergeSort

    use MSort

    implicit none
    private

    type, extends(TSort), public :: TMergeSort
    contains
        procedure, nopass :: sort => sortOriginalWrapper

        procedure, nopass :: sortOriginalWrapper
    end type
contains
    subroutine sortOriginalWrapper(array)
        integer, dimension(:), intent(in out) :: array

        call sortOriginal(array, 1, size(array))
    end subroutine

    recursive subroutine sortOriginal(array, begin, end)
        integer, dimension(:), intent(in out) :: array
        integer, intent(in)                   :: begin
        integer, intent(in)                   :: end

        integer middle

        if (begin < end) then
            middle = begin + ((end - begin) / 2)

            call sortOriginal(array, begin, middle)
            call sortOriginal(array, middle + 1, end)

            call mergeOriginal(array, begin, middle, end)
        end if
    end subroutine

    subroutine mergeOriginal(array, begin, middle, end)
        integer, dimension(:), intent(in out) :: array
        integer, intent(in)                   :: begin
        integer, intent(in)                   :: middle
        integer, intent(in)                   :: end

        integer index, jndex
        integer left, right
        integer buffer(size(array))

        index = begin
        left  = begin
        right = middle + 1

        do while ((left <= middle) .and. (right <= end))
            if (array(left) <= array(right)) then
                buffer(index) = array(left)
                left = left + 1
            else
                buffer(index) = array(right)
                right = right + 1
            end if

            index = index + 1
        end do

        if (left > middle) then
            do jndex = right, end
                buffer(index) = array(jndex)
                index = index + 1
            end do
        else
            do jndex = left, middle
                buffer(index) = array(jndex)
                index = index + 1
            end do
        end if

        array(begin:end) = buffer(begin:end)
    end subroutine
end module
