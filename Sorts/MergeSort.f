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
        procedure, nopass :: sort => sortOriginal

        procedure, nopass :: sortOriginal
    end type
contains
    subroutine sortOriginal(array)
        integer, dimension(:), intent(in out) :: array

        call sortOriginalProc(array, 1, size(array))
    end subroutine

    recursive subroutine sortOriginalProc(array, begin, end)
        integer, dimension(:), intent(in out) :: array
        integer, intent(in)                   :: begin
        integer, intent(in)                   :: end

        integer middle

        if (begin < end) then
            middle = begin + ((end - begin) / 2)

            call sortOriginalProc(array, begin, middle)
            call sortOriginalProc(array, middle + 1, end)

            call mergeOriginal(array, begin, middle, end)
        end if
    end subroutine

    subroutine mergeOriginal(array, begin, middle, end)
        integer, dimension(:), intent(in out) :: array
        integer, intent(in)                   :: begin
        integer, intent(in)                   :: middle
        integer, intent(in)                   :: end

        integer, dimension(end - begin + 1) :: buffer
        integer :: key1 = 1
        integer :: key2 = 1

        ! 1 3 5
        ! 2 4 6
        do while (key1 < size(buffer) .and. key2 < size(buffer))
            !if ()
        end do
    end subroutine
end module
