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

    use MSort

    implicit none
    private

    type, extends(TSort), public :: TQuickSort
    contains
        procedure, nopass :: sort => sortOriginalWrapper

        procedure, nopass :: sortOriginalWrapper
    end type

contains
    subroutine sortOriginalWrapper(array)
        integer, dimension(:), intent(in out) :: array

        call sortOriginal(array, 1, size(array))
    end subroutine

    recursive subroutine sortOriginal(array, left, right)
        integer, dimension(:), intent(in out) :: array
        integer, intent(in) :: left
        integer, intent(in) :: right

        integer index
        integer jndex
        integer value
        integer buffer

        if (right > left) then
            value = array(right)
            index = left - 1
            jndex = right

            do
                index = index + 1
                jndex = jndex - 1

                do while (array(index) < value)
                    index = index + 1
                end do

                do while (array(jndex) > value)
                    jndex = jndex - 1
                end do

                if (index >= jndex) then
                    exit
                end if

                buffer       = array(index)
                array(index) = array(jndex)
                array(jndex) = buffer
            end do

            buffer       = array(index)
            array(index) = array(right)
            array(right) = buffer

            call sortOriginal(array, left, index - 1)
            call sortOriginal(array, index + 1, right)
        end if
    end subroutine
end module
