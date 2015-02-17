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
        procedure :: sort => sortOriginal

        procedure :: sortOriginal
    end type
contains
    subroutine sortOriginal(instance, array)
        class(TMergeSort), intent(in)         :: instance
        integer, dimension(:), intent(in out) :: array

        integer index
        integer jndex
        integer temp

        do index = size(array) - 1, 1, -1
            do jndex = 1, index
                if (array(jndex) > array(jndex + 1)) then
                    temp             = array(jndex)
                    array(jndex)     = array(jndex + 1)
                    array(jndex + 1) = temp
                end if
            end do
        end do
    end subroutine
end module
