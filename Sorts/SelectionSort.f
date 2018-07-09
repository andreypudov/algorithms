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

module MSelectionSort

    use MSort

    implicit none
    private

    type, extends(TSort), public :: TSelectionSort
    contains
        procedure, nopass :: sort => sortOriginal

        procedure, nopass :: sortOriginal
    end type

contains
    subroutine sortOriginal(array)
        integer, dimension(:), intent(in out) :: array

        integer index
        integer jndex
        integer buffer
        integer min

        do index = 1, size(array)
            min = index

            do jndex = index + 1, size(array)
                if (array(jndex) < array(min)) then
                    min = jndex
                end if
            end do

            buffer       = array(min)
            array(min)   = array(index)
            array(index) = buffer
        end do
    end subroutine
end module
