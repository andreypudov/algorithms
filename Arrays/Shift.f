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

module MShift

    implicit none
    private

    type, public :: TShift
    contains
        procedure :: shift => shiftByLoop
        
        procedure :: shiftByLoop
        procedure :: shiftByAssignment
        procedure :: shiftByIntrinsic
    end type

contains    
    subroutine shiftByLoop(instance, array)
        class(TShift), intent(in)             :: instance
        integer, dimension(:), intent(in out) :: array

        integer index
        
        do index = size(array) - 1, 1, -1
            array(index + 1) = array(index)
        end do
    end subroutine

    subroutine shiftByAssignment(instance, array)
        class(TShift), intent(in)             :: instance
        integer, dimension(:), intent(in out) :: array

        array(2:size(array)) = array(1:size(array) - 1)
    end subroutine

    subroutine shiftByIntrinsic(instance, array)
        class(TShift), intent(in)             :: instance
        integer, dimension(:), intent(in out) :: array

        array    = cshift(array, -1)
        array(1) = array(2) ! replace the last element of an array by first element
    end subroutine
end module
