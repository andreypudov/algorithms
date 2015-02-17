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

module MUShift

    use MArrays
    use MShift

    implicit none
    private

    type, public :: TUShift
    contains
        procedure :: present
    end type
contains
    subroutine present(instance)
        class(TUShift), intent(in) :: instance
        type(TArrays) :: arrays

        integer, parameter :: NUMBER_OF_ELEMENTS = 1000000
        integer, dimension(NUMBER_OF_ELEMENTS) :: ARRAY

        call arrays%fillWithSequence(ARRAY)

        ! 1, 2, 3, 4, 5 => 1, 1, 2, 3, 4

        call shiftByIntrinsic(ARRAY)
        call shiftByAssignment(ARRAY)
        call shiftByLoop(ARRAY)

        print *, ''
    end subroutine present

    subroutine shiftByLoop(array)
        integer, dimension(:), intent(in) :: array

        type(TShift)                    :: shift
        integer, dimension(size(array)) :: copy
        integer index
        real    start

        copy = array
        call cpu_time(start)
        call shift%shiftByLoop(copy)
        call report(copy, 'Loop', start)
    end subroutine

    subroutine shiftByAssignment(array)
        integer, dimension(:), intent(in) :: array

        type(TShift)                    :: shift
        integer, dimension(size(array)) :: copy
        real    start

        copy = array
        call cpu_time(start)
        call shift%shiftByAssignment(copy)
        call report(copy, 'Assignment', start)
    end subroutine

    subroutine shiftByIntrinsic(array)
        integer, dimension(:), intent(in) :: array

        type(TShift)                    :: shift
        integer, dimension(size(array)) :: copy
        real    start

        copy = array
        call cpu_time(start)
        call shift%shiftByIntrinsic(copy)
        call report(copy, 'Intrinsic', start)
    end subroutine

    subroutine report(array, algorithm, start)
        integer, dimension(:), intent(in) :: array
        character(len=*), intent(in)      :: algorithm
        real, intent(in)                  :: start

        character(len=*), parameter :: format = "(t1, a6,  a22, a18, f0.3, a)"
        real finish

        call cpu_time(finish)
        print format, 'Shift: ', algorithm, ' ', finish - start, 's.'
    end subroutine
end module
