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

module MUArrayQueue

    use MArrays
    use MArrayQueue

    implicit none
    private

    type, public :: TUArrayQueue
    contains
        procedure :: present
    end type
contains
    subroutine present(instance)
        class(TUArrayQueue), intent(in) :: instance

        integer, parameter :: NUMBER_OF_ELEMENTS = 2000000
        integer, dimension(NUMBER_OF_ELEMENTS) :: ARRAY

        integer element
        integer index
        real start

        type(TArrayQueue) :: queue
        type(TArrays)     :: arrays

        !call arrays%fillWithRandom(ARRAY)
        call arrays%fillWithSequence(ARRAY)

        ! add elemenets to stack
        call cpu_time(start)
        call queue%init()

        do index = 1, size(ARRAY)
            call queue%push(ARRAY(index))
        end do
        call report('Push', start)

        call cpu_time(start)
        do index = 1, size(ARRAY)
            element = queue%peek()
            if (element /= ARRAY(1)) then
                print '(t1, a)', 'FAILED. The return value of peek operation is incorrect.'
                print '(t9, a, i10, a, i10)', 'Expected: ', ARRAY(size(ARRAY) - index), ', Value: ', element
                return
            end if
        end do
        call report('Peek', start)

        call cpu_time(start)
        do index = 1, size(ARRAY)
            element = queue%pop()
            if (element /= ARRAY(index)) then
                print '(t1, a)', 'FAILED. The return value of pop operation is incorrect.'
                print '(t9, a, i10, a, i10)', 'Expected: ', ARRAY(index), ', Value: ', element
                print *, ARRAY
                return
            end if
        end do
        call report('Pop', start)

        call queue%destroy()

        print *, ''
    end subroutine

    subroutine report(operation, start)
        character(len=*), intent(in) :: operation
        real, intent(in)             :: start

        character(len=*), parameter :: format = "(t1, a,  a10, a28, f0.3, a)"
        real finish

        call cpu_time(finish)
        print format, 'ArrayQueue:', operation, ' ', finish - start, 's.'
    end subroutine
end module
