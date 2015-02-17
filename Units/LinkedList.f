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

module MULinkedList

    use MArrays
    use MLinkedList

    implicit none
    private

    type, public :: TULinkedList
    contains
        procedure :: present
    end type
contains
    subroutine present(instance)
        class(TULinkedList), intent(in) :: instance

        integer, parameter :: NUMBER_OF_ELEMENTS = 30000!00
        integer, dimension(NUMBER_OF_ELEMENTS) :: ARRAY

        integer element
        integer index
        real    start

        type(TLinkedList) :: list
        type(TArrays)     :: arrays

        !call arrays%fillWithRandom(ARRAY)
        call arrays%fillWithSequence(ARRAY)

        ! add elemenets to stack
        call cpu_time(start)
        call list%init()

        do index = 1, size(ARRAY)
            call list%add(ARRAY(index))
        end do
        call report('Add', start)

        call cpu_time(start)
        do index = 1, size(ARRAY)
            if (list%contains(ARRAY(index)) /= .true.) then
                print '(t1, a)', 'FAILED. The return value of contains operation is incorrect.'
                print '(t9, a, i10, a, l)', 'Element: ', ARRAY(index), &
                        ', Value: ', list%contains(ARRAY(index))
                return
            end if
        end do
        call report('Contains', start)

        call cpu_time(start)
        do index = 1, size(ARRAY)
            if (list%get(index) /= ARRAY(index)) then
                print '(t1, a)', 'FAILED. The return value of get operation is incorrect.'
                print '(t9, a, i10, a, i10)', 'Expected: ', ARRAY(index), &
                        ', Value: ', list%get(index)
                return
            end if
        end do
        call report('Get', start)

        call list%destroy()
        call list%init()

        call cpu_time(start)
        do index = 1, size(ARRAY)
            call list%add(ARRAY(index))
            if (list%size() /= index) then
                print '(t1, a)', 'FAILED. The return value of size operation is incorrect.'
                print '(t9, a, i10, a, i10)', 'Expected: ', index, ', Value: ', list%size()
                return
            end if
        end do
        call report('Size', start)

        call list%destroy()

        print *, ''
    end subroutine

    subroutine report(operation, start)
        character(len=*), intent(in) :: operation
        real, intent(in)             :: start

        character(len=*), parameter :: format = "(t1, a,  a17, a18, f0.3, a)"
        real finish

        call cpu_time(finish)
        print format, 'LinkedList:', operation, ' ', finish - start, 's.'
    end subroutine
end module
