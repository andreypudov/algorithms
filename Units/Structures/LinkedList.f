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
    use MUReport

    implicit none
    private

    type, public :: TULinkedList
    contains
        procedure, nopass :: present
    end type
contains
    subroutine present()
        integer, parameter :: NUMBER_OF_ELEMENTS = 50000!0
        integer, dimension(NUMBER_OF_ELEMENTS) :: ARRAY

        integer element
        integer index
        real    start

        type(TLinkedList) :: list
        type(TArrays)     :: arrays

        call arrays%fillWithRandom(ARRAY)

        ! add elemenets to stack
        call cpu_time(start)
        call list%init()

        do index = 1, size(ARRAY)
            call list%add(ARRAY(index))
        end do
        call report('LinkedList', 'Add', '', start)

        call cpu_time(start)
        do index = 1, size(ARRAY)
            if (list%contains(ARRAY(index)) /= .true.) then
                print '(t1, a)', 'FAILED. The return value of contains operation is incorrect.'
                print '(t9, a, i10, a, l)', 'Element: ', ARRAY(index), &
                        ', Value: ', list%contains(ARRAY(index))
                return
            end if
        end do
        call report('LinkedList', 'Contains', '', start)

        call cpu_time(start)
        do index = 1, size(ARRAY)
            if (list%get(index) /= ARRAY(index)) then
                print '(t1, a)', 'FAILED. The return value of get operation is incorrect.'
                print '(t9, a, i10, a, i10)', 'Expected: ', ARRAY(index), &
                        ', Value: ', list%get(index)
                return
            end if
        end do
        call report('LinkedList', 'Get', '', start)

        call cpu_time(start)
        do index = 1, size(ARRAY)
            call list%set(index, index)
            if (list%get(index) /= index) then
                print '(t1, a)', 'FAILED. The return value of set operation is incorrect.'
                print '(t9, a, i10, a, i10)', 'Expected: ', index, ', Value: ', list%get(index)
                print *, list%size()
                return
            end if
        end do
        call report('LinkedList', 'Set', '', start)

        call cpu_time(start)
        do index = size(ARRAY) - 1, 0, -1
            call list%remove(list%size())
            if (list%size() /= index) then
                print '(t1, a)', 'FAILED. The return value of remove operation is incorrect.'
                print '(t9, a, i10, a, i10)', 'Expected: ', index, ', Value: ', list%size()
                return
            end if
        end do
        call report('LinkedList', 'Remove', '', start)

        call cpu_time(start)
        do index = 1, size(ARRAY)
            call list%add(ARRAY(index))
            if (list%size() /= index) then
                print '(t1, a)', 'FAILED. The return value of size operation is incorrect.'
                print '(t9, a, i10, a, i10)', 'Expected: ', index, ', Value: ', list%size()
                return
            end if
        end do
        call report('LinkedList', 'Size', '', start)

        call list%destroy()

        print *, ''
    end subroutine
end module
