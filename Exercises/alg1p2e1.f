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

module MExAlg1p2e1

    use MArrays
    use MFileReader
    use MUReport

    implicit none
    private

    integer(kind = 8) :: count = 0

    type, public :: TExAlg1p2e1
    contains
        procedure, nopass :: present
    end type
contains
    subroutine present()
        integer, dimension(:), allocatable :: data
        integer, dimension(:), allocatable :: list

        type(TFileReader) fileReader
        type(TArrays)     arrays

        real start

        !call fileReader%readListOfIntegers('Samples/QuickSort.txt', data)
        call fileReader%readListOfIntegers('Samples/QuickSort.txt', data)
        allocate(list(size(data)))

        ! first element based pivot sort
        list  = data
        count = 0
        call cpu_time(start)
        call firstElementSort(list, 1, size(list))
        if (arrays%isSorted(list) == .false.) then
            print '(t1, a)', 'FAILED. The sequence does not sorted properly.'
        end if
        call report('Alg1p2e1', 'FirstElement', '', start)
        print '(A,I)', 'Number of comparisons: ', count

        ! last element based pivot sort
        list  = data
        count = 0
        call cpu_time(start)
        call lastElementSort(list, 1, size(list))
        if (arrays%isSorted(list) == .false.) then
            print '(t1, a)', 'FAILED. The sequence does not sorted properly.'
        end if
        call report('Alg1p2e2', 'LastElement', '', start)
        print '(A,I)', 'Number of comparisons: ', count

        ! last element based pivot sort
        list  = data
        count = 0
        call cpu_time(start)
        call medianElementSort(list, 1, size(list))
        if (arrays%isSorted(list) == .false.) then
            print '(t1, a)', 'FAILED. The sequence does not sorted properly.'
        end if
        call report('Alg1p2e2', 'MedianElement', '', start)
        print '(A,I)', 'Number of comparisons: ', count

        deallocate(list)
        deallocate(data)
    end subroutine

    recursive subroutine firstElementSort(list, first, last)
        integer, dimension(:), allocatable, intent(in out) :: list
        integer, intent(in) :: first
        integer, intent(in) :: last

        integer pivot

        if (first < last) then
            pivot = firstElementPivot(list, first, last)
            call firstElementSort(list, first, pivot - 1)
            call firstElementSort(list, pivot + 1, last)
        end if
    end subroutine

    recursive subroutine lastElementSort(list, first, last)
        integer, dimension(:), allocatable, intent(in out) :: list
        integer, intent(in) :: first
        integer, intent(in) :: last

        type(TArrays) arrays
        integer pivot

        if (first < last) then
            !pivot = lastElementPivot(list, first, last)
            call arrays%swap(list, first, last)
            pivot = firstElementPivot(list, first, last)

            call lastElementSort(list, first, pivot - 1)
            call lastElementSort(list, pivot + 1, last)
        end if
    end subroutine

    recursive subroutine medianElementSort(list, first, last)
        integer, dimension(:), allocatable, intent(in out) :: list
        integer, intent(in) :: first
        integer, intent(in) :: last

        type(TArrays) arrays
        integer pivot
        integer middle

        if (first < last) then
            middle = first + (last - first) / 2
            if (((list(middle) > list(first)) .and. (list(middle) < list(last))) &
                    .or. ((list(middle) < list(first)) .and. (list(middle) > list(last)))) then
                call arrays%swap(list, middle, first)
            else if (((list(last) > list(first)) .and. (list(last) < list(middle))) &
                    .or. ((list(last) < list(first)) .and. (list(last) > list(middle)))) then
                call arrays%swap(list, last, first)
            end if
            pivot = firstElementPivot(list, first, last)

            call medianElementSort(list, first, pivot - 1)
            call medianElementSort(list, pivot + 1, last)
        end if
    end subroutine

    function firstElementPivot(list, first, last)
        integer, dimension(:), allocatable, intent(in out) :: list
        integer, intent(in) :: first
        integer, intent(in) :: last
        integer firstElementPivot

        type(TArrays) arrays

        integer value
        integer point
        integer index

        value = list(first)
        point = first

        do index = first + 1, last
            if (list(index) < value) then
                point = point + 1
                call arrays%swap(list, point, index)
            end if

            count = count + 1
        end do

        call arrays%swap(list, first, point)
        firstElementPivot = point
    end function

    function lastElementPivot(list, first, last)
        integer, dimension(:), allocatable, intent(in out) :: list
        integer, intent(in) :: first
        integer, intent(in) :: last
        integer lastElementPivot

        type(TArrays) arrays

        integer value
        integer point
        integer index

        value = list(last)
        point = last

        do index = last - 1, first, -1
            if (list(index) > value) then
                point = point - 1
                call arrays%swap(list, point, index)
            end if

            count = count + 1
        end do

        call arrays%swap(list, last, point)
        lastElementPivot = point
    end function
end module
