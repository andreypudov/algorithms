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

module MMinHeap

    use MHeap

    implicit none
    private

    type, extends(THeapNode), public :: TMinHeapNode
        integer vertex
        integer distance
    end type

    type, extends(THeap), public :: TMinHeap
    private
        type(TMinHeapNode), allocatable :: list

        integer :: capacity
        integer :: size
    contains
        procedure :: add
        procedure :: contains
        procedure :: get
        procedure :: empty

        !procedure(IExtractMin),  deferred :: extractMin
        procedure :: heapify
        procedure :: decreaseKey
    end type
contains
    subroutine add(instance, value)
        class(TMinHeap), intent(in out) :: instance
        integer, intent(in)             :: value
    end subroutine

    function contains(instance, value) result(status)
        class(TMinHeap), intent(in) :: instance
        integer, intent(in)         :: value
        logical                     :: status
    end function

    function get(instance, index) result(value)
        class(TMinHeap), intent(in) :: instance
        integer, intent(in)         :: index
        integer                     :: value
    end function

    function empty(instance) result(value)
        class(TMinHeap), intent(in) :: instance
        logical                     :: value
    end function

    !function IExtractMin(instance) result(value)
    !    import THeap
    !    import THeadNode

    !    class(THeap), intent(in)               :: instance
    !    class(THeadNode), pointer, intent(out) :: value
    !end function

    subroutine heapify(instance, index)
        class(TMinHeap), intent(in out) :: instance
        integer, intent(in)          :: index

        type(TMinHeapNode) node

        integer smallest
        integer left
        integer right

        smallest = index
        left     = 2 * index + 1
        right    = 2 * index + 2

        !node = instance%list(left)

        !if ((left < instance%size) .and. &
        !        (instance%list(left)%distance < instance%list(smallest)%distance)) then
        !    smallest = left
        !end if
    end subroutine

    subroutine decreaseKey(instance, vertex, distance)
        class(TMinHeap), intent(in out) :: instance
        integer, intent(in)          :: vertex
        integer, intent(in)          :: distance
    end subroutine
end module
