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

module MHeap

    implicit none
    public

    type, abstract :: THeap
    contains
        procedure(IAdd),         deferred :: add
        procedure(IContains),    deferred :: contains
        procedure(IGet),         deferred :: get
        procedure(IEmpty),       deferred :: empty

        !procedure(IExtractMin),  deferred :: extractMin
        procedure(IHeapify),     deferred :: heapify
        procedure(IDecreaseKey), deferred :: decreaseKey
    end type

    type, abstract :: THeapNode
        integer :: x
    end type

    abstract interface
        subroutine IAdd(instance, value)
            import THeap

            class(THeap), intent(in out) :: instance
            integer, intent(in)          :: value
        end subroutine

        function IContains(instance, value) result(status)
            import THeap

            class(THeap), intent(in) :: instance
            integer, intent(in)      :: value
            logical                  :: status
        end function

        function IGet(instance, index) result(value)
            import THeap

            class(THeap), intent(in) :: instance
            integer, intent(in)      :: index
            integer                  :: value
        end function

        function IEmpty(instance) result(value)
            import THeap

            class(THeap), intent(in) :: instance
            logical                  :: value
        end function

        !function IExtractMin(instance) result(value)
        !    import THeap
        !    import THeadNode

        !    class(THeap), intent(in)               :: instance
        !    class(THeadNode), pointer, intent(out) :: value
        !end function

        subroutine IHeapify(instance, index)
            import THeap

            class(THeap), intent(in out) :: instance
            integer, intent(in)          :: index
        end subroutine

        subroutine IDecreaseKey(instance, vertex, distance)
            import THeap

            class(THeap), intent(in out) :: instance
            integer, intent(in)          :: vertex
            integer, intent(in)          :: distance
        end subroutine
    end interface
end module
