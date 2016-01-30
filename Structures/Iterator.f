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

module MIterator

    implicit none
    public

    type, abstract :: TIterator
    contains
        procedure(IHasNext), deferred :: hasNext
        procedure(INext),    deferred :: next
        procedure(IRemove),  deferred :: remove
    end type

    abstract interface
        function IHasNext(instance) result(value)
            import TIterator

            class(TIterator), intent(in) :: instance
            logical :: value
        end function

        function INext(instance) result(value)
            import TIterator

            class(TIterator), intent(in out) :: instance
            integer :: value
        end function

        subroutine IRemove(instance)
            import TIterator

            class(TIterator), intent(in out) :: instance
        end subroutine
    end interface
end module
