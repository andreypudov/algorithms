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

module MList

    use MListIterator

    implicit none
    public

    type, abstract :: TList
    contains
        procedure(ILAdd),      deferred :: add
        procedure(ILContains), deferred :: contains
        procedure(ILGet),      deferred :: get
        procedure(ILIterator), deferred :: iterator
        procedure(ILRemove),   deferred :: remove
        procedure(ILSet),      deferred :: set
        procedure(ILSize),     deferred :: size
    end type

    abstract interface
        subroutine ILAdd(instance, value)
            import TList

            class(TList), intent(in out) :: instance
            integer, intent(in)          :: value
        end subroutine

        function ILContains(instance, value) result(status)
            import TList

            class(TList), intent(in) :: instance
            integer, intent(in)      :: value
            logical                  :: status
        end function

        function ILGet(instance, index) result(value)
            import TList

            class(TList), intent(in) :: instance
            integer, intent(in)      :: index
            integer                  :: value
        end function

        function ILIterator(instance) result(value)
            import TList
            import TListIterator

            class(TList), intent(in)      :: instance
            class(TListIterator), pointer :: value
        end function

        subroutine ILRemove(instance, index)
            import TList

            class(TList), intent(in out) :: instance
            integer, intent(in)          :: index
        end subroutine

        subroutine ILSet(instance, index, value)
            import TList

            class(TList), intent(in out) :: instance
            integer, intent(in)          :: index
            integer, intent(in)          :: value
        end subroutine

        function ILSize(instance) result(value)
            import TList

            class(TList), intent(in) :: instance
            integer                  :: value
        end function
    end interface
end module
