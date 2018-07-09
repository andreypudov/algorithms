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

module MLinkedListIterator

    use MListIterator
    use MLinkedListEntry

    implicit none
    private

    type, extends(TListIterator), public :: TLinkedListIterator
    private
        type(TLinkedListEntry), pointer :: entry  => null()
    contains
        procedure :: hasNext
        procedure :: next
        procedure :: remove
        ! hasPrevious
        ! previous
        ! nextIndex
        ! previousIndex
        ! set
        ! add

        procedure :: init
    end type

contains
    function hasNext(instance) result(value)
        class(TLinkedListIterator), intent(in) :: instance
        logical :: value

        value = (associated(instance%entry%next) == .true.)
    end function

    function next(instance) result(value)
        class(TLinkedListIterator), intent(in out) :: instance
        integer :: value

        type(TLinkedListEntry), pointer :: entry
        entry => instance%entry

        value =  instance%entry%value
        instance%entry => instance%entry%next
    end function

    subroutine remove(instance)
        class(TLinkedListIterator), intent(in out) :: instance
    end subroutine

    subroutine init(instance, entry)
        class(TLinkedListIterator), intent(in out)  :: instance
        type(TLinkedListEntry), pointer, intent(in) :: entry

        instance%entry => entry;
    end subroutine
end module
