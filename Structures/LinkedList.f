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

module MLinkedList

    use MList

    implicit none
    private

    type, extends(TList), public :: TLinkedList
    private
        type(TListEntry), pointer :: first => null()
        type(TListEntry), pointer :: last  => null()
    contains
        procedure :: add
        procedure :: contains
        procedure :: get
        !procedure :: iterator
        procedure :: remove
        procedure :: set
        procedure :: size

        procedure :: init
        procedure :: destroy
    end type

    type TListEntry
    private
        type(TListEntry), pointer :: next => null()
        integer                   :: value
    end type

contains
    subroutine add(instance, value)
        class(TLinkedList), intent(in out) :: instance
        integer, intent(in)                :: value

        type(TListEntry), pointer :: entry

        allocate(entry)
        entry%next  => null()
        entry%value =  value

        instance%last%next => entry

        if (associated(instance%first) /= .true.) then
            instance%first => entry
        end if
    end subroutine

    function contains(instance, value) result(status)
        class(TLinkedList), intent(in) :: instance
        integer, intent(in)            :: value
        logical                        :: status

        status = .false.
    end function

    function get(instance, index) result(value)
        class(TLinkedList), intent(in) :: instance
        integer, intent(in)            :: index
        integer                        :: value

        value = 0
    end function

    subroutine remove(instance, index)
        class(TLinkedList), intent(in out) :: instance
        integer, intent(in)                :: index
    end subroutine

    subroutine set(instance, index, value)
        class(TLinkedList), intent(in out) :: instance
        integer, intent(in)                :: index
        integer, intent(in)                :: value
    end subroutine

    function size(instance) result(value)
        class(TLinkedList), intent(in) :: instance
        integer                        :: value

        value = 0
    end function

    subroutine init(instance)
        class(TLinkedList), intent(in out) :: instance

        ! left blank
    end subroutine

    subroutine destroy(instance)
        class(TLinkedList), intent(in out) :: instance

        ! left blank
    end subroutine
end module
