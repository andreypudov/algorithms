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

module MListStack

    use MStack

    implicit none
    private

    type, extends(TStack), public :: TArrayStack
    private
        integer, dimension(:), allocatable :: array
        integer                            :: index
    contains
        procedure :: peek
        procedure :: pop
        procedure :: push

        procedure :: empty

        procedure :: init
        procedure :: destroy
    end type

contains
    function peek(instance) result(value)
        class(TArrayStack), intent(in) :: instance
        integer                        :: value

        value = 0
        if (instance%index /= 0) then
            value = instance%array(instance%index)
        end if
    end function

    function pop(instance) result(value)
        class(TArrayStack), intent(in out) :: instance
        integer                            :: value

        value = 0
        if (instance%index /= 0) then
            value          = instance%array(instance%index)
            instance%index = instance%index - 1
        end if
    end function

    subroutine push(instance, value)
        class(TArrayStack), intent(in out) :: instance
        integer, intent(in)                :: value

        integer, dimension(:), allocatable :: temporary_array
        integer length

        instance%index = instance%index + 1

        ! increase array size when required
        if (instance%index > size(instance%array)) then
            length = size(instance%array) * 3 / 2
            allocate(temporary_array(length))
            temporary_array(1:size(instance%array)) = instance%array
            call move_alloc(temporary_array, instance%array)
        end if

        instance%array(instance%index) = value
    end subroutine

    function empty(instance) result(value)
        class(TArrayStack), intent(in) :: instance
        logical :: value

        value = (instance%index == 0)
    end function

    subroutine init(instance)
        class(TArrayStack), intent(in out) :: instance

        !allocate(instance%array(DEFAULT_SIZE))
        instance%index = 0
    end subroutine

    subroutine destroy(instance)
        class(TArrayStack), intent(in out) :: instance

        deallocate(instance%array)
    end subroutine
end module
