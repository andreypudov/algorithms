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

module MArrayQueue

    use MQueue

    implicit none
    private

    integer, parameter :: DEFAULT_SIZE = 16

    type, extends(TQueue), public :: TArrayQueue
        private
        integer, dimension(:), allocatable :: array
        integer :: low  ! the first pushed element
        integer :: high ! the last pushed element
    contains
        procedure :: peek
        procedure :: pop
        procedure :: push

        procedure :: empty

        procedure :: init
        procedure :: destroy
    end type
contains
    ! Cycle algorithm explanation:
    !
    ! a) 1-lh   b) 1-l    b) -      c) 5-h    d) 5-h    e) 5-lh
    !              2         2-l       2-l       -         -
    !              3         3         3         3-l       -
    !              4-h       4-h       4         4         -
    !
    ! *In case no elements in queue, high and low are equals to zero.

    function peek(instance) result(value)
        class(TArrayQueue), intent(in) :: instance
        integer                        :: value

        value = 0
        if (instance%high /= 0) then
            value = instance%array(instance%low)
        end if
    end function

    function pop(instance) result(value)
        class(TArrayQueue), intent(in out) :: instance
        integer                            :: value

        value = 0
        if (count(instance) /= 0) then
            value        = instance%array(instance%low)
            instance%low = instance%low + 1

            if (instance%low > size(instance%array)) then
                instance%low = 1
            end if

            ! queue is empty now
            if (instance%low > instance%high) then
                instance%low  = 1
                instance%high = 0
            end if
        end if
    end function

    subroutine push(instance, value)
        class(TArrayQueue), intent(in out) :: instance
        integer, intent(in)                :: value

        integer, dimension(:), allocatable :: temporary_array
        integer length

        if ((count(instance) + 1) > size(instance%array)) then
            length = size(instance%array) * 3 / 2
            allocate(temporary_array(length))

            if (instance%low < instance%high) then
                temporary_array(1:size(instance%array)) = instance%array

                instance%high = instance%high + 1
                temporary_array(instance%high) = value
            else
                length = size(instance%array) - instance%low
                temporary_array(1:(length + 1))                            = instance%array(instance%low:size(instance%array))
                temporary_array((length + 2):(length + instance%high + 1)) = instance%array(1:instance%high)

                instance%low  = 1
                instance%high = length + instance%high + 1
            end if

            deallocate(instance%array)
            call move_alloc(temporary_array, instance%array)
        else
            instance%high = instance%high + 1
            if (instance%high > size(instance%array)) then
                instance%high = 1
            end if

            instance%array(instance%high) = value
        end if
    end subroutine

    function empty(instance) result(value)
        class(TArrayQueue), intent(in) :: instance
        logical :: value

        value = (count(instance) == 0)
    end function

    subroutine init(instance)
        class(TArrayQueue), intent(in out) :: instance

        allocate(instance%array(DEFAULT_SIZE))
        instance%low  = 1
        instance%high = 0
    end subroutine

    subroutine destroy(instance)
        class(TArrayQueue), intent(in out) :: instance

        deallocate(instance%array)
    end subroutine

    function count(instance) result(value)
        class(TArrayQueue), intent(in) :: instance
        integer ::value

        ! queue is empty
        if (instance%high == 0) then
            value = 0
            return
        end if

        select case (instance%high - instance%low)
        case (1:)
            value = instance%high - instance%low + 1
        case (:-1)
            value = (size(instance%array) - instance%low + 1) + instance%high
        case default
            value = 1
        end select
    end function
end module
