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

module MVertex

    implicit none
    private

    type, public :: TVertex
    private
        integer :: value
        logical :: visited
    contains
        procedure :: getValue
        procedure :: setValue

        procedure :: isVisited
        procedure :: setVisited

        procedure :: init
        procedure :: destroy
    end type
contains
    function getValue(instance) result(value)
        class(TVertex), intent(in) :: instance
        integer :: value

        value = instance%value
    end function

    subroutine setValue(instance, value)
        class(TVertex), intent(in out) :: instance
        integer, intent(in) :: value

        instance%value = value
    end subroutine

    function isVisited(instance) result(value)
        class(TVertex), intent(in) :: instance
        logical :: value

        value = instance%visited
    end function

    subroutine setVisited(instance, value)
        class(TVertex), intent(in out) :: instance
        logical, intent(in) :: value

        instance%visited = value
    end subroutine

    subroutine init(instance, value, visited)
        class(TVertex), intent(in out) :: instance
        integer, optional, intent(in)  :: value
        logical, optional, intent(in)  :: visited

        if ((present(value) == .false.) .or. (present(visited) == .false.)) then
            instance%value   = 0
            instance%visited = .false.

            return
        end if

        instance%value   = value
        instance%visited = visited
    end subroutine

    subroutine destroy(instance)
        class(TVertex), intent(in out) :: instance

        instance%value   = 0
        instance%visited = .false.
    end subroutine
end module
