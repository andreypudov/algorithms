!
! The Laboratory of Algorithms
!
! The MIT License
!
! Copyright 2011-2016 Andrey Pudov.
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

module UFoundation

    use Foundation

    use MUAsserts
    use MUReport

    implicit none
    private

    type, public :: UObject
    contains
        procedure, nopass :: present
    end type

    ! Base object in hierarchy.
    type, extends(Object), public :: Shape
        integer :: color
    contains
        procedure :: getColor
        procedure :: setColor
    end type

    type, extends(Shape), public :: Circle
        integer :: radius
    contains
        procedure :: getRadius
        procedure :: setRadius
    end type
contains
    subroutine present()
        type(Shape)  :: shape
        type(Circle) :: circle

        real start

        call shape%setColor(1)
        call circle%setColor(3)
        call circle%setRadius(32)

        call assert_equals(shape%getColor(), 1)
        call assert_equals(circle%getColor(), 3)
        call assert_equals(circle%getRadius(), 32)

        call report('Foundation', 'Object', 'Inherit.', start)
    end subroutine

    function getColor(self) result(color)
        class(Shape), intent(in) :: self
        integer :: color

        color = self%color
    end function

    subroutine setColor(self, color)
        class(Shape), intent(in out) :: self
        integer, intent(in) :: color

        self%color = color
    end subroutine

    function getRadius(self) result(radius)
        class(Circle), intent(in) :: self
        integer :: radius

        radius = self%radius
    end function

    subroutine setRadius(self, radius)
        class(Circle), intent(in out) :: self
        integer, intent(in) :: radius

        self%radius = radius
    end subroutine
end module
