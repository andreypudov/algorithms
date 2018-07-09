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

submodule (UFoundation) ObjectInit

    use Foundation

    use MUAsserts
    use MUReport

    type, extends(Object) :: Shape
        integer :: color
    contains
        procedure :: init
        procedure :: initWithColor
    end type

contains
    module subroutine presentObjectInit()
        type(Shape), pointer :: shape_dynamic
        type(Shape)          :: shape_static

        real start

        call cpu_time(start)

        allocate(shape_dynamic)
        call shape_dynamic%init()
        call shape_static%init()

        call assert_ok(associated(shape_dynamic), '[1]')

        call shape_dynamic%destroy()
        call shape_static%destroy()
        deallocate(shape_dynamic)

        call report('Foundation', 'Object', 'Init', start)
    end subroutine

    subroutine init(self)
        class(Shape), intent(in out) :: self

        ! call initializer subroutine of object class
        call self%object%init()

        self%color = 0
    end subroutine

    subroutine initWithColor(self, color)
        class(Shape), intent(in out) :: self
        integer, intent(in)          :: color

        ! call default initializer of shape object
        call self%init()

        self%color = color
    end subroutine
end submodule
