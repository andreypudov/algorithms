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

submodule (UFoundation) ObjectEquals

    use Foundation

    use MUAsserts
    use MUReport

    ! Base object in hierarchy.
    type, extends(Object) :: Shape
        integer :: color
    end type

    type, extends(Shape) :: Circle
        integer :: radius
    end type

contains
    module subroutine presentObjectEquals()
        type(Shape), target  :: shape_object
        type(Circle), target :: circle_object

        type(Shape), pointer  :: shape_pointer
        type(Circle), pointer :: circle_pointer

        class(Object), pointer :: object_shape_pointer
        class(Object), pointer :: object_circle_pointer

        real start

        call cpu_time(start)

        shape_pointer  => shape_object
        circle_pointer => circle_object

        object_shape_pointer  => shape_object
        object_circle_pointer => circle_object

        call assert_ok(associated(shape_pointer, shape_object), '[1]')

        call assert_ok(shape_object%equals(shape_pointer), '[2]')
        call assert_ok(shape_pointer%equals(shape_object), '[3]')
        call assert_ok(circle_object%equals(circle_pointer), '[4]')
        call assert_ok(circle_pointer%equals(circle_object), '[5]')
        call assert_ok(.not. shape_object%equals(circle_pointer), '[6]')
        call assert_ok(.not. circle_object%equals(shape_pointer), '[7]')

        call assert_ok(shape_object%equals(object_shape_pointer), '[8]')
        call assert_ok(circle_object%equals(object_circle_pointer), '[9]')
        call assert_ok(.not. shape_object%equals(object_circle_pointer), '[10]')
        call assert_ok(.not. circle_object%equals(object_shape_pointer), '[11]')

        call report('Foundation', 'Object', 'Equals', start)
    end subroutine
end submodule
