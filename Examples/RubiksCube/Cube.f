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

!
!        2 2 2
!        2 2 2
!        2 2 2
!
! 3 3 3  1 1 1  5 5 5  6 6 6
! 3 3 3  1 1 1  5 5 5  6 6 6
! 3 3 3  1 1 1  5 5 5  6 6 6
!
!        4 4 4
!        4 4 4
!        4 4 4
!

module MERubiksCubeCube

    use MERubiksCubeCommon

    implicit none
    private

    type, public :: TECube
    private
        integer, dimension(LENGTH_OF_SIDE, LENGTH_OF_SIDE, NUMBER_OF_SIDES), public :: cube
    contains
        procedure :: get
        procedure :: set

        procedure :: print

        final :: detroy
    end type

    interface TECube
        procedure :: init
    end interface
contains
    function get(this) result(vector)
        class(TECube), intent(in)           :: this
        integer, dimension(size(this%cube)) :: vector

        vector = reshape(this%cube, [size(this%cube)])
    end function

    subroutine set(this, vector)
        class(TECube), intent(in out)     :: this
        integer, dimension(:), intent(in) :: vector

        this%cube = reshape(vector, shape(this%cube))
    end subroutine

    subroutine print(this)
        class(TECube), intent(in) :: this
        integer, dimension(9, 12) :: buffer

        integer index

        buffer(1:3, 4:6)   = this%cube(1:3, 1:3, WHITE)
        buffer(4:6, 1:3)   = this%cube(1:3, 1:3, BLUE)
        buffer(4:6, 4:6)   = this%cube(1:3, 1:3, RED)
        buffer(4:6, 7:9)   = this%cube(1:3, 1:3, GREEN)
        buffer(4:6, 10:12) = this%cube(1:3, 1:3, ORANGE)
        buffer(7:9, 4:6)   = this%cube(1:3, 1:3, YELLOW)

        do index = 1, size(buffer, 1)
            !print '(\A2)', CUBE_COLORS(buffer(index, :))
            print '(\A11)', COLORIFY(buffer(index, :))
            print '(X)'
        end do
    end subroutine

    function init() result(object)
        type(TECube) :: object

        integer index

        do index = 1, size(object%cube, 3)
            object%cube(1:3, 1:3, index) = index
        end do
    end function

    subroutine detroy(this)
        type(TECube), intent(in out) :: this
    end subroutine
end module
