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

module MERubiksCubeRotation

    use MERubiksCubeCommon
    use MERubiksCubeCube

    implicit none
    private

    type, public :: TERotation
        private
    contains
        procedure, nopass :: rotate
    end type
contains
    subroutine rotate(cube, type)
        class(TECube), intent(in out) :: cube
        integer, intent(in)           :: type

        select case(type)
            case (CUBE_ROTATE_RED_CW)
                call rotate_red_cw(cube)
        end select
    end subroutine

    subroutine rotate_red_cw(cube)
        class(TECube), intent(in out) :: cube

        call rotate_cw(cube%cube(1:3, 1:3, 1))
    end subroutine rotate_red_cw

    subroutine rotate_cw(array)
        integer, dimension(:,:), intent(in out) :: array

        array = transpose(array(size(array, 1):1:-1,:))
    end subroutine
end module
