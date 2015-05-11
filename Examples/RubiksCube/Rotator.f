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

module MERubiksCubeRotator

    use MERubiksCubeCommon
    use MERubiksCubeCube

    implicit none
    private

    type, public :: TERotator
        private
    contains
        procedure, nopass :: rotate
    end type

    ! structures to minimize the number of comparisons
    abstract interface
        subroutine IERotate(cube)
            import TECube
            class(TECube), intent(in out) :: cube
        end subroutine
    end interface

    type TERotatorPointer
        procedure(IERotate), pointer, nopass :: rotator
    end type

    type(TERotatorPointer), dimension(-NUMBER_OF_SIDES:NUMBER_OF_SIDES) :: ROTATORS
contains
    subroutine rotate(cube, type)
        class(TECube), intent(in out) :: cube
        integer, intent(in)           :: type

        ! initialize an array at the first call
        if (associated(ROTATORS(type)%rotator) /= .true.) then
            call init()
        end if

        call ROTATORS(type)%rotator(cube)
    end subroutine

    subroutine rotate_red_cw(cube)
        class(TECube), intent(in out)          :: cube
        integer, dimension(size(cube%cube, 1)) :: buffer

        call rotate_cw(cube%cube(1:3, 1:3, RED))

        buffer = cube%cube(1:3, 1, G)
        cube%cube(1:3, 1, G) = cube%cube(3, 1:3, W)
        cube%cube(3, 1:3, W) = reverse(cube%cube(1:3, 3, B))
        cube%cube(1:3, 3, B) = cube%cube(1, 1:3, Y)
        cube%cube(1, 1:3, Y) = reverse(buffer)
    end subroutine

    subroutine rotate_red_ccw(cube)
        class(TECube), intent(in out)          :: cube
        integer, dimension(size(cube%cube, 1)) :: buffer

        call rotate_ccw(cube%cube(1:3, 1:3, RED))

        buffer = cube%cube(1:3, 3, B)
        cube%cube(1:3, 3, B) = reverse(cube%cube(3, 1:3, W))
        cube%cube(3, 1:3, W) = cube%cube(1:3, 1, G)
        cube%cube(1:3, 1, G) = reverse(cube%cube(1, 1:3, Y))
        cube%cube(1, 1:3, Y) = buffer
    end subroutine

    subroutine rotate_white_cw(cube)
        class(TECube), intent(in out)          :: cube
        integer, dimension(size(cube%cube, 1)) :: buffer

        call rotate_cw(cube%cube(1:3, 1:3, WHITE))

        buffer = cube%cube(1, 1:3, G)
        cube%cube(1, 1:3, G) = cube%cube(1, 1:3, O)
        cube%cube(1, 1:3, O) = cube%cube(1, 1:3, B)
        cube%cube(1, 1:3, B) = cube%cube(1, 1:3, R)
        cube%cube(1, 1:3, R) = buffer
    end subroutine

    subroutine rotate_white_ccw(cube)
        class(TECube), intent(in out)          :: cube
        integer, dimension(size(cube%cube, 1)) :: buffer

        call rotate_ccw(cube%cube(1:3, 1:3, WHITE))

        buffer = cube%cube(1, 1:3, B)
        cube%cube(1, 1:3, B) = cube%cube(1, 1:3, O)
        cube%cube(1, 1:3, O) = cube%cube(1, 1:3, G)
        cube%cube(1, 1:3, G) = cube%cube(1, 1:3, R)
        cube%cube(1, 1:3, R) = buffer
    end subroutine

    subroutine rotate_blue_cw(cube)
        class(TECube), intent(in out)          :: cube
        integer, dimension(size(cube%cube, 1)) :: buffer

        call rotate_cw(cube%cube(1:3, 1:3, BLUE))

        buffer = cube%cube(1:3, 1, R)
        cube%cube(1:3, 1, R) = cube%cube(1:3, 1, W)
        cube%cube(1:3, 1, W) = reverse(cube%cube(1:3, 3, O))
        cube%cube(1:3, 3, O) = reverse(cube%cube(1:3, 1, Y))
        cube%cube(1:3, 1, Y) = buffer
    end subroutine

    subroutine rotate_blue_ccw(cube)
        class(TECube), intent(in out)          :: cube
        integer, dimension(size(cube%cube, 1)) :: buffer

        call rotate_ccw(cube%cube(1:3, 1:3, BLUE))

        buffer = cube%cube(1:3, 3, O)
        cube%cube(1:3, 3, O) = reverse(cube%cube(1:3, 1, W))
        cube%cube(1:3, 1, W) = cube%cube(1:3, 1, R)
        cube%cube(1:3, 1, R) = cube%cube(1:3, 1, Y)
        cube%cube(1:3, 1, Y) = reverse(buffer)
    end subroutine

    subroutine rotate_yellow_cw(cube)
        class(TECube), intent(in out)          :: cube
        integer, dimension(size(cube%cube, 1)) :: buffer

        call rotate_cw(cube%cube(1:3, 1:3, YELLOW))

        buffer = cube%cube(3, 1:3, G)
        cube%cube(3, 1:3, G) = cube%cube(3, 1:3, R)
        cube%cube(3, 1:3, R) = cube%cube(3, 1:3, B)
        cube%cube(3, 1:3, B) = cube%cube(3, 1:3, O)
        cube%cube(3, 1:3, O) = buffer
    end subroutine

    subroutine rotate_yellow_ccw(cube)
        class(TECube), intent(in out)          :: cube
        integer, dimension(size(cube%cube, 1)) :: buffer

        call rotate_ccw(cube%cube(1:3, 1:3, YELLOW))

        buffer = cube%cube(3, 1:3, B)
        cube%cube(3, 1:3, B) = cube%cube(3, 1:3, R)
        cube%cube(3, 1:3, R) = cube%cube(3, 1:3, G)
        cube%cube(3, 1:3, G) = cube%cube(3, 1:3, O)
        cube%cube(3, 1:3, O) = buffer
    end subroutine

    subroutine rotate_green_cw(cube)
        class(TECube), intent(in out)          :: cube
        integer, dimension(size(cube%cube, 1)) :: buffer

        call rotate_cw(cube%cube(1:3, 1:3, GREEN))

        buffer = cube%cube(1:3, 1, O)
        cube%cube(1:3, 1, O) = reverse(cube%cube(1:3, 3, W))
        cube%cube(1:3, 3, W) = cube%cube(1:3, 3, R)
        cube%cube(1:3, 3, R) = cube%cube(1:3, 3, Y)
        cube%cube(1:3, 3, Y) = reverse(buffer)
    end subroutine

    subroutine rotate_green_ccw(cube)
        class(TECube), intent(in out)          :: cube
        integer, dimension(size(cube%cube, 1)) :: buffer

        call rotate_ccw(cube%cube(1:3, 1:3, GREEN))

        buffer = cube%cube(1:3, 3, R)
        cube%cube(1:3, 3, R) = cube%cube(1:3, 3, W)
        cube%cube(1:3, 3, W) = reverse(cube%cube(1:3, 1, O))
        cube%cube(1:3, 1, O) = reverse(cube%cube(1:3, 3, Y))
        cube%cube(1:3, 3, Y) = buffer
    end subroutine

    subroutine rotate_orange_cw(cube)
        class(TECube), intent(in out)          :: cube
        integer, dimension(size(cube%cube, 1)) :: buffer

        call rotate_cw(cube%cube(1:3, 1:3, ORANGE))

        buffer = cube%cube(1:3, 1, B)
        cube%cube(1:3, 1, B) = reverse(cube%cube(1, 1:3, W))
        cube%cube(1, 1:3, W) = cube%cube(1:3, 3, G)
        cube%cube(1:3, 3, G) = reverse(cube%cube(3, 1:3, Y))
        cube%cube(3, 1:3, Y) = buffer
    end subroutine

    subroutine rotate_orange_ccw(cube)
        class(TECube), intent(in out)          :: cube
        integer, dimension(size(cube%cube, 1)) :: buffer

        call rotate_ccw(cube%cube(1:3, 1:3, ORANGE))

        buffer = cube%cube(1:3, 3, G)
        cube%cube(1:3, 3, G) = cube%cube(1, 1:3, W)
        cube%cube(1, 1:3, W) = reverse(cube%cube(1:3, 1, B))
        cube%cube(1:3, 1, B) = cube%cube(3, 1:3, Y)
        cube%cube(3, 1:3, Y) = reverse(buffer)
    end subroutine

    subroutine rotate_cw(array)
        integer, dimension(:,:), intent(in out) :: array

        array = transpose(array(size(array, 1):1:-1,:))
    end subroutine

    subroutine rotate_ccw(array)
        integer, dimension(:,:), intent(in out) :: array

        array = transpose(array(:,size(array, 1):1:-1))
    end subroutine

    function reverse(array) result(value)
        integer, dimension(:), intent(in out) :: array
        integer, dimension(size(array))       :: value

        value = array(size(array):1:-1)
    end function

    subroutine init()
        ROTATORS(RED_CW)%rotator     => rotate_red_cw
        ROTATORS(RED_CCW)%rotator    => rotate_red_ccw

        ROTATORS(WHITE_CW)%rotator   => rotate_white_cw
        ROTATORS(WHITE_CCW)%rotator  => rotate_white_ccw

        ROTATORS(BLUE_CW)%rotator    => rotate_blue_cw
        ROTATORS(BLUE_CCW)%rotator   => rotate_blue_ccw

        ROTATORS(YELLOW_CW)%rotator  => rotate_yellow_cw
        ROTATORS(YELLOW_CCW)%rotator => rotate_yellow_ccw

        ROTATORS(GREEN_CW)%rotator   => rotate_green_cw
        ROTATORS(GREEN_CCW)%rotator  => rotate_green_ccw

        ROTATORS(ORANGE_CW)%rotator  => rotate_orange_cw
        ROTATORS(ORANGE_CCW)%rotator => rotate_orange_ccw
    end subroutine
end module
