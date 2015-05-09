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

module MERubiksCube

    use MERubiksCubeCommon
    use MERubiksCubeCube
    use MERubiksCubeRotator

    implicit none
    private

    type, public :: TERubiksCube
    contains
        procedure :: present
    end type
contains
    subroutine present(this)
        class(TERubiksCube), intent(in) :: this

        type(TECube)    cube
        type(TERotator) rotator

        cube    = TECube()
        rotator = TERotator()

        cube%cube(1, 1, WHITE)  = ORANGE
        cube%cube(1, 1, RED)    = WHITE
        cube%cube(1, 1, YELLOW) = RED
        cube%cube(1, 3, ORANGE) = BLUE
        cube%cube(1, 1, BLUE)   = ORANGE

        print '(A)', 'Initial state: '
        call cube%set([R, R, R, R, R, R, R, R, Y, W, W, W, W, W, W, G, W, W, &
                B, B, Y, B, B, B, B, B, B, Y, Y, O, Y, Y, G, G, O, G, &
                G, G, R, G, G, Y, Y, G, W, O, O, O, O, O, Y, O, O, B])
        call cube%print()

        call rotator%rotate(cube, ORANGE_CCW)

        print '(/A)', 'Rotated state: '
        call cube%print()
    end subroutine
end module
