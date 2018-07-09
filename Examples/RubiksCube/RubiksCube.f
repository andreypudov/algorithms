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
    use MERubiksCubeSearch

    use MERubiksCubeCube
    use MERubiksCubeRotator

    implicit none
    private

    logical, dimension(NUMBER_OF_CUBICLES), parameter :: DEFAULT_MASK  = .true.

    integer, dimension(NUMBER_OF_CUBICLES), parameter :: DEFAULT_STATE = &
            [R, R, R, R, R, R, R, R, R, W, W, W, W, W, W, W, W, W, &
             B, B, B, B, B, B, B, B, B, Y, Y, Y, Y, Y, Y, Y, Y, Y, &
             G, G, G, G, G, G, G, G, G, O, O, O, O, O, O, O, O, O]

    integer, dimension(12), parameter :: DEFAULT_ROTATIONS = &
               [RED_CW, RED_CCW, WHITE_CW, WHITE_CCW, BLUE_CW, BLUE_CCW, &
               YELLOW_CW, YELLOW_CCW, GREEN_CW, GREEN_CCW, ORANGE_CW, ORANGE_CCW]

    type, public :: TERubiksCube
    contains
        procedure, nopass :: present
    end type
contains
    subroutine present()
        type(TESearch)  search

        ! list of colors in top to bottom / left to right order
        integer, dimension(NUMBER_OF_CUBICLES) :: source = &
               [R, R, R, Y, R, R, W, G, B, & ! red
                W, W, G, W, W, R, G, G, O, & ! white
                B, B, B, B, B, B, W, B, B, & ! blue
                Y, Y, Y, W, Y, Y, R, O, G, & ! yellow
                G, W, W, Y, G, G, Y, R, Y, & ! green
                O, G, R, O, O, O, O, O, O]   ! orange

        integer, dimension(NUMBER_OF_CUBICLES) :: destination = &
               [R, R, R, R, R, R, R, R, R, &
                W, W, W, W, W, W, W, W, W, &
                B, B, B, B, B, B, B, B, B, &
                Y, Y, Y, Y, Y, Y, Y, Y, Y, &
                G, G, G, G, G, G, G, G, G, &
                O, O, O, O, O, O, O, O, O]

        logical, dimension(NUMBER_OF_CUBICLES) :: mask = &
               [.true., .true., .true., .true., .true., .true., .true., .true., .true., &
                .true., .true., .true., .true., .true., .true., .true., .true., .true., &
                .true., .true., .true., .true., .true., .true., .true., .true., .true., &
                .true., .true., .true., .true., .true., .true., .true., .true., .true., &
                .true., .true., .true., .true., .true., .true., .true., .true., .true., &
                .true., .true., .true., .true., .true., .true., .true., .true., .true.]

        integer, dimension(12) :: rotations = &
               [RED_CW, RED_CCW, WHITE_CW, WHITE_CCW, BLUE_CW, BLUE_CCW, &
               YELLOW_CW, YELLOW_CCW, GREEN_CW, GREEN_CCW, ORANGE_CW, ORANGE_CCW]

        integer :: depth  = 20
        logical :: status = .false.

        call presentRotation()

        ! search for desired state
        ! RED_CW BLUE_CCW RED_CW RED_CW WHITE_CW WHITE_CW ORANGE_CW WHITE_CW
        ! ORANGE_CCW WHITE_CW RED_CCW YELLOW_CW YELLOW_CW ORANGE_CW ORANGE_CW
        ! BLUE_CW BLUE_CW YELLOW_CCW ORANGE_CW GREEN_CW GREEN_CW WHITE_CCW
        ! GREEN_CW GREEN_CW YELLOW_CCW RED_CW RED_CW
        ! status = search%search(source, destination, mask, rotations, depth)
    end subroutine

    subroutine presentRotation()
        type(TECube)    cube
        type(TERotator) rotator

        ! list of colors in top to bottom / left to right order
        integer, dimension(NUMBER_OF_CUBICLES) :: source = &
               [R, R, R, Y, R, R, W, G, B, & ! red
                W, W, G, W, W, R, G, G, O, & ! white
                B, B, B, B, B, B, W, B, B, & ! blue
                Y, Y, Y, W, Y, Y, R, O, G, & ! yellow
                G, W, W, Y, G, G, Y, R, Y, & ! green
                O, G, R, O, O, O, O, O, O]   ! orange

        integer, dimension(28) :: rotations = &
                [RED_CW, GREEN_CCW, RED_CW, RED_CW, WHITE_CW, WHITE_CW, &
                 ORANGE_CW, WHITE_CW, ORANGE_CCW, WHITE_CW, RED_CCW, YELLOW_CW, &
                 YELLOW_CW, ORANGE_CW, ORANGE_CW, BLUE_CW, BLUE_CW, YELLOW_CCW, &
                 ORANGE_CW, ORANGE_CW, GREEN_CW, GREEN_CW, WHITE_CCW, GREEN_CW, &
                 GREEN_CW, YELLOW_CCW, RED_CW, RED_CW]
        integer index

        print '(A)', 'Initial state: '
        call cube%set(source)
        call cube%print()
        print '(X)'

        do index = 1, size(rotations)
            call rotator%rotate(cube, rotations(index))
        end do

        print '(A)', 'Destination state: '
        call cube%print()
        print '(X)'
    end subroutine
end module
