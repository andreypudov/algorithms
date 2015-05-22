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

    implicit none
    private

    type, public :: TERubiksCube
    contains
        procedure, nopass :: present
    end type
contains
    subroutine present()
        type(TESearch)  search

        ! default state
        integer, dimension(NUMBER_OF_CUBICLES) :: default = &
               [R, R, R, R, R, R, R, R, R, W, W, W, W, W, W, W, W, W, &
                B, B, B, B, B, B, B, B, B, Y, Y, Y, Y, Y, Y, Y, Y, Y, &
                G, G, G, G, G, G, G, G, G, O, O, O, O, O, O, O, O, O]
        logical, dimension(NUMBER_OF_CUBICLES) :: defaultMask = .true.
        integer, dimension(12) :: defaultRotations = &
               [RED_CW, RED_CCW, WHITE_CW, WHITE_CCW, BLUE_CW, BLUE_CCW, &
               YELLOW_CW, YELLOW_CCW, GREEN_CW, GREEN_CCW, ORANGE_CW, ORANGE_CCW]

        ! source data
        integer, dimension(NUMBER_OF_CUBICLES) :: source = &
               [R, R, R, R, R, R, R, R, Y, W, W, W, W, W, W, G, W, W, &
                B, B, Y, B, B, B, B, B, B, Y, Y, O, Y, Y, G, G, O, G, &
                G, G, R, G, G, Y, Y, G, W, O, O, O, O, O, Y, O, O, B]
        !integer, dimension(NUMBER_OF_CUBICLES) :: destination = &
        !       [O, G, R, R, R, B, G, W, G, W, R, Y, Y, W, B, B, O, O, &
        !        O, W, R, Y, B, B, B, Y, G, W, O, Y, Y, Y, B, R, G, B, &
        !        Y, G, Y, G, G, R, R, R, O, W, W, W, O, O, W, G, O, B]
        integer, dimension(NUMBER_OF_CUBICLES) :: destination = &
               [R, R, R, R, R, R, R, R, y, W, W, W, W, W, W, G, W, W, &
                B, B, Y, B, B, B, B, B, B, Y, Y, O, Y, Y, G, g, O, g, &
                G, G, r, G, G, Y, Y, G, W, O, O, O, O, O, Y, o, O, B]
        logical, dimension(NUMBER_OF_CUBICLES) :: mask = &
               [.true., .true., .true., .true., .true., .true., .true., .true., .false., &
                .true., .true., .true., .true., .true., .true., .true., .true., .true., &
                .true., .true., .true., .true., .true., .true., .true., .true., .true., &
                .true., .true., .true., .true., .true., .true., .false., .true., .false., &
                .true., .true., .false., .true., .true., .true., .true., .true., .false., &
                .true., .true., .false., .true., .true., .true., .true., .true., .true.]
        !integer, dimension(12) :: rotations = &
        !       [RED_CW, RED_CCW, WHITE_CW, WHITE_CCW, BLUE_CW, BLUE_CCW, &
        !       YELLOW_CW, YELLOW_CCW, GREEN_CW, GREEN_CCW, ORANGE_CW, ORANGE_CCW]
        integer, dimension(6) :: rotations = &
               [BLUE_CW, BLUE_CCW, GREEN_CW, GREEN_CCW, ORANGE_CW, ORANGE_CCW]

        integer :: depth  = 11
        logical :: status = .false.

        ! search for desired state
        status = search%search(source, destination, mask, rotations, depth)
    end subroutine
end module
