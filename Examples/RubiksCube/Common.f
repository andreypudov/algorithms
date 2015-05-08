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

module MERubiksCubeCommon

    implicit none
    public

    integer, parameter :: CUBE_LENGTH_OF_SIDE  = 3
    integer, parameter :: CUBE_NUMBER_OF_SIDES = 6

    integer, parameter :: CUBE_RED    = 1
    integer, parameter :: CUBE_WHITE  = 2
    integer, parameter :: CUBE_BLUE   = 3
    integer, parameter :: CUBE_YELLOW = 4
    integer, parameter :: CUBE_GREEN  = 5
    integer, parameter :: CUBE_ORANGE = 6

    integer, parameter :: CUBE_ROTATE_RED_CW     =  CUBE_RED
    integer, parameter :: CUBE_ROTATE_RED_CCW    = -CUBE_RED
    integer, parameter :: CUBE_ROTATE_WHITE_CW   =  CUBE_WHITE
    integer, parameter :: CUBE_ROTATE_WHITE_CCW  = -CUBE_WHITE
    integer, parameter :: CUBE_ROTATE_BLUE_CW    =  CUBE_BLUE
    integer, parameter :: CUBE_ROTATE_BLUE_CCW   = -CUBE_BLUE
    integer, parameter :: CUBE_ROTATE_YELLOW_CW  =  CUBE_YELLOW
    integer, parameter :: CUBE_ROTATE_YELLOW_CCW = -CUBE_YELLOW
    integer, parameter :: CUBE_ROTATE_GREEN_CW   =  CUBE_GREEN
    integer, parameter :: CUBE_ROTATE_GREEN_CCW  = -CUBE_GREEN
    integer, parameter :: CUBE_ROTATE_ORANGE_CW  =  CUBE_ORANGE
    integer, parameter :: CUBE_ROTATE_ORANGE_CCW = -CUBE_ORANGE

    character, parameter, dimension(0:6) :: CUBE_COLORS = [' ', 'R', 'W', 'B', 'Y', 'G', 'O']

    ! 30 - black    35 - purple        93 - light yellow
    ! 31 - red      36 - aqua          94 - light blue
    ! 32 - green    90 - dark grey     95 - pink
    ! 33 - yellow   91 - peach         96 - light aqua
    ! 34 - blue     92 - light green   97 - pearl white
    character(len=10), parameter, dimension(0:6) :: COLORIFY = &
           [achar(27) // '[34m ' // achar(27) // '[0m', &
            achar(27) // '[31mR' // achar(27) // '[0m', &
            achar(27) // '[97mW' // achar(27) // '[0m', &
            achar(27) // '[34mB' // achar(27) // '[0m', &
            achar(27) // '[33mY' // achar(27) // '[0m', &
            achar(27) // '[32mG' // achar(27) // '[0m', &
            achar(27) // '[35mO' // achar(27) // '[0m']

end module
