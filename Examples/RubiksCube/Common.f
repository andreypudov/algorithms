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

    integer, parameter :: LENGTH_OF_SIDE  = 3
    integer, parameter :: NUMBER_OF_SIDES = 6

    integer, parameter :: R = 1, RED    = 1
    integer, parameter :: W = 2, WHITE  = 2
    integer, parameter :: B = 3, BLUE   = 3
    integer, parameter :: Y = 4, YELLOW = 4
    integer, parameter :: G = 5, GREEN  = 5
    integer, parameter :: O = 6, ORANGE = 6

    integer, parameter :: RED_CW     =  RED
    integer, parameter :: RED_CCW    = -RED
    integer, parameter :: WHITE_CW   =  WHITE
    integer, parameter :: WHITE_CCW  = -WHITE
    integer, parameter :: BLUE_CW    =  BLUE
    integer, parameter :: BLUE_CCW   = -BLUE
    integer, parameter :: YELLOW_CW  =  YELLOW
    integer, parameter :: YELLOW_CCW = -YELLOW
    integer, parameter :: GREEN_CW   =  GREEN
    integer, parameter :: GREEN_CCW  = -GREEN
    integer, parameter :: ORANGE_CW  =  ORANGE
    integer, parameter :: ORANGE_CCW = -ORANGE

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
