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

! Lattice paths
!
! Starting in the top left corner of a 2x2 grid, and only being able to move to
! the right and down, there are exactly 6 routes to the bottom right corner.
!
!     ¯¯|    ¯|_     ¯|     |__     |_      |
!       |       |     |_       |      |_    |__
!
! How many such routes are there through a 20x20 grid?
module MPEProblem15

    implicit none
    private

    ! global variable to store available pathes
    integer :: pathes

    type, public :: TPEProblem15
    contains
        procedure, nopass :: present
    end type
contains
    subroutine present
        write (*, '(A)') 'Problem 15. Lattice paths.'
        call path(2, 2)
        write (*, '(A, I)') 'Path 1: ', pathes
    end subroutine

    !
    ! Offered solution implements following loop:
    !   1) step right if not visited
    !   2) step down if not visited and inverse all accesible steps (right and down)
    !   3) step back
    !
    subroutine path(width, height)
        integer, intent(in) :: width
        integer, intent(in) :: height


    end subroutine
end module
