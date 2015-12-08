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

! Sum square difference
!
! The sum of the squares of the first ten natural numbers is,
! 1^2 + 2^2 + ... + 10^2 = 385
!
! The square of the sum of the first ten natural numbers is,
! (1 + 2 + ... + 10)^2 = 55^2 = 3025
!
! Hence the difference between the sum of the squares of the first ten natural
! numbers and the square of the sum is 3025 - 385 = 2640.
!
! Find the difference between the sum of the squares of the first one hundred
! natural numbers and the square of the sum.
module MPEProblem6

    implicit none
    private

    type, public :: TPEProblem6
    contains
        procedure, nopass :: present
    end type
contains
    subroutine present
        write (*, '(A)') 'Problem 6. Sum square difference.'

        write (*, '(A, I)') 'Difference 1: ', difference1()
    end subroutine

    function difference1()
        integer difference1
        integer sum
        integer square
        integer index

        ! initial values
        sum    = 0
        square = 0

        do concurrent (index = 1:100)
            ! the sum of the squares
            sum = sum + index ** 2

            ! the square of the sum
            square = square + index
        end do

        square = square ** 2

        difference1 = square - sum
    end function
end module
