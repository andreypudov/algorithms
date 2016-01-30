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

! Longest Collatz sequence
!
! The following iterative sequence is defined for the set of positive integers:
!
!    n  > n/2 (n is even)
!    n  > 3n + 1 (n is odd)
!
! Using the rule above and starting with 13, we generate the following sequence:
!
!    13 > 40 > 20 > 10 > 5 > 16 > 8 > 4 > 2 > 1
!
! It can be seen that this sequence (starting at 13 and finishing at 1) contains
! 10  terms. Although it has not been proved yet (Collatz Problem), it is
! thought that all starting numbers finish at 1.
!
! Which starting number, under one million, produces the longest chain?
!
! NOTE: Once the chain starts the terms are allowed to go above one million.
module MPEProblem14

    implicit none
    private

    type, public :: TPEProblem14
    contains
        procedure, nopass :: present
    end type
contains
    subroutine present
        write (*, '(A)') 'Problem 14. Longest Collatz sequence.'

        write (*, '(A, I)') 'Chain 1: ', chain1()
    end subroutine

    function chain1
        integer :: chain1
        integer :: index
        integer :: value
        integer :: max

        ! initial values
        chain1 = 0
        index  = 0
        value  = 0
        max    = 0

        do index = 1, 1000000
            value = get_length(index)

            if (value .gt. max) then
                max    = value
                chain1 = index
            end if
        end do
    end function

    function get_length(number)
        integer, intent(in) :: number

        integer :: get_length
        integer*8 :: value

        ! initial value
        get_length = 1
        value      = number

        do while (value .gt. 1)
            if (mod(value, 2) .eq. 0) then
                value = value / 2
            else
                value = (value * 3) + 1
            end if

            get_length = get_length + 1
        end do
    end function
end module
