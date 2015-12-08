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

! Largest prime factor
!
! The prime factors of 13195 are 5, 7, 13 and 29.
! What is the largest prime factor of the number 600851475143 ?
module MPEProblem3

    implicit none
    private

    type, public :: TPEProblem3
    contains
        procedure, nopass :: present
    end type
contains
    subroutine present
        write (*, '(A)') 'Problem 3. Largest prime factor.'

        write (*, '(A, I)') 'Factor 1: ', factor()
    end subroutine

    function factor()
        integer*8 factor
        integer*8 index

        factor = 600851475143
        index  = 2

        do while ((index ** 2) .le. factor)
            if (mod(factor, index) .eq. 0) then
                factor = factor / index
            else
                index = index + 1
            end if
        end do
    end function
end module
