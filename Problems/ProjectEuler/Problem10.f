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

! The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
!
! Find the sum of all the primes below two million.
module MPEProblem10

    implicit none
    private

    type, public :: TPEProblem10
    contains
        procedure, nopass :: present
    end type
contains
    subroutine present
        write (*, '(A)') 'Problem 10. Summation of primes.'

        write (*, '(A, I)') 'Sum 1: ', sum1()
    end subroutine

    pure function sum1()
        integer(8), parameter        :: limit = 2000000
        integer(8), dimension(limit) :: list

        integer(8) :: sum1
        integer(8) :: index
        integer(8) :: iterator

        ! initial value
        sum1     = 0
        iterator = 1

        ! fill the list of prime numbers
        do index = 2, limit
            if (isPrime(index)) then
                list(iterator) = index
                sum1 = sum1 + index

                iterator = iterator + 1
            end if
        end do
    end function

    pure function isPrime(number)
        integer(8), intent(in) :: number

        integer(8) devider
        logical(8) isPrime

        ! initial value
        isPrime = .true.

        do devider = 2, sqrt(real(number))
            if (mod(number, devider) .eq. 0) then
                isPrime = .false.
                exit
            end if
        end do
    end function
end module
