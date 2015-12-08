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
! By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
!
! What is the 10 001st prime number?
module MPEProblem7

    implicit none
    private

    type, public :: TPEProblem7
    contains
        procedure, nopass :: present
    end type
contains
    subroutine present
        write (*, '(A)') 'Problem 7. 10001st prime.'

        write (*, '(A, I)') 'Prime 1: ', prime1()
    end subroutine

    function prime1()
        integer index
        integer count
        integer prime1

        ! initial value
        index = 1
        count = 0

        do while (count .lt. 10001)
            index = index + 1

            if (isPrime(index) .eq. .true.) then
                count = count + 1
            end if
        end do

        prime1 = index
    end function

    function isPrime(number)
        integer number
        integer devider
        logical isPrime

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
