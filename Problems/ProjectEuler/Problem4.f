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

! Largest palindrome product
!
! A palindromic number reads the same both ways. The largest palindrome made
! from the product of two 2-digit numbers is 9009 = 91x99.?
! Find the largest palindrome made from the product of two 3-digit numbers.
module MPEProblem4

    implicit none
    private

    type, public :: TPEProblem4
    contains
        procedure, nopass :: present
    end type
contains
    subroutine present
        write (*, '(A)') 'Problem 4. Largest palindrome product.'

        write (*, '(A, I)') 'Palindrome 1: ', palindrome()
    end subroutine

    function palindrome()
        integer palindrome
        integer current
        integer number1
        integer number2

        integer number
        integer digit
        integer remaining

        palindrome = 0

        do number1 = 999, 100, -1
            do number2 = 999, 100, -1
                current = number1 * number2

                number    = current
                remaining = 0
                do while (number .gt. 0)
                    digit     = mod(number, 10)
                    remaining = remaining * 10 + digit
                    number    = number / 10
                end do

                if (remaining .eq. current) then
                    if (current .gt. palindrome) then
                        palindrome = current
                    end if
                end if
            end do
        end do
    end function
end module
