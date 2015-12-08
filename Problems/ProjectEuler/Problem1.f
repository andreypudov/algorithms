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

! Multiples of 3 and 5
!
! If we list all the natural numbers below 10 that are multiples of 3 or 5,
! we get 3, 5, 6 and 9. The sum of these multiples is 23.
!
! Find the sum of all the multiples of 3 or 5 below 1000.
module MPEProblem1

    implicit none
    private

    type, public :: TPEProblem1
    contains
        procedure, nopass :: present
    end type
contains
    subroutine present
        write (*, '(A)') 'Problem 1. Multiples of 3 and 5.'

        write (*, '(A, I)') 'Multiply 1: ', multiply1()
        write (*, '(A, I)') 'Multiply 2: ', multiply2()
    end subroutine

    ! A simple way to do this is to go through all numbers from 1 to 999
    ! and test whether they are divisible by 3 or by 5.
    function multiply1()
        integer multiply1
        integer index

        ! initial value
        multiply1 = 0

        do index = 2, 999
            if ((mod(index, 3) .eq. 0) .or. (mod(index, 5) .eq. 0)) then
                multiply1 = multiply1 + index
            end if
        end do
    end function

    ! To get a more efficient solution you could also calculate the sum of
    ! the numbers less than 1000 that are divisible by 3, plus the sum of
    ! the numbers less than 1000 that are divisible by 5. But as you have
    ! summed numbers divisible by 15 twice you would have to subtract the
    ! sum of the numbers divisible by 15
    function multiply2()
        integer multiply2

        multiply2 = sumDevisibleBy(3) + sumDevisibleBy(5)  - sumDevisibleBy(15)
    end function

    ! 3 + 6 + 9 + 12 + ...... + 999 = 3 * (1 + 2 + 3 + 4 + ... + 333)
    function sumDevisibleBy(num)
        integer sumDevisibleBy
        integer num
        integer index

        ! nearest integer to the argument
        index = nint(999.0 / num)

        sumDevisibleBy = nint(DBLE(num * (index * (index + 1))) / 2)
    end function
end module
