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

! A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
!
!          a^2 + b^2 = c^2
!
! For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
! There exists exactly one Pythagorean triplet for which a + b + c = 1000.
! Find the product abc.
module MPEProblem9

    implicit none
    private

    type, public :: TPEProblem9
    contains
        procedure, nopass :: present
    end type
contains
    subroutine present
        write (*, '(A)') 'Problem 9. Special Pythagorean triplet.'

        write (*, '(A, I)') 'Product 1: ', product1()
    end subroutine

    pure function product1()
        integer, parameter:: limit = 1000
        integer product1
        integer a
        integer b
        integer c

        ! initial value
        product1 = 0

        do c = 1, limit
            do b = 1, c - 1
                do a = 1, b - 1
                    if (a + b + c .eq. limit) then
                        if ((a**2) + (b**2) .eq. (c**2)) then
                            product1 = a * b * c

                            return
                        end if
                    end if
                end do
            end do
        end do
    end function
end module
