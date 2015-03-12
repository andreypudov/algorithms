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

module MLinearCongruential

    use MRandom

    implicit none
    private

    integer, parameter :: m  = 100000000
    integer, parameter :: m1 = 10000
    integer, parameter :: b  = 31415821

    type, extends(TRandom), public :: TLinearCongruential
    contains
        procedure, nopass :: random
    end type
contains
    function random()
        integer :: random

        integer, save :: value = 1234567
        value = mod(multiply(value, b) + 1, m)

        random = value
    end function

    !
    ! Computes (value1 * value2 mod m) with no overflow.
    !
    function multiply(value1, value2)
        integer, intent(in) :: value1
        integer, intent(in) :: value2
        integer :: multiply

        integer p1
        integer p0
        integer q1
        integer q0

        p1 = value1 / m1
        p0 = mod(value1, m1)

        q1 = value2 / m1
        q0 = mod(value2, m1)

        multiply = mod((mod(p0 * q1 + p1 * q0, m1) * m1 + p0 * q0), m)
    end function
end module
