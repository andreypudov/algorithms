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

module MUGreatestCommonDivisor

    use MGreatestCommonDivisor
    use MUAsserts
    use MUReport

    implicit none
    private

    integer, parameter :: NUMBER_OF_ITERATIONS = 10000000

    type, public :: TUGreatestCommonDivisor
    contains
        procedure :: present
    end type

contains
    subroutine present(instance)
        class(TUGreatestCommonDivisor), intent(in) :: instance

        call gcdOriginal()
    end subroutine

    subroutine gcdOriginal()
        type(TGreatestCommonDivisor) :: greatestCommonDivisor
        integer index
        integer gcd
        real    start

        call cpu_time(start)

        do index = 1, NUMBER_OF_ITERATIONS
            gcd = greatestCommonDivisor%gcdOriginal(461952, 116298)
            call assert_equals(gcd, 18)
        end do

        call report('GCD', 'Original', '', start)

        print *, ''
    end subroutine
end module
