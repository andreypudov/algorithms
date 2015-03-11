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

module MUFibonacci

    use MFibonacci
    use MUAsserts
    use MUReport

    implicit none
    private

    integer, parameter :: NUMBER_OF_ITERATIONS = 6000000

    type, public :: TUFibonacci
    contains
        procedure, nopass :: present
    end type

contains
    subroutine present()
        call fibonacciRecursive()
        call fibonacciIterate()
        print *, ''
    end subroutine

    subroutine fibonacciRecursive()
        type(TFibonacci) :: fibonacci
        integer index
        integer value
        real    start

        call cpu_time(start)

        do index = 1, NUMBER_OF_ITERATIONS
            value = fibonacci%fibonacciRecursive(10)
            call assert_equals(value, 55)
        end do

        call report('Fibonacci', 'Recursive', '', start)
    end subroutine

    subroutine fibonacciIterate()
        type(TFibonacci) :: fibonacci
        integer index
        integer value
        real    start

        call cpu_time(start)

        do index = 1, NUMBER_OF_ITERATIONS
            value = fibonacci%fibonacciIterate(10)
            call assert_equals(value, 55)
        end do

        call report('Fibonacci', 'Iterate', '', start)
    end subroutine
end module
