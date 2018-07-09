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

module MUPi

    use MPi
    use MUAsserts
    use MUReport

    implicit none
    private

    real, parameter :: PI_VALUE = 3.1415926

    type, public :: TUPi
    contains
        procedure, nopass :: present
    end type
contains
    subroutine present()
        call vietesSeries()
        call wallisSeries()
        call leibnizSeries()
        call nilakanthaSeries()

        print *, ''
    end subroutine

    subroutine vietesSeries()
        type(TPi) :: pi
        real value
        real start

        call cpu_time(start)
        value = pi%vietesSeries()
        call assert_equals(value, PI_VALUE)
        call report('Pi', 'Vietes', '', start)
    end subroutine

    subroutine wallisSeries()
        type(TPi) :: pi
        real value
        real start

        call cpu_time(start)
        value = pi%wallisSeries()
        call assert_equals(value, PI_VALUE)
        call report('Pi', 'Wallis', '', start)
    end subroutine

    subroutine leibnizSeries()
        type(TPi) :: pi
        real value
        real start

        call cpu_time(start)
        value = pi%leibnizSeries()
        call assert_equals(value, PI_VALUE)
        call report('Pi', 'Leibniz', '', start)
    end subroutine

    subroutine nilakanthaSeries()
        type(TPi) :: pi
        real value
        real start

        call cpu_time(start)
        value = pi%nilakanthaSeries()
        call assert_equals(value, PI_VALUE)
        call report('Pi', 'Nilakantha', '', start)
    end subroutine
end module
