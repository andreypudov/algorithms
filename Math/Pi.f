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

module MPi

    implicit none
    private

    type, public :: TPi
    contains
        procedure, nopass :: vietesSeries
        procedure, nopass :: wallisSeries
        procedure, nopass :: leibnizSeries
        procedure, nopass :: nilakanthaSeries
    end type
contains
    function vietesSeries() result(pi)
        real :: pi

        integer, parameter :: ITERATIONS = 15
        integer index
        integer jndex

        real factor

        pi = 1
        do index = ITERATIONS, 2, -1
            factor = 2

            do jndex = 1, index - 1
                factor = 2 + sqrt(factor)
            end do

            factor = sqrt(factor)
            pi = pi * factor / 2
        end do

        pi = pi * sqrt(2.0) / 2
        pi = 2 / pi
    end function

    function wallisSeries() result(pi)
        real(8) :: pi

        integer, parameter :: ITERATIONS = 900000000
        real(8) index

        index = 3
        pi    = 4
        do while (index <= (ITERATIONS + 2))
            pi = pi * ((index - 1) / index) * ((index + 1) / index)

            index = index + 2
        end do
    end function

    function leibnizSeries() result(pi)
        real(8) :: pi

        integer, parameter :: ITERATIONS = 900000000
        integer sign
        integer index

        index = 1
        sign  = 1
        pi    = 0
        do while (index <= (ITERATIONS * 2))
            pi   = pi + sign * (4.0 / index)
            sign = -sign

            index = index + 2
        end do
    end function

    function nilakanthaSeries() result(pi)
        real(8) :: pi

        integer, parameter :: ITERATIONS = 500
        integer sign
        real(8) index

        index = 2
        sign  = 1
        pi    = 3
        do while (index <= (ITERATIONS * 2))
            pi   = pi + sign * (4 / (index * (index + 1) * (index + 2)))
            sign = -sign

            index = index + 2
        end do
    end function
end module
