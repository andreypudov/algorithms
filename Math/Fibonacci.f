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

module MFibonacci

    implicit none
    private

    type, public :: TFibonacci
    contains
        procedure, nopass :: fibonacci => fibonacciRecursive

        procedure, nopass :: fibonacciRecursive
        procedure, nopass :: fibonacciIterate
    end type
contains
    recursive function fibonacciRecursive(position) result(fibonacci)
        integer, intent(in) :: position
        integer :: fibonacci

        if (position <= 2) then
            fibonacci = 1
            return
        end if

        fibonacci = fibonacciRecursive(position - 1) + fibonacciRecursive(position - 2)
    end function

    function fibonacciIterate(position) result(fibonacci)
        integer, intent(in) :: position
        integer :: fibonacci

        integer, dimension(10), save :: buffer
        integer, save                :: index  = 3
        buffer(1) = 1
        buffer(2) = 1

        if (buffer(position) == 0) then
            do index = index, position
                buffer(index) = buffer(index - 1) + buffer(index - 2)
            end do
        end if

        fibonacci = buffer(position)
    end function
end module
