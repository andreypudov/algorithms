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

module MGreatestCommonDivisor

    implicit none
    private

    type, public :: TGreatestCommonDivisor
    contains
        procedure, nopass :: gcd => gcdOriginal

        procedure, nopass :: gcdOriginal
    end type
contains
    function gcdOriginal(value1, value2) result(gcd)
        integer, intent(in) :: value1
        integer, intent(in) :: value2
        integer :: gcd
        integer :: v1
        integer :: v2

        v1 = value1
        v2 = value2

        do while (v1 > 0)
            if (v1 < v2) then
                gcd = v1
                v1  = v2
                v2  = gcd
            end if

            v1 = v1 - v2
        end do

        gcd = v2
    end function
end module
