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

module MUParameters

    implicit none
    private

    ! constant complexity O(1)
    integer, parameter, public :: CONSTANT_COMPLEXITY     = 10000

    ! logarithmic complexity O(logN)
    integer, parameter, public :: LOGARITHMIC_COMPLEXITY  = 10000

    ! linear complexity O(N)
    integer, parameter, public :: LINEAR_COMPLEXITY       = 10000

    ! linearithmic complexity O(NlogN)
    integer, parameter, public :: LINEARITHMIC_COMPLEXITY = 10000

    ! quadratic complexity O(N^2)
    integer, parameter, public :: QUADRATIC_COMPLEXITY    = 25000

    ! cubic complexity O(N^3)
    integer, parameter, public :: CUBIC_COMPLEXITY        = 10000

    ! exponential complexity O(2^N)
    integer, parameter, public :: EXPONENTIAL_COMPLEXITY  = 10000
end module
