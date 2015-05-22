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

module MFArrays

    use MUAsserts
    use MUReport

    implicit none
    private

    integer, parameter :: NUMBER_OF_ELEMENTS = 400000000

    type, public :: TFArrays
    contains
        procedure, nopass :: present
    end type

    interface getArray
        module procedure getArrayOneDimension, getArrayTwoDimension
    end interface
contains
    subroutine present()
        integer, dimension(NUMBER_OF_ELEMENTS)                     :: oneDimensional
        integer, dimension(NUMBER_OF_ELEMENTS, NUMBER_OF_ELEMENTS) :: twoDimensional

        ! rank   - the number of dimensions of an array.
        ! bounds - the upper and lower limits of the index in each dimension.
        ! extent - the number of elements along a dimension of an array.
        ! size   - the total number of elements in an array.
        ! shape  - value is determined by its rank and its extents in each dimension.

        ! conformable - two arrays are said to be conformable if they have the same shape,
        !               that is, they have the same rank and the same extent in each dimension.

        ! integer, dimension(-10:15):: current
        ! bounds -10 and 15 and an extent of 26.

        call getArray(oneDimensional)
        call getArray(twoDimensional)

        print *, ''
    end subroutine

    subroutine getArrayOneDimension(array)
        integer, dimension(:), intent(in) :: array

        integer, parameter :: FIRST = 1
        integer, parameter :: RANK  = 1

        integer, dimension(RANK) :: shape_
        integer, dimension(RANK) :: lbound_
        integer, dimension(RANK) :: ubound_
        integer :: size_

        real start

        call cpu_time(start)
        shape_  = shape(array)
        lbound_ = lbound(array)
        ubound_ = ubound(array)
        size_   = size(array)

        call report('Arrays', 'OneDimension', '', start)
        call assert_equals(shape_,  [NUMBER_OF_ELEMENTS])
        call assert_equals(lbound_, [FIRST])
        call assert_equals(ubound_, [NUMBER_OF_ELEMENTS])
        call assert_equals(size_,    NUMBER_OF_ELEMENTS)
    end subroutine

    subroutine getArrayTwoDimension(array)
        integer, dimension(:, :), intent(in) :: array

        integer, parameter :: FIRST = 1
        integer, parameter :: RANK  = 2

        integer, dimension(RANK) :: shape_
        integer, dimension(RANK) :: lbound_
        integer, dimension(RANK) :: ubound_
        integer :: size_

        real start

        call cpu_time(start)
        shape_  = shape(array)
        lbound_ = lbound(array)
        ubound_ = ubound(array)
        size_   = size(array)

        call report('Arrays', 'TwoDimension', '', start)
        call assert_equals(shape_,  [NUMBER_OF_ELEMENTS, NUMBER_OF_ELEMENTS])
        call assert_equals(lbound_, [FIRST, FIRST])
        call assert_equals(ubound_, [NUMBER_OF_ELEMENTS, NUMBER_OF_ELEMENTS])
        call assert_equals(size_,   NUMBER_OF_ELEMENTS * NUMBER_OF_ELEMENTS)
    end subroutine
end module
