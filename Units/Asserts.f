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

module MUAsserts

    use MArrays

    implicit none
    public

    interface assert_equals
        module procedure assert_equals_int, assert_equals_real, &
                assert_equals_int_arrays1d, assert_equals_int_arrays2d
    end interface
contains
    subroutine assert_equals_int(value1, value2)
        integer, intent(in) :: value1
        integer, intent(in) :: value2

        if (value1 .ne. value2) then
            print '(t1, a)', 'FAILED. The keys are set to different values.'
            print *, value1, value2
        end if
    end subroutine

    subroutine assert_equals_real(value1, value2)
        real, intent(in) :: value1
        real, intent(in) :: value2

        real, parameter :: EPSILON = 0.000001

        if (abs(value1 - value2) > EPSILON) then
            print '(t1, a)', 'FAILED. The keys are set to different values.'
            print *, value1, value2
        end if
    end subroutine

    subroutine assert_equals_int_arrays1d(value1, value2)
        integer, dimension(:), intent(in) :: value1
        integer, dimension(:), intent(in) :: value2

        if (all(value1 .eq. value2 ) .ne. .true. ) then
            print '(t1, a)', 'FAILED. The keys are set to different values.'
            print *, value1, value2
        end if
    end subroutine

    subroutine assert_equals_int_arrays2d(value1, value2)
        integer, dimension(:,:), intent(in) :: value1
        integer, dimension(:,:), intent(in) :: value2

        if (all(value1 .eq. value2 ) .ne. .true. ) then
            print '(t1, a)', 'FAILED. The keys are set to different values.'
            print *, value1, value2
        end if
    end subroutine

    subroutine assert_sorted(array)
        integer, dimension(:), intent(in) :: array
        type(TArrays) :: arrays

        if (arrays%isSorted(array) .ne. .true.) then
            print '(t1, a)', 'FAILED. The sequence does not sorted properly.'
            print *, array
        end if
    end subroutine
end module
