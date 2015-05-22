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

module MEXArraysRubiksCube

    use MUAsserts
    use MUReport

    implicit none
    private

    type, public :: RubiksCube
    contains
        procedure, nopass :: present
    end type
contains
    subroutine present()
        integer, dimension(8)  :: array1 = [1, 1, -1, 2, 3, 4, 5, 6]
        integer, dimension(8)  :: array2 = [1, 2, 3, 4, 5, 6, 7, -7]
        integer, dimension(8)  :: array3 = [2, -2, 1, 2, 3, 4, 5, 6]
        integer, dimension(10) :: array4 = [1, 2, -2, 2, 3, -3, 3, 4, 5, 6]

        integer, dimension(10) :: array5 = [2, 2, 2, 2, 1, 2, 3, 4, 5, 6]
        integer, dimension(10) :: array6 = [1, 2, 2, 2, 2, 2, 3, 4, 5, 6]
        integer, dimension(10) :: array7 = [1, 2, 3, 4, 5, 6, 7, 7, 7, 7]
        integer, dimension(14) :: array8 = [1, 2, 3, 4, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7]

        integer, dimension(16) :: array9  = [1, 1, 2, 2, 2, 2, 2, -2, -2, -2, -2, -2, 1, 1, 1, 1]
        integer, dimension(8)  :: array10 = [1, 1, 2, -2, 1, 1, 1, 1]
        integer, dimension(7)  :: array11 = [1, 1, 2, -2, 1, 1, 1]
        integer, dimension(7)  :: array12 = [1, 1, 1, 1, 1, 1, -1]
        integer, dimension(7)  :: array13 = [-3, 3, 3, 3, 3, 3, 1]
        integer, dimension(7)  :: array14 = [-3, 3, 3, 3, 3, -3, 3]
        integer, dimension(4)  :: array15 = [3, 3, 3, 3]
        integer, dimension(7)  :: array16 = [1, 1, 1, 1, 1, 1, -1]

        integer, dimension(6) :: result1 = [1, 2, 3, 4, 5, 6]
        integer, dimension(2) :: result2 = [1, 1]
        integer, dimension(1) :: result3 = [1]
        integer, dimension(3) :: result4 = [3, 3, 3]
        integer, dimension(0) :: result5

        integer index
        real    start

        call cpu_time(start)

        index = optimize(array1)
        call assert_equals(array1(1:index), result1)
        index = optimize(array2)
        call assert_equals(array2(1:index), result1)
        index = optimize(array3)
        call assert_equals(array3(1:index), result1)
        index = optimize(array4)
        call assert_equals(array4(1:index), result1)

        index = optimize(array5)
        call assert_equals(array5(1:index), result1)
        index = optimize(array6)
        call assert_equals(array6(1:index), result1)
        index = optimize(array7)
        call assert_equals(array7(1:index), result1)
        index = optimize(array8)
        call assert_equals(array8(1:index), result1)

        index = optimize(array9)
        call assert_equals(array9(1:index), result2)
        index = optimize(array10)
        call assert_equals(array10(1:index), result2)
        index = optimize(array11)
        call assert_equals(array11(1:index), result3)
        index = optimize(array12)
        call assert_equals(array12(1:index), result3)
        index = optimize(array13)
        call assert_equals(array13(1:index), result3)
        index = optimize(array14)
        call assert_equals(array14(1:index), result4)
        index = optimize(array15)
        call assert_equals(array15(1:index), result5)
        index = optimize(array16)
        call assert_equals(array16(1:index), result3)

        call report('Arrays', 'Pairs', '', start)
    end subroutine

    recursive function optimize(array) result(jndex)
        integer, dimension(:), intent(in out) :: array
        integer index, jndex
        integer count, value
        logical state

        index = 1
        jndex = 1
        count = 1
        state = .false.
        value = array(index)
        do while (index < size(array))
            index = index + 1
            jndex = jndex + 1

            if (array(index - 1) == array(index) * -1) then
                index = index + 1
                jndex = jndex - 1
                count = count - 1
                state = .true.

                ! last pair is reversed
                if (index > size(array)) then
                    jndex = jndex - 1
                    exit
                end if
            end if

            if (array(index) == value) then
                count = count + 1
                if (count == 4) then
                    index = index + 1
                    jndex = jndex - 3
                    count = 1
                    state = .true.
                    value = array(index)

                    if (index > size(array)) then
                        jndex = jndex - 1
                        exit
                    end if
                end if
            else
                count = 1
                value = array(index)
            end if

            array(jndex) = array(index)
        end do

        ! repeat optimization until last modification
        if (state .and. (jndex > 0)) then
            jndex = optimize(array(1:jndex))
        end if
    end function
end module
