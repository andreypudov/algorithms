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

module MRadixSort

    use MSort

    implicit none
    private

    type, extends(TSort), public :: TRadixSort
    contains
        procedure, nopass :: sort => sortExchange

        procedure, nopass :: sortExchange
    end type
contains
    subroutine sortExchange(array)
        integer, dimension(:), intent(in out) :: array

        call sortExchangeProc(array, 1, size(array), 30)
    end subroutine

    recursive subroutine sortExchangeProc(array, left, right, leftmost)
        integer, dimension(:), intent(in out) :: array
        integer, intent(in) :: left
        integer, intent(in) :: right
        integer, intent(in) :: leftmost

        integer index
        integer jndex
        integer buffer

        if ((right > left) .and. (leftmost >= 0)) then
            index = left
            jndex = right

            do while (jndex /= index)
                do while ((bits(array(index), leftmost, 1) == 0) .and. (index < jndex))
                    index = index + 1
                end do

                do while ((bits(array(jndex), leftmost, 1) /= 0) .and. (jndex > index))
                    jndex = jndex - 1
                end do

                buffer       = array(index)
                array(index) = array(jndex)
                array(jndex) = buffer
            end do

            if (bits(array(right), leftmost, 1) == 0) then
                jndex = jndex + 1
            end if

            call sortExchangeProc(array, left, jndex - 1, leftmost - 1)
            call sortExchangeProc(array, jndex, right, leftmost - 1)
        end if
    end subroutine

    function bits(number, right, count)
        integer, intent(in) :: number
        integer, intent(in) :: right
        integer, intent(in) :: count
        integer :: bits

        bits = ibits(number, right, count)
    end function
end module
