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

! Smallest multiple
!
! 2520 is the smallest number that can be divided by each of the numbers
! from 1 to 10 without any remainder.
!
! What is the smallest positive number that is evenly divisible by all of
! the numbers from 1 to 20?
module MPEProblem5

    implicit none
    private

    type, public :: TPEProblem5
    contains
        procedure, nopass :: present
    end type
contains
    subroutine present
        write (*, '(A)') 'Problem 5. Smallest multiple.'

        write (*, '(A, I)') 'Multiple 1: ', multiple1()
        write (*, '(A, I)') 'Multiple 2: ', multiple2()
    end subroutine

    function multiple1()
        integer multiple1
        integer base
        logical flag

        do multiple1 = 1, huge(multiple1)
            flag = .true.

            do base = 1, 20
                if (mod(multiple1, base) .ne. 0) then
                    flag = .false.
                end if
            end do

            if (flag .eq. .true.) then
                return
            end if
        end do
    end function

    function multiple2()
        integer multiple2
        integer base
        logical flag

        do multiple2 = 1, huge(multiple2)
            flag = .true.

            do base = 2, 20
                if (mod(multiple2, base) .ne. 0) then
                    flag = .false.
                    exit
                end if
            end do

            if (flag .eq. .true.) then
                return
            end if
        end do
    end function
end module
