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

module MExAlg1p1e1

    use MFileReader
    use MUReport

    implicit none
    private

    type, public :: TExAlg1p1e1
    contains
        procedure, nopass :: present
    end type
contains
    subroutine present()
        call sortSequential()
    end subroutine

    subroutine sortSequential()
        type(TFileReader) fileReader

        integer, dimension(:), allocatable :: array
        integer(kind = 8) :: count = 0
        integer value
        integer index
        integer jndex
        real    start

        call cpu_time(start)
        call fileReader%readListOfIntegers('Samples/IntegerArray.txt', array)
        do index = 1, size(array)
            do jndex = index + 1, size(array)
                if (array(jndex) < array(index)) then
                    value = array(jndex)
                    array(jndex) = array(index)
                    array(index) = value

                    count = count + 1
                end if
            end do
        end do

        call report('Alg1p1e1', 'Sequential', '', start)
        print '(A,I)', 'Number of reverses: ', count

        deallocate(array)
    end subroutine
end module
