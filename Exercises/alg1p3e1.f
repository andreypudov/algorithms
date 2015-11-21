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

module MExAlg1p3e1

    use MIntrinsicRandom
    use MFileReader
    use MUReport

    implicit none
    private

    type, public :: TExAlg1p3e1
    contains
        procedure, nopass :: present
    end type
contains
    subroutine present()
        integer, dimension(:,:), allocatable :: list

        type(TFileReader) fileReader

        real start

        call fileReader%readAdjacencyList('Samples/kargerMinCut_8_1', list)

        call cpu_time(start)
        call randomizedContraction(list)

        call report('Alg1p3e1', 'Randomized Contraction', '', start)
        print '(A,I)', 'Min cut: ', 1

        deallocate(list)
    end subroutine

    recursive subroutine randomizedContraction(list)
        integer, dimension(:,:), allocatable, intent(in out) :: list

        type(TIntrinsicRandom) random

        integer index
        real value

        do index = 1, 24
            print *, random%random(1, 6)
        end do

    end subroutine
end module
