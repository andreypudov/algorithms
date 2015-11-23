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

    use MArrays
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
        type(TArrays) arrays

        logical, dimension(:,:), allocatable :: mask
        integer vertex
        integer n, i, j

        allocate(mask(size(list, 1), size(list, 2)))
        n = size(list, 1)

        print '(8I2)', list
        do while (n > 2)
            vertex = random%random(1, n)
            vertex = 3

            print *, vertex
            mask           = .true.
            mask(vertex,:) = .false.

            !list = reshape(pack(list, mask), (/ n - 1, n /))
            n = n - 1

            list(8, 4) = 8
            print '(8L2)', transpose(mask)
            print '(8I2)', transpose(list)
            print *, mask(vertex, 1), mask(vertex, 4), mask(vertex, 7)
            call arrays%print(list)
        end do

        deallocate(mask)
    end subroutine
end module
