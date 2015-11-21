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

module MExAlg1p5e1

    use MIntrinsicRandom
    use MFileReader
    use MUReport

    implicit none
    private

    type, public :: TExAlg1p5e1
    contains
        procedure, nopass :: present
    end type
contains
    subroutine present()
        integer, dimension(:,:), allocatable :: list

        type(TFileReader) fileReader

        real start

        call fileReader%readAdjacencyWeightedList('Samples/dijkstraData_4_1', list)

        call cpu_time(start)
        call dijkstrasShortestPath(list)

        call report('Alg1p5e1', 'Dijkstra''s shortest-path', '', start)
        print '(A,I)', 'Shortest-path distances: ', 1

        deallocate(list)
    end subroutine

    recursive subroutine dijkstrasShortestPath(list)
        integer, dimension(:,:), allocatable, intent(in out) :: list

        integer index, jndex

        do index = 1, size(list, 1)
            do jndex = 1, size(list, 2)
                print '(I2\)', list(index, jndex)
            end do
            print *, ''
        end do

    end subroutine
end module
