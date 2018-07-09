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

module MUDepthFirstSearch

    use MDepthFirstSearch
    use MGraph
    use MUAsserts
    use MUReport

    implicit none
    private

    type, public :: TUDepthFirstSearch
    contains
        procedure, nopass :: present
    end type
contains
    subroutine present()
        type(TDepthFirstSearch) dfs
        type(TGraph)            graph

        integer, dimension(:), pointer :: sequence
        real :: start

        call graph%init()

        call graph%addVertex(1)
        call graph%addVertex(2)
        call graph%addVertex(3)
        call graph%addVertex(4)
        call graph%addVertex(5)
        call graph%addVertex(6)


        !   2 - 3 - 6
        !  /
        ! 1
        !  \
        !   4 - 5

        ! expected to have 1 - 2 - 3 - 6 - 4 - 5

        call graph%addEdge(1, 2)
        call graph%addEdge(2, 3)
        call graph%addEdge(3, 6)
        call graph%addEdge(1, 4)
        call graph%addEdge(4, 5)

        call cpu_time(start)

        sequence => dfs%search(graph)

        call report('Graph', 'DepthFirstSearch', '', start)
        call assert_equals(sequence(1:6), (/ 1, 2, 3, 6, 4, 5 /))

        call graph%destroy()
        deallocate(sequence)

        print *, ''
    end subroutine
end module
