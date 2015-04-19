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

module MBreadthFirstSearch

    use MArrayQueue
    use MGraph
    use MVertex

    implicit none
    private

    type, public :: TBreadthFirstSearch
    contains
        procedure, nopass :: search
    end type
contains
    function search(graph) result(sequence)
        class(TGraph), intent(in out) :: graph

        logical, dimension(:,:), pointer     :: adjacencyMatrix
        type(TVertex), dimension(:), pointer :: vertexList
        integer, dimension(:), pointer       :: sequence
        type(TArrayQueue) :: queue

        integer vertexNumber
        integer vertex
        integer index

        adjacencyMatrix => graph%getAdjacencyMatrix()
        vertexList      => graph%getVertexlist()
        vertexNumber    =  graph%getVertexNumber()

        allocate(sequence(size(vertexList)))

        call vertexList(1)%setVisited(.true.)
        call queue%init()

        index = 1
        sequence(index) = vertexList(index)%getValue()
        call queue%push(vertexList(index)%getValue())

        do while (queue%empty() == .false.)
            vertex = getAdjacencyUnvisitedVertex(adjacencyMatrix, vertexList, vertexNumber, queue%peek())

            if (vertex == 0) then
                vertex = queue%pop()
            else
                call vertexList(vertex).setVisited(.true.)
                call queue%push(vertex)

                index = index + 1
                sequence(index) = vertex
            end if
        end do

        do index = 1, vertexNumber
            call vertexList(index).setVisited(.false.)
        end do

        call queue%destroy()
    end function

    function getAdjacencyUnvisitedVertex(adjacencyMatrix, vertexList, vertexNumber, vertex) result(value)
        logical, dimension(:,:)     :: adjacencyMatrix
        type(TVertex), dimension(:) :: vertexList
        integer, intent(in)         :: vertexNumber
        integer, intent(in)         :: vertex
        integer :: value

        integer index

        do index = 1, vertexNumber
            if ((adjacencyMatrix(vertex, index) == .true.) &
                    .and. (vertexList(index)%isVisited() == .false.)) then
                value = index
                return
            end if
        end do

        value = 0
    end function
end module
