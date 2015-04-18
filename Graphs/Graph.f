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

module MGraph

    use MArrays
    use MVertex

    implicit none
    private

    integer, parameter :: DEFAULT_NUMBER_OF_VERTEXES = 16

    type, public :: TGraph
        private
        logical, dimension(:,:), pointer     :: adjacencyMatrix
        type(TVertex), dimension(:), pointer :: vertexList

        integer  :: vertexesNumber
    contains
        procedure :: addVertex
        procedure :: addEdge

        procedure :: getAdjacencyMatrix
        procedure :: getVertexList
        procedure :: getVertexNumber

        procedure :: init
        procedure :: destroy
    end type
contains
    subroutine addVertex(instance, value)
        class(TGraph), intent(in out) :: instance
        integer, intent(in)           :: value

        type(TArrays) arrays
        type(TVertex) vertex

        type(TVertex), dimension(:), pointer :: temporary_array1d
        logical, dimension(:,:), pointer     :: temporary_array2d
        integer length
        integer index

        call vertex%init(value, .false.)
        instance%vertexesNumber = instance%vertexesNumber + 1

        ! increase array size when required
        if (instance%vertexesNumber > size(instance%vertexList)) then
            ! increase vertex list
            length = size(instance%vertexList) * 3 / 2
            allocate(temporary_array1d(length))
            temporary_array1d(1:size(instance%vertexList)) = instance%vertexList
            deallocate(instance%vertexList)
            instance%vertexList => temporary_array1d

            ! increase adjacency matrix
            length = size(instance%vertexList)
            allocate(temporary_array2d(length, length))
            temporary_array2d(1:size(instance%adjacencyMatrix, 1), &
                    1:size(instance%adjacencyMatrix, 2)) = instance%adjacencyMatrix
            deallocate(instance%adjacencyMatrix)
            instance%adjacencyMatrix => temporary_array2d
        end if

        instance%vertexList(instance%vertexesNumber) = vertex
    end subroutine

    subroutine addEdge(instance, start, end)
        class(TGraph), intent(in out) :: instance
        integer, intent(in)           :: start
        integer, intent(in)           :: end

        instance%adjacencyMatrix(start, end) = .true.
        instance%adjacencyMatrix(end, start) = .true.
    end subroutine

    function getAdjacencyMatrix(instance) result(value)
        class(TGraph), intent(in)        :: instance
        logical, dimension(:,:), pointer :: value

        value => instance%adjacencyMatrix
    end function

    function getVertexList(instance) result(value)
        class(TGraph), intent(in)            :: instance
        type(TVertex), dimension(:), pointer :: value

        value => instance%vertexList
    end function

    function getVertexNumber(instance) result(value)
        class(TGraph), intent(in) :: instance
        integer :: value

        value = instance%vertexesNumber
    end function

    subroutine init(instance)
        class(TGraph), intent(in out) :: instance

        integer index
        integer jndex

        allocate(instance%adjacencyMatrix(DEFAULT_NUMBER_OF_VERTEXES, DEFAULT_NUMBER_OF_VERTEXES))
        allocate(instance%vertexList(DEFAULT_NUMBER_OF_VERTEXES))

        instance%vertexesNumber  = 0

        do index = 1, size(instance%adjacencyMatrix, 1)
            do jndex = 1, size(instance%adjacencyMatrix, 2)
                instance%adjacencyMatrix(index, jndex) = 0
            end do
        end do
    end subroutine

    subroutine destroy(instance)
        class(TGraph), intent(in out) :: instance

        deallocate(instance%adjacencyMatrix)
        deallocate(instance%vertexList)

        instance%vertexesNumber = 0
    end subroutine
end module
