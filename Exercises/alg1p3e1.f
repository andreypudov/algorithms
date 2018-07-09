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

    integer, parameter :: NUMBER_OF_EXECUTIONS = 128

    type, public :: TExAlg1p3e1
    contains
        procedure, nopass :: present
    end type
contains
    subroutine present()
        integer, dimension(:,:), allocatable :: list
        integer, dimension(:), allocatable   :: sequence
        integer, dimension(:), allocatable   :: buffer

        type(TFileReader) fileReader

        integer index
        integer count
        integer minimum
        real    start

        call fileReader%readAdjacencyList('Samples/kargerMinCut', list)
        allocate(sequence(size(list)))
        allocate(buffer(size(list)))

        call cpu_time(start)

        minimum  = huge(minimum)
        do index = 1, NUMBER_OF_EXECUTIONS
            buffer = 0
            count  = randomizedContraction(list, buffer, .false.)
            if (count < minimum) then
                sequence = buffer
                minimum  = count

                print '(A,I)', 'Found minimum: ', minimum
            end if
        end do

        call report('Alg1p3e1', 'Randomized Contraction', '', start)

        minimum = randomizedContraction(list, sequence, .true.)
        print '(A,I)', 'Minimum number of cuts: ', minimum

        deallocate(list)
        deallocate(sequence)
        deallocate(buffer)
    end subroutine

    function randomizedContraction(original, sequence, defined) result(value)
        integer, dimension(:,:), allocatable, intent(in)   :: original
        integer, dimension(:), allocatable, intent(in out) :: sequence
        logical, intent(in) :: defined

        type(TArrays) arrays

        integer, dimension(:,:), allocatable :: list
        integer, dimension(:,:), allocatable :: count
        logical, dimension(:,:), allocatable :: mask
        integer vertex1, vertex2
        integer index
        integer value

        allocate(list(size(original, 1),  size(original, 2)))
        allocate(count(size(original, 1), size(original, 2)))
        allocate(mask(size(original, 1),  size(original, 2)))

        list       = original
        count      = 0
        count(:,1) = (/ (index, index = 1, size(count, 1)) /)

        do while (size(list, 1) > 2)
            call selectRandomEdge(list, vertex1, vertex2, sequence, defined)

            mask            = .true.
            mask(vertex2,:) = .false.
            mask(:,vertex2) = .false.

            ! add connection from second vertex to the first one
            list(vertex1,:) = list(vertex1,:) + list(vertex2,:)
            list(:,vertex1) = list(:,vertex1) + list(:,vertex2)

            ! remove self-loops
            forall (index = 1:size(list, 1))
                list(index, index) = 0
            end forall

            ! remove vertex from array and mask
            list = reshape(pack(list, mask), (/ size(list, 1) - 1, size(list, 2) - 1 /))
            call arrays%resize(list, size(list, 1) - 1, size(list, 2) - 1)
            deallocate(mask)
            allocate(mask(size(list, 1), size(list, 2)))

            call countEdges(list, count, vertex1, vertex2)
        end do

        value = countCuts(original, count, defined)

        deallocate(list)
        deallocate(count)
        deallocate(mask)
    end function

    subroutine selectRandomEdge(list, vertex1, vertex2, sequence, defined)
        integer, dimension(:,:), allocatable, intent(in)   :: list
        integer, dimension(:), allocatable, intent(in out) :: sequence
        integer, intent(out) :: vertex1
        integer, intent(out) :: vertex2
        logical, intent(in)  :: defined

        type(TIntrinsicRandom), save :: random
        integer, save :: position = 1
        integer index
        integer left, right

        vertex1 = random%random(1, size(list, 1))
        vertex2 = random%random(1, size(list, 2))

        left    = -1
        right   = -1

        ! select closest connection to randomly selected vertex
        do index = (vertex2), 1, -1
            if (list(vertex1, index) /= 0) then
                left = index
                exit
            end if
        end do

        do index = (vertex2), size(list, 2)
            if (list(vertex1, index) /= 0) then
                right = index
                exit
            end if
        end do

        if ((((vertex2 - left) < (right - vertex2)) .and. left /= -1) .or. right == -1) then
            vertex2 = left
        else
            vertex2 = right
        end if

        if (defined == .false.) then
            do index = 1, size(sequence)
                if (sequence(index) == 0) then
                    sequence(index)     = vertex1
                    sequence(index + 1) = vertex2
                    position = 1

                    exit
                end if
            end do
        end if

        if (defined == .true.) then
            vertex1  = sequence(position)
            vertex2  = sequence(position + 1)
            position = position + 2
        end if
    end subroutine

    subroutine countEdges(list, count, vertex1, vertex2)
        integer, dimension(:,:), allocatable, intent(in out) :: list
        integer, dimension(:,:), allocatable, intent(in out) :: count
        integer, intent(in) :: vertex1
        integer, intent(in) :: vertex2

        type(TArrays) arrays
        logical, dimension(:,:), allocatable :: mask
        integer index, jndex, kndex

        allocate(mask(size(count, 1), size(count, 2)))
        mask            = .true.
        mask(vertex2,:) = .false.

        do index = 1, size(count, 2)
            if (count(vertex1, index) == 0) then
                count(vertex1:vertex1, index:size(count, 2)) = &
                        count(vertex2:vertex2, 1:(size(count, 2) - index + 1))
                exit
            end if
        end do
        count = reshape(pack(count, mask), (/ size(count, 1) - 1, size(count, 2) /))
        call arrays%resize(count, size(count, 1) - 1, size(count, 2))

        deallocate(mask)
    end subroutine

    function countCuts(list, count, defined) result(value)
        integer, dimension(:,:), allocatable, intent(in) :: list
        integer, dimension(:,:), allocatable, intent(in) :: count
        logical, intent(in) :: defined

        integer index, jndex
        integer vertex1, vertex2
        integer value

        value = 0

        do index = 1, size(count, 2)
            vertex1 = count(1, index)
            if (vertex1 == 0) then
                exit
            end if

            do jndex = 1, size(count, 2)
                vertex2 = count(2, jndex)
                if (vertex2 == 0) then
                    exit
                end if

                if (list(vertex1, vertex2) /= 0) then
                    if (defined) then
                        print '(A,I,I,A)', 'Cut: [', vertex1, vertex2, ']'
                    end if

                    value = value + 1
                end if
            end do
        end do
    end function
end module
