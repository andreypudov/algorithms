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

module MERubiksCubeSearch

    use MERubiksCubeCommon
    use MERubiksCubeCube
    use MERubiksCubeRotator

    use iso_fortran_env
    use omp_lib

    implicit none
    private

    type, public :: TESearch
    contains
        procedure, nopass :: search
    end type
contains
    !
    ! Searches for desired state of a cube and returns true if solution is
    ! found, and false otherwise.
    !
    ! source      - the source state of a cube.
    ! destination - the desired state of a cube.
    ! mask        - the mask to compare states.
    ! rotations   - the list of available rotations.
    ! depth       - the maximum number of steps.
    !
    ! status      - return true if solution is found, and false otherwise.
    !
    function search(source, destination, mask, rotations, depth) result(status)
        integer, dimension(:), intent(in) :: source
        integer, dimension(:), intent(in) :: destination
        logical, dimension(:), intent(in) :: mask
        integer, dimension(:), intent(in) :: rotations
        integer, intent(in)               :: depth

        integer, dimension(0:depth) :: buffer
        integer, dimension(1:depth) :: sequence

        type(TECube)    cube
        type(TERotator) rotator

        integer(kind=int64) expectation, count
        integer index, jndex
        logical masked
        logical status

        ! array dividing parameters
        integer nthreads, threadid
        integer start, end
        real    divider

        ! search results
        integer, dimension(:,:), allocatable :: subresults
        integer, dimension(:,:), allocatable :: results
        integer subcounter

        ! seach elapsed time
        real(kind=real64) :: time1, time2

        cube    = TECube()
        cube    = TECube()
        rotator = TERotator()

        masked = .not. all(mask == .true.)
        status = SOLUTION_NOT_FOUND

        print '(A)', 'Initial state: '
        call cube%set(source)
        call cube%print()
        print '(X)'

        print '(A)', 'Desired state: '
        call cube%set(destination)
        call cube%print()
        print '(X)'

        if (masked) then
            print '(A)', 'Comparison mask: '
            call cube%set(mask + 6)
            call cube%print()
            print '(X)'
        end if

        time1 = omp_get_wtime()

        !omp parallel private(cube, buffer, nthreads, threadid, start, end, divider, expectation, count) &
        !omp private(subresults, results, subcounter)

        nthreads = omp_get_num_threads()
        threadid = omp_get_thread_num()
        buffer   = 0
        start    = -1
        end      = -1

        allocate(subresults(16, depth))
        subcounter = 0

        expectation = size(rotations) ** depth / nthreads
        if (expectation == 0) then
            print '(A)', 'Search in progress... (status is unavailable)'
        end if

        ! iterate over possible rotations
        do while ((buffer(0) == 0) .and. (buffer(1) /= end))
            if (start == -1) then
                divider = size(rotations) / real(nthreads)
                start   = floor(divider * threadid + 1.0)
                end     = ceiling(start + divider - 1)

                buffer(1) = start - 1
            end if

            ! begin rotation sequence
            call cube%set(source)

            !print '(\A3)', CUBE_ROTATIONS(rotations(buffer(1:depth) + 1))
            do index = 1, depth
                call rotator%rotate(cube, rotations(buffer(index) + 1))

                ! validate current and destired states
                if (all(((cube%get() - destination) * mask) == 0)) then
                    !print '(A)', 'Desired pattern found.'
                    print '(\A3)', CUBE_ROTATIONS(rotations(buffer(1:depth) + 1))
                    print '(X)'
                    !jndex = rotator%optimize(buffer, rotations)
                    !print '(\A3)', CUBE_ROTATIONS(rotations(buffer(1:jndex - 1) + 1))
                    !print '(X)'
                    sequence = buffer(1:depth) + 1
                    jndex = rotator%optimize(sequence, rotations)
                    print '(\A3)', CUBE_ROTATIONS(rotations(sequence(1:jndex)))
                    print '(X/)'
                    !call insertResult(buffer(1:jndex - 1), subresults, subcounter)

                    status = SOLUTION_FOUND
                end if
            end do

            !$omp flush(status)
            if ((status == SOLUTION_FOUND) .and. (masked == .false.)) then
                buffer(1) = end
            end if

            count = count + 1
            !print '(X)'

            !$omp master
            if ((mod(count, 2500000) == 0) .and. (expectation /= 0)) then
                print '(\A,I3,A)', 'Search in progress... ', (count * 100 / expectation), '%'
                print '(\A)', achar(13)
            end if
            !$omp end master

            buffer(depth) = buffer(depth) + 1
            jndex = depth
            do while (buffer(jndex) == size(rotations))
                buffer(jndex) = 0
                jndex = jndex - 1
                buffer(jndex) = buffer(jndex) + 1
            end do
        end do

        !omp critical
        !omp end critical

        if (allocated(subresults)) then
            deallocate(subresults)
        end if

        !omp end parallel

        if (status == SOLUTION_NOT_FOUND) then
            print '(A)', 'Desired pattern doesn''t found.'
        end if

        time2 = omp_get_wtime()
        print '(X/,A,F6.3,A)', 'Elapsed time: ', time2 - time1, 's.'
    end function

    subroutine insertResult(value, subresults, subcounter)
        integer, dimension(:), intent(in)                    :: value
        integer, dimension(:,:), allocatable, intent(in out) :: subresults
        integer, intent(in out)                              :: subcounter

        integer, dimension(:,:), allocatable :: temporary_array

        subcounter = subcounter + 1
        if (subcounter > size(subresults)) then
            allocate(temporary_array(size(subresults, 1) * 3 / 2, size(subresults, 2)))
            temporary_array(1:size(subresults, 1), 1:size(subresults, 2)) = subresults
            deallocate(subresults)
            call move_alloc(temporary_array, subresults)
        end if

        !subresults(subcounter, 1:) = value
    end subroutine

    function containsResult(resvalue, subresults) result(status)
        integer, dimension(:), intent(in)                    :: resvalue
        integer, dimension(:,:), allocatable, intent(in out) :: subresults

        integer index
        logical status

        status = .false.
        do index = 1, size(subresults, 1)
            if (all(subresults(index, 1:) == resvalue)) then
                status = .true.
                exit
            end if
        end do
    end function
end module
