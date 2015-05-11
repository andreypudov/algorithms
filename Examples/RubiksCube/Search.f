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

    use MArrays

    use MERubiksCubeCommon
    use MERubiksCubeCube
    use MERubiksCubeRotator

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

        integer, dimension(size(rotations)) :: rotations_copy
        integer, dimension(:), allocatable  :: buffer

        type(TArrays) arrays
        logical       status

        allocate(buffer(depth))
        rotations_copy = rotations

        ! sort the sequence of rotations
        call arrays%sort(rotations_copy)

        !print '(A)', 'Initial state: '
        !call cube%set(source)
        !call cube%print()

        call permutation(1, depth, source, destination, mask, rotations_copy, buffer)

        deallocate(buffer)
        status = SOLUTION_FOUND
    end function search

    !
    ! length - the length of sequence (N)
    ! number - the number of elements in sequence (K) / rotations size
    !
    recursive subroutine permutation(position, length, source, destination, mask, rotations, buffer)
        integer, dimension(:), intent(in out) :: buffer
        integer, dimension(:), intent(in) :: source
        integer, dimension(:), intent(in) :: destination
        logical, dimension(:), intent(in) :: mask
        integer, dimension(:), intent(in) :: rotations
        integer, intent(in) :: position
        integer, intent(in) :: length

        type(TECube)    cube
        type(TERotator) rotator
        integer :: index

        cube    = TECube()
        rotator = TERotator()

        call cube%set(source)

        if (position == length + 1) then
            do index = 1, length
                print '(\A3)', CUBE_ROTATIONS(rotations(buffer(index)))
                call rotator%rotate(cube, rotations(buffer(index)))
            end do

            print '(X)'
            return
        end if

        do index = 1, size(rotations)
            buffer(position) = index
            call permutation(position + 1, length, source, destination, mask, rotations, buffer)
        end do
    end subroutine
end module
