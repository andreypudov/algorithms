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

        integer, dimension(:), allocatable  :: buffer

        type(TECube)    cube
        type(TERotator) rotator

        integer       count
        integer       index
        integer       jndex
        logical       status

        cube    = TECube()
        rotator = TERotator()

        allocate(buffer(depth))

        print '(A)', 'Initial state: '
        call cube%set(source)
        call cube%print()

        buffer = 0
        count  = 0

        ! iterate over possible rotations
        do while (buffer(0) == 0)
            do index = 1, depth
                !print '(\I3)', buffer(index)
                print '(\A3)', CUBE_ROTATIONS(rotations(buffer(index) + 1))
                call rotator%rotate(cube, rotations(buffer(index) + 1))
            end do

            count = count + 1
            print '(X)'

            buffer(depth) = buffer(depth) + 1
            jndex = depth
            do while (buffer(jndex) == size(rotations))
                buffer(jndex) = 0
                jndex = jndex - 1
                buffer(jndex) = buffer(jndex) + 1
            end do
        end do

        print *, count

        deallocate(buffer)
        status = SOLUTION_FOUND
    end function
end module
