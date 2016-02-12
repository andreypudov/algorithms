!
! The Laboratory of Algorithms
!
! The MIT License
!
! Copyright 2011-2016 Andrey Pudov.
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

submodule (Foundation) Array
contains
    module subroutine array_init(self)
        class(Array), intent(in out) :: self
    end subroutine

    module subroutine array_destroy(self)
        class(Array), intent(in out) :: self

        integer index

        if (self%selfAllocated) then
            do index = 1, size(self%list)
                deallocate(self%list(index)%link)
            end do
        end if

        deallocate(self%list)
    end subroutine

    module subroutine array_initWithFArray_character(self, list)
        class(Array), intent(in out)        :: self
        character, dimension(:), intent(in) :: list

        class(Number), pointer :: buffer
        integer index

        allocate(self%list(size(list)))
        self%selfAllocated = .true.

        do index = 1, size(list)
            allocate(buffer)

            buffer = list(index)
            self%list(index)%link => buffer
        end do
    end subroutine

    module subroutine array_initWithFArray_integer(self, list)
        class(Array), intent(in out)      :: self
        integer, dimension(:), intent(in) :: list

        class(Number), pointer :: buffer
        integer index

        allocate(self%list(size(list)))
        self%selfAllocated = .true.

        do index = 1, size(list)
            allocate(buffer)

            buffer = list(index)
            self%list(index)%link => buffer
        end do
    end subroutine

    module subroutine array_initWithFArray_logical(self, list)
        class(Array), intent(in out)      :: self
        logical, dimension(:), intent(in) :: list

        class(Number), pointer :: buffer
        integer index

        allocate(self%list(size(list)))
        self%selfAllocated = .true.

        do index = 1, size(list)
            allocate(buffer)

            buffer = list(index)
            self%list(index)%link => buffer
        end do
    end subroutine

    module subroutine array_initWithFArray_real(self, list)
        class(Array), intent(in out)   :: self
        real, dimension(:), intent(in) :: list

        class(Number), pointer :: buffer
        integer index

        allocate(self%list(size(list)))
        self%selfAllocated = .true.

        do index = 1, size(list)
            allocate(buffer)

            buffer = list(index)
            self%list(index)%link => buffer
        end do
    end subroutine

    module function array_count(self) result(value)
        class(Array), intent(in) :: self
        integer                  :: value

        value = size(self%list)
    end function

    module function array_objectAtIndex(self, index) result(value)
        class(Array), intent(in) :: self
        integer, intent(in)      :: index
        class(Object), pointer   :: value

        if ((index < 1) .or. (index > size(self%list))) then
            ! TODO
        end if

        value => self%list(index)%link
    end function

    module function array_sortedArrayUsingFunction(self) result(value)
        class(Array), intent(in) :: self
        class(Array), pointer    :: value

        allocate(value)
    end function
end submodule
