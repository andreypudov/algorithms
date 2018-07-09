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
    !
    ! Initializes a newly allocated array.
    !
    module subroutine array_init(self)
        class(Array), intent(in out) :: self
    end subroutine

    !
    ! Releases allocated array.
    !
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

    !
    ! Initializes a newly allocated array by placing in it the objects contained in a given Fortran array.
    !
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

    !
    ! Initializes a newly allocated array by placing in it the objects contained in a given Fortran array.
    !
    module subroutine array_initWithFArray_complex(self, list)
        class(Array), intent(in out)      :: self
        complex, dimension(:), intent(in) :: list

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

    !
    ! Initializes a newly allocated array by placing in it the objects contained in a given Fortran array.
    !
    module subroutine array_initWithFArray_double(self, list)
        class(Array), intent(in out)               :: self
        double precision, dimension(:), intent(in) :: list

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

    !
    ! Initializes a newly allocated array by placing in it the objects contained in a given Fortran array.
    !
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

    !
    ! Initializes a newly allocated array by placing in it the objects contained in a given Fortran array.
    !
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

    !
    ! Initializes a newly allocated array by placing in it the objects contained in a given Fortran array.
    !
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

    !
    ! The number of objects in the array.
    !
    module function array_count(self) result(value)
        class(Array), intent(in) :: self
        integer                  :: value

        value = size(self%list)
    end function

    !
    ! Returns the object located at the specified index.
    !
    module function array_objectAtIndex(self, index) result(value)
        class(Array), intent(in) :: self
        integer, intent(in)      :: index
        class(Object), pointer   :: value

        if ((index < 1) .or. (index > size(self%list))) then
            ! TODO
        end if

        value => self%list(index)%link
    end function

    !
    ! The new array contains references to the receiving array’s elements, not copies of them.
    !
    module function array_sortedArrayUsingFunction(self, comparator) result(value)
        class(Array), intent(in) :: self
        interface
            function comparator(value1, value2) result(value)
                import Object

                class(Object), pointer, intent(in) :: value1
                class(Object), pointer, intent(in) :: value2
                integer                   :: value
            end function
        end interface
        class(Array), pointer :: value

        type(ObjectLink), dimension((size(self%list) + 1) / 2) :: buffer

        allocate(value)
        allocate(value%list, source = self%list)

        call mergeSort(value%list, buffer, size(value%list), comparator)
    end function

    recursive subroutine mergeSort(list, buffer, length, comparator)
        integer, intent(in)                                           :: length
        type(ObjectLink), dimension(length), intent(in out)           :: list
        type(ObjectLink), dimension((length + 1) / 2), intent(in out) :: buffer
        interface
            function comparator(value1, value2) result(order)
                import Object

                class(Object), pointer, intent(in) :: value1
                class(Object), pointer, intent(in) :: value2
                integer                            :: order
            end function
        end interface

        type(ObjectLink) link
        integer :: NA, NB

        select case (length)
        case (:1)
            return
        case (2)
            if (comparator(list(1)%link, list(2)%link) == ORDERED_DESCENDING) then
                link = list(1)
                list(1) = list(2)
                list(2) = link
            end if

            return
        case default
            NA = (length + 1) / 2
            NB = length - NA

            call mergeSort(list, buffer, NA, comparator)
            call mergeSort(list(NA + 1), buffer, NB, comparator)

            if (comparator(list(NA)%link, list(NA + 1)%link) == ORDERED_DESCENDING) then
                buffer(1:NA) = list(1:NA)
                call merge(buffer, NA, list(NA + 1), NB, list, length, comparator)
            endif
            return
        end select
    end subroutine

    subroutine merge(A, NA, B, NB, C, NC, comparator)
        integer, intent(in) :: NA,NB,NC           ! Normal usage: NA+NB = NC
        type(ObjectLink), intent(in out) :: A(NA) ! B overlays C(NA+1:NC)
        type(ObjectLink), intent(in)     :: B(NB)
        type(ObjectLink), intent(in out) :: C(NC)
        interface
            function comparator(value1, value2) result(order)
                import Object

                class(Object), pointer, intent(in) :: value1
                class(Object), pointer, intent(in) :: value2
                integer                            :: order
            end function
        end interface

        integer :: I,J,K
        integer order

        I = 1; J = 1; K = 1;
        do while(I <= NA .and. J <= NB)
            order = comparator(A(I)%link, B(J)%link)
            if ((order == ORDERED_ASCENDING) .or. (order == ORDERED_SAME)) then
                C(K) = A(I)
                I = I+1
            else
                C(K) = B(J)
                J = J+1
            endif

            K = K + 1
        enddo

        do while (I <= NA)
            C(K) = A(I)
            I = I + 1
            K = K + 1
        enddo
    end subroutine
end submodule
