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

module MDoublyLinkedList

    implicit none
    private

    ! linked list container contains pointers to the first and the last elements
    type LinkedList
        type (ListEntry), pointer :: first => null()
        type (ListEntry), pointer :: last  => null()
    end type

    ! linked list entry contains pointers to the next and the previous elements
    type ListEntry
        type (ListEntry), pointer :: back => null()
        type (ListEntry), pointer :: next => null()

        integer :: value
    end type

    contains
        subroutine struct()
            type (LinkedList), pointer :: list
            type (ListEntry),  pointer :: listEntry

            integer :: index

            list => create()

            write (*, '(A)') 'Structure 1. Linked List.'

            write (*, '(A)') 'Creating linked list with ten millions entries...'
            ! fill linked list by sequence of natural numbers
            do index = 0, 9999999
                call add(list, index)
            end do

            listEntry => list%last
            write(*, '(A, I)') 'Element with the last value (fast): ', listEntry%value
            listEntry => get(list, 9999999)
            write(*, '(A, I)') 'Element with the last value (slow): ', listEntry%value
        end subroutine

        ! creates linked list container structure
        function create()
            type (LinkedList), pointer :: create
            type (ListEntry),  pointer :: listEntry

            allocate(create)
            allocate(listEntry)

            listEntry%back  => null()
            listEntry%next  => null()
            listEntry%value = 0

            create%first => listEntry
            create%last  => listEntry
        end function

        ! adds new entry to the end of linked list
        subroutine add(list, value)
            type (LinkedList), pointer :: list
            type (ListEntry),  pointer :: lastEntry
            type (ListEntry),  pointer :: listEntry

            integer :: value

            lastEntry => list%last

            allocate(listEntry)
            listEntry%back  => lastEntry
            listEntry%next  => null()
            listEntry%value =  value

            lastEntry%next => listEntry
            list%last      => listEntry
        end subroutine

        ! Gets element with specified value
        function get(list, value)
            type (LinkedList), pointer :: list
            type (ListEntry),  pointer :: get

            integer :: value

            get => list%first
            do while (associated(get))
                if (get%value .eq. value) then
                    return
                end if

                get => get%next
            end do

            get => null()
        end function
end module
