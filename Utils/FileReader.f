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

module MFileReader

    use MArrays

    implicit none
    private

    integer, parameter :: DEFAULT_SIZE    = 16
    integer, parameter :: LINE_LENGTH_MAX = 16

    type, public :: TFileReader
    contains
        procedure, nopass :: readListOfIntegers
        procedure, nopass :: readAdjacencyList
    end type
contains
    subroutine readListOfIntegers(name, array)
        character(len=*), intent(in)                       :: name
        integer, dimension(:), allocatable, intent(in out) :: array
        integer, dimension(:), allocatable                 :: temporary_array

        type(TArrays) arrays

        integer unit
        integer status
        integer value
        integer length
        integer index

        allocate(array(DEFAULT_SIZE))
        unit   = 0
        status = 0
        length = 0

        open(unit = unit, file = name)

        do while (status == 0)
            read(unit, '(I)', iostat = status) value

            if (status  == 0) then
                length = length + 1
                call arrays%increaseWhenRequired(array, length)
                array(length) = value
            end if
        end do

        call arrays%resize(array, length)

        close(unit)
    end subroutine

    subroutine readAdjacencyList(name, array)
        character(len=*), intent(in)                         :: name
        integer, dimension(:,:), allocatable, intent(in out) :: array

        type(TArrays) arrays

        character(len=LINE_LENGTH_MAX) line

        integer unit
        integer status
        integer length
        integer index

        integer value
        integer begin
        integer end
        integer first
        integer beginning

        allocate(array(DEFAULT_SIZE, DEFAULT_SIZE))
        unit   = 0
        status = 0
        length = 0

        open(unit = unit, file = name)

        do while (status == 0)
            read(unit, '(A)', iostat = status) line

            if (status  == 0) then
                beginning = .true.

                index = 1
                do while (index <= len(line))
                    begin = index
                    do while (line(index:index) /= ' ')
                        end = index
                        index = index + 1
                    end do

                    if (begin <= end) then
                        read(line(begin:end), '(I)', iostat = status) value

                        if (beginning == .true.) then
                            beginning = .false.
                            first     = value
                        else
                            call arrays%increaseWhenRequired(array, max(first, value), max(first, value))
                            array(first, value) = 1
                        end if
                    end if

                    index = index + 1
                end do
            end if
        end do

        call arrays%resize(array, first, first)

        close(unit)
    end subroutine
end module
