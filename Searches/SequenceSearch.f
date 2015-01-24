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

module MSequenceSearch

    use MSearch
    
    implicit none
    private

    type, extends(TSearch), public :: TSequenceSearch
    contains
        procedure :: search
    end type

contains
    function search(instance, array, key, begin, end) result(position)
        class(TSequenceSearch), intent(in)  :: instance
        integer, dimension(:), intent(in) :: array
        integer, intent(in)               :: key
        integer, optional, intent(in)     :: begin
        integer, optional, intent(in)     :: end
        integer :: position

        integer low
        integer high
        integer index
        
        if (present(begin) .and. present(end)) then
            low  = begin
            high = end
        else
            low  = 1
            high = size(array)
        end if

        ! sequence search

        do index = low, high
            if (array(index) .eq. key) then
                position = index
                return
            end if
        end do

        position = -1
    end function
end module
