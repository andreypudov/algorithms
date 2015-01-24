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

module MInsertionSort

    use MSort
    use MBinarySearch
      
    implicit none
    private

    type, extends(TSort), public :: TInsertionSort
    contains
        procedure :: sort => sortBinary
        
        procedure :: sortOriginal
        procedure :: sortBinary
    end type

contains    
    subroutine sortOriginal(instance, array)
        class(TInsertionSort), intent(in)     :: instance
        integer, dimension(:), intent(in out) :: array

        integer index
        integer location
        integer key

        do index = 2, size(array)
            key      = array(index)
            location = index - 1
             
            do while ((location > 0) .and. (array(location) > key))
                array(location + 1) = array(location)
                location = location - 1
            end do

            array(location + 1) = key
        end do
    end subroutine

    subroutine sortBinary(instance, array)
        class(TInsertionSort), intent(in)     :: instance
        integer, dimension(:), intent(in out) :: array

        type(TBinarySearch) :: BinarySearch
        
        integer index
        integer jndex
        integer location
        integer key
        
        do index = 2, size(array)
            key      = array(index)
            location = BinarySearch%search(array, key, 1, index)

            if (location < 1) then
                location = -location
            end if

            !array(location + 1:index) = array(location:index - 1)
            
            do jndex = index - 1, location, -1
                array(jndex + 1) = array(jndex)
            end do
            array(location) = key
        end do
    end subroutine
end module
