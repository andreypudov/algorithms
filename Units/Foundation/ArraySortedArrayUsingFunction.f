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

submodule (UFoundation) ArraySortedArrayUsingFunction

    use Foundation

    use MUAsserts
    use MUReport

contains
    module subroutine presentArraySortedArrayUsingFunction()
        class(Object), pointer :: value
        integer, dimension(6)  :: intArray

        class(Array), pointer :: arrayIntSorted
        type(Array) arrayInt

        integer index
        real    start

        intArray  = (/ 5, 4, 3, 2, 1, 0 /)

        call cpu_time(start)

        call arrayInt%initWithFArray(intArray)
        arrayIntSorted => arrayInt%sortedArrayUsingFunction(comparator)

        call assert_equals(arrayIntSorted%count(), size(intArray))
        do index = size(intArray), 1, -1
            value => arrayIntSorted%objectAtIndex(size(intArray) - index + 1)

            select type (value)
            class is (Number)
                call assert_equals(value%integerValue(), intArray(index))
            class default
                call assert_ok(.false., '{1}')
            end select
        end do

        call arrayInt%destroy()
        call arrayIntSorted%destroy()

        deallocate(arrayIntSorted)

        call report('Foundation', 'Array', 'SortedUF', start)
    end subroutine

    function comparator(value1, value2) result(order)
        type(ObjectLink), intent(in) :: value1
        type(ObjectLink), intent(in) :: value2
        integer                      :: order

        order = 1
    end function
end submodule
