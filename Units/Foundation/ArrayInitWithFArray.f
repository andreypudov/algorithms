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

submodule (UFoundation) ArrayInitWithFArray

    use Foundation

    use MUAsserts
    use MUReport

contains
    module subroutine presentArrayInitWithFArray()
        type(Array) arrayChar
        type(Array) arrayInt
        type(Array) arrayLog
        type(Array) arrayReal

        class(Object), pointer :: value

        character, dimension(6) :: charArray
        integer,   dimension(6) :: intArray
        logical,   dimension(6) :: logArray
        real,      dimension(6) :: realArray

        integer index
        real    start

        charArray = (/ 'a', 'b', 'c', 'd', 'e', 'f' /)
        intArray  = (/ 0, 1, 2, 3, 4, 5 /)
        logArray  = (/ .true., .false., .true., .false., .true., .false. /)
        realArray = (/ 0.1, 0.01, 0.001, 0.0001, 0.00001, 0.000001 /)

        call cpu_time(start)

        call arrayChar%initWithFArray(charArray)
        call arrayInt%initWithFArray(intArray)
        call arrayLog%initWithFArray(logArray)
        call arrayReal%initWithFArray(realArray)

        do index = 1, size(intArray)
            value => arrayChar%objectAtIndex(index)

            select type (value)
            class is (Number)
                call assert_equals(value%characterValue(), charArray(index))
            class default
                call assert_ok(.false., '{1}')
            end select
        end do

        do index = 1, size(intArray)
            value => arrayInt%objectAtIndex(index)

            select type (value)
            class is (Number)
                call assert_equals(value%integerValue(), intArray(index))
            class default
                call assert_ok(.false., '{2}')
            end select
        end do

        do index = 1, size(intArray)
            value => arrayLog%objectAtIndex(index)

            select type (value)
            class is (Number)
                call assert_equals(value%logicalValue(), logArray(index))
            class default
                call assert_ok(.false., '{3}')
            end select
        end do

        do index = 1, size(intArray)
            value => arrayReal%objectAtIndex(index)

            select type (value)
            class is (Number)
                call assert_equals(value%realValue(), realArray(index))
            class default
                call assert_ok(.false., '{4}')
            end select
        end do

        call arrayChar%destroy()
        call arrayInt%destroy()
        call arrayLog%destroy()
        call arrayReal%destroy()

        call report('Foundation', 'Array', 'InitWFA', start)
    end subroutine
end submodule
