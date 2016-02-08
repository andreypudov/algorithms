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

submodule (UFoundation) VariableArgumentList

    use Foundation

    use MUAsserts
    use MUReport

contains
    module subroutine presentVariableArgumentList()
        type(Number) :: number1
        type(Number) :: number2
        type(Number) :: number3

        real start

        call cpu_time(start)

        number1 = 2
        number2 = 3
        number3 = 5

        call square(2, number1, number2, number3)

        call report('Foundation', 'VAList', '', start)
    end subroutine

    subroutine square(base, &
             o1,  o2,  o3,  o4,  o5,  o6,  o7,  o8,  o9,  o10, o11, o12, o13, o14, o15, o16, &
            o17, o18, o19, o20, o21, o22, o23, o24, o25, o26, o27, o28, o29, o30, o31, o32)
        integer, intent(in) :: base
        class(Object), optional, intent(in out) :: &
            o1,  o2,  o3,  o4,  o5,  o6,  o7,  o8,  o9,  o10, o11, o12, o13, o14, o15, o16, &
            o17, o18, o19, o20, o21, o22, o23, o24, o25, o26, o27, o28, o29, o30, o31, o32

        type(VariableArgumentList) :: list

        call list%initWithObjects( &
             o1,  o2,  o3,  o4,  o5,  o6,  o7,  o8,  o9,  o10, o11, o12, o13, o14, o15, o16, &
            o17, o18, o19, o20, o21, o22, o23, o24, o25, o26, o27, o28, o29, o30, o31, o32)
    end subroutine
end submodule
