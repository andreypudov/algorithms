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

submodule (UFoundation) StringInitWithFString

    use Foundation

    use MUAsserts
    use MUReport

contains
    module subroutine presentStringInitWithFString()
        character(len=13) :: fstring1

        type(String), pointer :: string_dynamic
        type(String)          :: string_static

        real start

        call cpu_time(start)

        fstring1 = 'Hello, World!'

        allocate(string_dynamic)
        call string_dynamic%initWithFString(fstring1)
        call string_static%initWithFString(fstring1)

        call assert_equals(fstring1, string_dynamic%getFString())
        call assert_equals(fstring1, string_static%getFString())

        call string_dynamic%destroy()
        call string_static%destroy()

        deallocate(string_dynamic)

        call report('Foundation', 'String', 'IWFStr.', start)
    end subroutine
end submodule
