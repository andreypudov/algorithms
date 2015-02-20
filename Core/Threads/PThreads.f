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

module MPThreads

    implicit none
    public

    !character(len=11 ,kind=c_char) :: digit_string = c_char_'0123456789'

    interface
        subroutine pthread_mutex_init(mutex) bind(C, name='_pthread_mutex_init')
            use, intrinsic :: iso_c_binding
            implicit none

            integer(c_int), intent(out) :: mutex
        end subroutine

        function pthread_create(id, procedure, argument) bind(C, name='_pthreads_create') result(status)
            use, intrinsic :: iso_c_binding
            implicit none

            integer(c_int), intent(out) :: id
            type(c_funptr), intent(in)  :: procedure
            type(c_ptr),    intent(in)  :: argument
            integer(c_int) :: status
        end function

        function pthreads_strlen(string) bind(C, name='strlen') result(length)
            use, intrinsic :: iso_c_binding
            implicit none

            character(kind=c_char), dimension(*), intent(in) :: string
            integer(c_int) :: length
        end function
    end interface
end module
