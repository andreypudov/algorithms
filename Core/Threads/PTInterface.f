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

module MPTInterface

    implicit none
    public

    abstract interface
        function IRunnableRoutine(argument) bind(c) result(routine)
            use iso_c_binding
            type(c_ptr)                     :: routine
            type(c_ptr), value, intent(in)  :: argument
        end function
    end interface

    interface
        subroutine thread_init(info) bind(c)
            use iso_c_binding
            integer(c_int), intent(out) :: info
        end subroutine

        subroutine thread_alloc(thread_id, info) bind(c)
            use iso_c_binding
            integer(c_int), intent(out) :: thread_id
            integer(c_int), intent(out) :: info
        end subroutine

        subroutine thread_create(thread_id, attribute_id, routine, argument, info) bind(c)
            use iso_c_binding
            integer(c_int),     intent(in)  :: thread_id
            integer(c_int),     intent(in)  :: attribute_id
            type(c_funptr),     intent(in)  :: routine
            type(c_ptr), value, intent(in)  :: argument
            integer(c_int),     intent(out) :: info
        end subroutine

        subroutine thread_join(thread_id, value_pointer, info) bind(c)
            use iso_c_binding
            integer(c_int), intent(in)  :: thread_id
            type(c_ptr),    intent(out) :: value_pointer
            integer(c_int), intent(out) :: info
        end subroutine

        subroutine thread_exit(value_pointer) bind(c)
            use iso_c_binding
            type(c_ptr), intent(in) :: value_pointer
        end subroutine

        subroutine thread_destroy(info) bind(c)
            use iso_c_binding
            integer(c_int), intent(out) :: info
        end subroutine

        subroutine thread_mutex_init(mutex_id, attribute_id, info) bind(c)
            use iso_c_binding
            integer(c_int), intent(out) :: mutex_id
            integer(c_int), intent(in)  :: attribute_id
            integer(c_int), intent(out) :: info
        end subroutine

        subroutine thread_mutex_lock(mutex_id, info) bind(c)
            use iso_c_binding
            integer(c_int), intent(in)  :: mutex_id
            integer(c_int), intent(out) :: info
        end subroutine

        subroutine thread_mutex_unlock(mutex_id, info) bind(c)
            use iso_c_binding
            integer(c_int), intent(in)  :: mutex_id
            integer(c_int), intent(out) :: info
        end subroutine

        subroutine thread_mutex_destroy(mutex_id, info) bind(c)
            use iso_c_binding
            integer(c_int), intent(in)  :: mutex_id
            integer(c_int), intent(out) :: info
        end subroutine
    end interface
end module
