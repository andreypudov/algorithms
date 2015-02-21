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

module MPTData

    implicit none
    public

    abstract interface
        subroutine IRunnable(argument)
            integer :: argument
        end subroutine
    end interface

    type TRunnable
        procedure(IRunnable), pointer, nopass :: run
        integer, pointer                      :: argument
    end type

    type TRunnablePointer
        type(TRunnable), pointer :: value => null()
    end type

    type(TRunnablePointer), dimension(:), pointer :: routine_table => null()
    integer, parameter :: init_size = 16
    integer            :: routine_table_size
    integer            :: routine_table_mutex

contains
    function start_routine(argument) bind(c) result(routine)
        use iso_c_binding
        implicit none

        type(c_ptr), value, intent(in) :: argument
        type(c_ptr)                    :: routine

        type(TRunnable), pointer :: runnable
        integer, pointer         :: value

        call c_f_pointer(argument, runnable)
        call runnable%run(runnable%argument)

        routine = c_null_ptr
    end function
end module
