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

    use MPTData
    use MPTInterface
    use iso_c_binding

    implicit none
    public

contains
    subroutine pthread_init(info)
        integer, intent(out) :: info

        allocate(routine_table(init_size))
        routine_table_size = init_size

        call thread_init(info)
        call thread_mutex_init(routine_table_mutex, -1, info)
    end subroutine

    subroutine pthread_create(thread_id, attr_id, routine, argument, info)
        integer, intent(out) :: thread_id
        integer, intent(in)  :: attr_id
        procedure(IRunnable) :: routine
        integer, target      :: argument
        integer, intent(out) :: info

        procedure(IRunnableRoutine), bind(c), pointer :: routine_ptr
        type(TRunnablePointer), dimension(:), pointer :: buffer
        integer                                       :: index

        call thread_mutex_lock(routine_table_mutex, info)

        call thread_alloc(thread_id, info)
        if (thread_id .gt. routine_table_size) then
            nullify(buffer)
            allocate(buffer(routine_table_size * 2))

            do index = 1, routine_table_size
                buffer(index) = routine_table(index)
            end do

            deallocate(routine_table)
            routine_table => buffer
            routine_table_size = routine_table_size * 2
        endif

        allocate(routine_table(thread_id)%value)
        routine_table(thread_id)%value%run      => routine
        routine_table(thread_id)%value%argument => argument
        routine_ptr => start_routine

        call thread_create(thread_id, attr_id, c_funloc(routine_ptr), &
                c_loc(routine_table(thread_id)%value), info)

        call thread_mutex_unlock(routine_table_mutex, info)
    end subroutine

    subroutine pthread_join(thread_id, value, info)
        integer, intent(in)  :: thread_id
        integer, pointer     :: value
        integer, intent(out) :: info

        type(c_ptr) :: value_pointer

        call thread_join(thread_id, value_pointer, info)
        call c_f_pointer(value_pointer, value)
    end subroutine

    subroutine pthread_destroy(info)
        integer, intent(out) :: info

        deallocate(routine_table)
        routine_table_size = 0

        call thread_mutex_destroy(routine_table_mutex, info)
        call thread_destroy(info)
    end subroutine

    subroutine pthread_mutex_init(mutex_id, attribute_id, info)
        integer, intent(out) :: mutex_id
        integer, intent(in)  :: attribute_id
        integer, intent(out) :: info

        call thread_mutex_init(mutex_id, attribute_id, info)
    end subroutine

    subroutine pthread_mutex_lock(mutex_id, info)
        integer, intent(in)  :: mutex_id
        integer, intent(out) :: info

        call thread_mutex_lock(mutex_id, info)
    end subroutine

    subroutine pthread_mutex_unlock(mutex_id, info)
        integer, intent(in)  :: mutex_id
        integer, intent(out) :: info

        call thread_mutex_unlock(mutex_id, info)
    end subroutine

    subroutine pthread_mutex_destroy(mutex_id, info)
        integer, intent(in)  :: mutex_id
        integer, intent(out) :: info

        call thread_mutex_destroy(mutex_id, info)
    end subroutine
end module
