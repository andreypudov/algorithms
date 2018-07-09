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

module MFOpemMPExample4

    use omp_lib

    implicit none
    private

    integer(kind=omp_lock_kind) :: lock
    logical :: pingpong

    type,  public :: TFExample4
    contains
        procedure, nopass :: present
    end type
contains
    subroutine present()

        call omp_init_lock(lock)
        pingpong = .false.

        !$omp parallel default(none) shared(lock, pingpong)
        !$omp sections

        !$omp section
        call ping()

        !$omp section
        call pong()

        !$omp end sections
        !$omp end parallel

        call omp_destroy_lock(lock)
    end subroutine

    subroutine ping()
        integer index
        index = 0

        do while (index <= 10)
            call omp_set_lock(lock)

            if (pingpong) then
                print '(A)', 'Ping'

                pingpong = .false.
                index = index + 1
            end if

            !$omp flush(pingpong)

            call omp_unset_lock(lock)
        end do
    end subroutine

    subroutine pong()
        integer index
        index = 0

        do while (index <= 10)
            call omp_set_lock(lock)

            if (pingpong .ne. .true.) then
                print '(A)', 'Pong'

                pingpong = .true.
                index = index + 1
            end if

            !$omp flush(pingpong)

            call omp_unset_lock(lock)
        end do
    end subroutine
end module
