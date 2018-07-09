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

module MEPingPong

    use MPThreads

    implicit none
    private

    logical :: pingpong = .false.
    integer :: mutex

    type, public :: TEPingPong
    contains
        procedure, nopass :: present
    end type
contains
    subroutine present()
        integer pinger
        integer ponger

        integer attribute_id
        integer argument
        integer, target  :: result
        integer, pointer :: result_pointer
        integer info

        argument     = 0
        attribute_id = -1
        result_pointer => result

        call pthread_init(info)
        if (info .ne. 0) then
            print *, 'Error initializing.'
            return
        end if

        call pthread_mutex_init(mutex, attribute_id, info)

        call pthread_create(pinger, attribute_id, ping, argument, info)
        call pthread_create(ponger, attribute_id, pong, argument, info)
        if (info .ne. 0) then
            print *, 'Error creating threads.'
        endif

        call pthread_join(pinger, result_pointer, info)
        call pthread_join(ponger, result_pointer, info)
        if (info .ne. 0) then
            print *, 'Error joining threads.'
        endif

        call pthread_mutex_destroy(mutex, info)
        call pthread_destroy(info)
        if (info .ne. 0) then
            print *, 'Error destroying.'
        endif
    end subroutine

    subroutine ping(argument)
        integer :: argument
        integer index
        integer info
        integer, target  :: status
        integer, pointer :: status_pointer

        status_pointer => status
        status         =  0

        do index = 1, 10
            call pthread_mutex_lock(mutex, info)

            if (pingpong) then
                print *, 'Ping'
                pingpong = .false.
            end if

            ! additional async output
            print *, ''

            call pthread_mutex_unlock(mutex, info)
        end do

        call pthread_exit(status_pointer)
    end subroutine

    subroutine pong(argument)
        integer :: argument
        integer index
        integer info
        integer, target  :: status
        integer, pointer :: status_pointer

        status_pointer => status
        status         =  0

        do index = 1, 10
            call pthread_mutex_lock(mutex, info)

            if (pingpong .ne. .true.) then
                print *, 'Pong'
                pingpong = .true.
            end if

            call pthread_mutex_unlock(mutex, info)
        end do

        call pthread_exit(status_pointer)
    end subroutine
end module
