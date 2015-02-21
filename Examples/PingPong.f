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

    ! pthread_mutex_t mutex
    logical :: pingpong = .false.

    integer :: mutex

    type, public :: TEPingPong
    contains
        procedure :: present
    end type
contains
    subroutine present(instance)
        class(TEPingPong), intent(in) :: instance

        ! pthread_t pinger
        ! pthread_t ponger
        !
        ! pthread_mutex_init(&mutex, NULL)
        !
        ! pthread_create(&pinger, NULL, (void*)&ping, NULL)
        ! pthread_create(&ponger, NULL, (void*)&pong, NULL)
        !
        ! pthread_join(pinger, NULL)
        ! pthread_join(ponger, NULL)

        integer, parameter :: n = 100
        integer, dimension(n) :: threads
        integer thread_id
        integer attribute_id
        integer argument
        integer, target  ::  result
        integer, pointer :: result_pointer
        integer info
        integer index

        argument     = 0
        attribute_id = 0
        result_pointer => result

        call pthread_init(info)
        if (info .ne. 0) then
            print *, 'Error initializing ', info
            return
        end if

        call pthread_mutex_init(mutex, attribute_id, info)

        do index = 1, n
            call pthread_create(thread_id, attribute_id, ping, argument, info)
            threads(index) = thread_id

            if (info .ne. 0) then
                print *, 'Error creating thread ', index
            endif
        enddo

        do index = 1, n
            call pthread_join(threads(index), result_pointer, info)

            if (info .ne. 0) then
                print *, 'Error joining thread ', index
            endif
        enddo

        call pthread_mutex_destroy(mutex, info)
        call pthread_destroy(info)
        if (info .ne. 0) then
            print *, 'Error destroying.'
        endif
    end subroutine

    subroutine ping(argument)
        integer :: argument
        integer index

        print *, 'TEST'

        do index = 1, 100
            ! pthread_mutex_lock(&mutex)
            if (pingpong) then
                print *, 'Ping'
                pingpong = .false.
            !else
                !index = index - 1
            end if
            ! pthread_mutex_unlock(&mutex)
        end do

        ! pthread_exit(0)
    end subroutine

    subroutine pong(argument)
        integer, intent(in) :: argument
        integer index

        do index = 1, 100
            ! pthread_mutex_lock(&mutex)
            if (pingpong .ne. .true.) then
                print *, 'Pong'
                pingpong = .true.
            !else
                !index = index - 1
            end if
            ! pthread_mutex_unlock(&mutex)
        end do

        ! pthread_exit(0)
    end subroutine
end module
