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

        integer index
        index = pthreads_init(10)

        print *, index
    end subroutine

    subroutine ping()
        integer index

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

    subroutine pong()
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
