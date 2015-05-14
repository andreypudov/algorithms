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

module MFOpemMPExample3

    use omp_lib

    implicit none
    private

    type,  public :: TFExample3
    contains
        procedure, nopass :: present
    end type
contains
    subroutine present()
        integer, parameter :: N = 3
        integer, parameter :: K = 3

        integer, dimension(0:K) :: buffer
        integer nthreads, threadid
        integer index, jndex
        integer start, end
        real    divider

        integer count

        !$omp parallel private(nthreads, threadid, buffer) reduction(+:count)

        nthreads = OMP_GET_NUM_THREADS()
        threadid = OMP_GET_THREAD_NUM()
        buffer   = 0
        start    = -1
        end      = -1

        do while (buffer(0) == 0 .and. buffer(1) /= end)
            if (start == -1) then
                divider = N / real(nthreads)
                start   = floor(divider * threadid + 1.0)
                end     = ceiling(start + divider - 1)

                buffer(1) = start - 1
            end if

            print '(I3,4X,4I2)', threadid, buffer(1:K) + 1
            count = count + 1

            buffer(K) = buffer(K) + 1
            index = K
            do while (buffer(index) == N)
                buffer(index) = 0
                index = index - 1
                buffer(index) = buffer(index) + 1
            end do
        end do

        !$omp end parallel

        print '(A,I,A,I)', 'Expected: ', N ** K, ' Processed: ', count
    end subroutine
end module
