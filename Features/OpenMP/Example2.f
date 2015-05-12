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

module MFOpemMPExample2

    use omp_lib

    implicit none
    private

    type,  public :: TFExample2
    contains
        procedure, nopass :: present
    end type
contains
    subroutine present()
        integer(selected_int_kind(18)), parameter :: intervals = 1e7
        integer(selected_int_kind(18)) :: i
        integer :: nthreads, threadid

        real(kind(1.d0)), parameter :: PI25DT = acos(-1.d0)
        real(kind(1.d0)) :: dx, sum, x
        real(kind(1.d0)) :: f, pi

        real(kind(1.d0)) :: time1, time2

        !$omp parallel
        nthreads = OMP_GET_NUM_THREADS()
        threadid = OMP_GET_THREAD_NUM()
        if (threadid == 0) then
            print '(A,I)', 'The number of threads: ', nthreads
        end if
        !$omp end parallel

        print '(A, I)', 'The number of intervals: ', intervals
        sum   = 0.d0
        dx    = 1.d0 / intervals
        time1 = omp_get_wtime()

        !$omp parallel do private(x, f) &
        !$omp reduction(+:sum)
        do i = intervals, 1, -1
            x = dx * (i - 0.5d0)
            f = 4.d0 / (1.d0 + x * x)
            sum = sum + f
        end do
        !$omp end parallel do

        pi    = dx * sum
        time2 = omp_get_wtime()

        print '(a13,2x,f30.25)', 'Computed PI = ', pi
        print '(a13,2x,f30.25)', 'The true PI = ', PI25DT
        print '(a13,2x,f30.25)', 'Error         ', PI25DT - pi
        print '(X)'
        print *, 'Elapsed time ', time2 - time1, ' s.'
    end subroutine
end module
