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

module MIntrinsicRandom

    use MRandom

    implicit none
    private

    type, extends(TRandom), public :: TIntrinsicRandom
        logical initialized
    contains
        procedure :: random
    end type
contains
    function random(instance, from, to)
        class(TIntrinsicRandom), intent(in out) :: instance
        integer, optional, intent(in) :: from
        integer, optional, intent(in) :: to
        integer :: random

        integer :: low
        integer :: high

        real value

        if (present(from) .and. present(to)) then
            low  = from
            high = to
        else
            low  = 1
            high = 256
        end if

        if (instance%initialized == .false.) then
            call initialize()
            instance%initialized = .true.
        end if

        call random_number(value)
        random = from + floor((to + 1 - from) * value)
    end function

    subroutine initialize()
        integer :: index, size, clock
        integer, dimension(:), allocatable :: seed

        call random_seed(size = size)
        allocate(seed(size))

        call system_clock(count = clock)

        seed = clock + 37 * (/ (index - 1, index = 1, size) /)
        call random_seed(put = seed)

        deallocate(seed)
    end subroutine
end module
