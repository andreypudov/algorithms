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

module MHopfieldNetwork

    implicit none
    private

    type, public :: THopfieldNetwork
    private
        integer, dimension(:,:), allocatable :: weights
        integer :: neurons
    contains
        !procedure :: train
        !procedure :: recognize

        procedure :: init
        procedure :: destroy
    end type
contains
    subroutine init(this, neurons)
        class(THopfieldNetwork), intent(in out) :: this
        integer, intent(in) :: neurons

        allocate(this%weights(neurons, neurons))
        this%weights = 0
        this%neurons = neurons
    end subroutine

    subroutine destroy(this)
        class(THopfieldNetwork), intent(in out) :: this

        deallocate(this%weights)
        this%neurons = 0
    end subroutine
end module
