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

module MNeuron

    implicit none
    private

    type, public :: TNeuron
    private
        integer, dimension(4) :: weights
        integer :: activation
    contains
        procedure :: activate

        final     :: destroy
    end type

    interface TNeuron
        procedure :: init
    end interface
contains
    function activate(this, pattern) result(activation)
        class(TNeuron), intent(in out)     :: this
        integer, dimension(:), intent(in) :: pattern
        integer :: activation
        integer :: index

        do index = 1, size(pattern)
            activation = activation + pattern(index) * this%weights(index)
        end do
    end function

    function init() result(object)
        type(TNeuron) :: object

        object%activation = 0
        object%weights    = 0
    end function

    subroutine destroy(this)
        type(TNeuron), intent(in out) :: this

        this%activation = 0
        this%weights    = 0
    end subroutine
end module
