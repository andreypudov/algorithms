!
! The Laboratory of Algorithms
!
! The MIT License
!
! Copyright 2011-2016 Andrey Pudov.
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

module Foundation

    implicit none
    private

    type, public :: Object
    private
    contains
        procedure, pass :: init
        procedure, pass :: destroy
        procedure, pass :: equals
        procedure, pass :: description
    end type

    type, extends(Object), public :: String
    private
        character(len=:), allocatable :: data
    contains
        procedure, nopass :: initWithFormat
        procedure, nopass :: initWithFString
        procedure, pass :: length
    end type

    interface
        !
        ! Object
        !
        module subroutine init(self)
           class(Object), intent(in out) :: self
        end subroutine

        module subroutine destroy(self)
            class(Object), intent(in out) :: self
        end subroutine

        module function equals(self, any) result(value)
            class(Object), target, intent(in) :: self
            class(Object), target, intent(in) :: any
            logical :: value
        end function

        module function description(self) result(value)
            class(Object), intent(in) :: self
            type(String) :: value
        end function

        !
        ! String
        !
        module function initWithFormat() result(value)
            type(String), pointer :: value
        end function

        module function initWithFString(text) result(value)
            character(len=*), intent(in) :: text
            type(String), pointer        :: value
        end function

        module function getFString(self) result(value)
            class(String), target, intent(in) :: self
            character(len=:), pointer         :: value
        end function

        module function length(self) result(value)
            class(String), intent(in) :: self
            integer :: value
        end function
    end interface
end module
