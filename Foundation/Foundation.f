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

    integer, parameter, public :: VARIABLE_ARGUMENT_LIST_MAX_LENGTH = 32

    type, public :: Object
    private
    contains
        procedure, pass :: init        => object_init
        procedure, pass :: destroy     => object_destroy
        procedure, pass :: equals      => object_equals
        procedure, pass :: description => object_description
    end type

    type, extends(Object), public :: Array
    private
    contains
        procedure, pass :: init            => array_init
        procedure, pass :: initWithObjects => array_initWithObjects
    end type

    type, extends(Object), public :: Date
    private
    contains
        procedure, pass :: init           => date_init
        procedure, pass :: initWithString => date_initWithString
    end type

    type, extends(Object), public :: Number
    private
        integer :: intValue
    contains
        procedure, pass :: initWithInteger => number_initWithInteger
        procedure, pass :: integerValue    => number_integerValue
    end type

    type, extends(Object), public :: String
    private
        character(len=:), allocatable :: data
    contains
        procedure, pass :: init        => string_init
        procedure, pass :: destroy     => string_destroy
        procedure, pass :: equals      => string_equals
        procedure, pass :: description => string_description

        procedure, pass :: initWithFormat  => string_initWithFormat
        procedure, pass :: initWithFString => string_initWithFString
        procedure, pass :: getFString      => string_getFString
        procedure, pass :: length          => string_length

        procedure, private, nopass :: assign_fstring => string_assign_fstring
    end type

    type, public :: VariableArgumentList
    private
        type(Object), dimension(VARIABLE_ARGUMENT_LIST_MAX_LENGTH) :: list
        integer :: count
    contains
        procedure, pass :: initWithObjects => valist_initWithObjects
    end type

    interface
        !
        ! Object
        !
        module subroutine object_init(self)
           class(Object), intent(in out) :: self
        end subroutine

        module subroutine object_destroy(self)
            class(Object), intent(in out) :: self
        end subroutine

        module function object_equals(self, any) result(value)
            class(Object), target, intent(in) :: self
            class(Object), target, intent(in) :: any
            logical :: value
        end function

        module function object_description(self) result(value)
            class(Object), intent(in) :: self
            type(String) :: value
        end function

        !
        ! Array
        !
        module subroutine array_init(self)
            class(Array), intent(in out) :: self
        end subroutine

        module subroutine array_initWithObjects(self, text)
            class(Array), intent(in out) :: self
            class(String), intent(in)   :: text
        end subroutine

        !
        ! Date
        !
        module subroutine date_init(self)
            class(Date), intent(in out) :: self
        end subroutine

        module subroutine date_initWithString(self, text)
            class(Date), intent(in out) :: self
            class(String), intent(in)   :: text
        end subroutine

        !
        ! Number
        !
        module subroutine number_initWithInteger(self, value)
            class(Number), intent(in out) :: self
            integer, intent(in)           :: value
        end subroutine

        module function number_integerValue(self) result(value)
            class(Number), intent(in) :: self
            integer                   :: value
        end function

        module subroutine number_assign_integer(instance, value)
            class(Number), intent(out)   :: instance
            integer, intent(in)          :: value
        end subroutine

        !
        ! String
        !
        module subroutine string_init(self)
           class(String), intent(in out) :: self
        end subroutine

        module subroutine string_destroy(self)
            class(String), intent(in out) :: self
        end subroutine

        module function string_equals(self, any) result(value)
            class(String), target, intent(in) :: self
            class(Object), target, intent(in) :: any
            logical :: value
        end function

        module function string_description(self) result(value)
            class(String), intent(in) :: self
            type(String) :: value
        end function

        module subroutine string_initWithFormat(self)
            class(String), intent(in out) :: self
        end subroutine

        module subroutine string_initWithFString(self, text)
            class(String), intent(in out) :: self
            character(len=*), intent(in)  :: text
        end subroutine

        module function string_getFString(self) result(value)
            class(String), target, intent(in) :: self
            character(len=:), pointer         :: value
        end function

        module function string_length(self) result(value)
            class(String), intent(in) :: self
            integer :: value
        end function

        module subroutine string_assign_fstring(instance, text)
            class(String), intent(out)   :: instance
            character(len=*), intent(in) :: text
        end subroutine

        !
        ! Variable argument list
        !
        module subroutine valist_initWithObjects(self, &
                 o1,  o2,  o3,  o4,  o5,  o6,  o7,  o8,  o9,  o10, o11, o12, o13, o14, o15, o16, &
                o17, o18, o19, o20, o21, o22, o23, o24, o25, o26, o27, o28, o29, o30, o31, o32)
            class(VariableArgumentList), intent(in out) :: self
            class(Object), optional, intent(in out)     :: &
                 o1,  o2,  o3,  o4,  o5,  o6,  o7,  o8,  o9,  o10, o11, o12, o13, o14, o15, o16, &
                o17, o18, o19, o20, o21, o22, o23, o24, o25, o26, o27, o28, o29, o30, o31, o32
        end subroutine
    end interface

    interface assignment(=)
        module procedure number_assign_integer
        module procedure string_assign_fstring
    end interface
    public :: assignment(=)
end module
