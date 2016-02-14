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

    integer, parameter, public :: ORDERED_ASCENDING  = -1
    integer, parameter, public :: ORDERED_SAME       = 0
    integer, parameter, public :: ORDERED_DESCENDING = 1

    integer, parameter, public :: VARIABLE_ARGUMENT_LIST_MAX_LENGTH = 32

    integer, parameter, public :: CHARACTER_TYPE = 0
    integer, parameter, public :: COMPLEX_TYPE   = 1
    integer, parameter, public :: DOUBLE_TYPE    = 2
    integer, parameter, public :: INTEGER_TYPE   = 3
    integer, parameter, public :: LOGICAL_TYPE   = 4
    integer, parameter, public :: REAL_TYPE      = 5

    type, public :: Object
    private
    contains
        procedure, pass :: init        => object_init
        procedure, pass :: destroy     => object_destroy
        procedure, pass :: equals      => object_equals
        procedure, pass :: description => object_description
    end type

    type, public :: ObjectLink
        class(Object), pointer :: link
    end type

    type, extends(Object), public :: Array
    private
        type(ObjectLink), dimension(:), allocatable :: list
        logical :: selfAllocated
    contains
        procedure, private, pass :: init => array_init
        procedure, pass :: destroy       => array_destroy

        procedure, private, pass :: initWithFArray_charcter => array_initWithFArray_character
        procedure, private, pass :: initWithFArray_integer  => array_initWithFArray_integer
        procedure, private, pass :: initWithFArray_logical  => array_initWithFArray_logical
        procedure, private, pass :: initWithFArray_real     => array_initWithFArray_real

        generic :: initWithFArray => initWithFArray_charcter, initWithFArray_integer, &
                initWithFArray_logical, initWithFArray_real

        procedure, pass :: count          => array_count
        procedure, pass :: objectAtIndex  => array_objectAtIndex

        procedure, pass :: sortedArrayUsingFunction => array_sortedArrayUsingFunction
    end type

    type, extends(Object), public :: Date
    private
    contains
        procedure, pass :: init           => date_init
        procedure, pass :: initWithString => date_initWithString
    end type

    type, extends(Object), public :: Number
    private
        integer :: type

        character        :: characterVal
        complex          :: complexVal
        double precision :: doubleVal
        integer          :: integerVal
        logical          :: logicalVal
        real             :: realVal
    contains
        procedure, pass :: initWithCharacter => number_initWithCharacter
        procedure, pass :: initWithComplex   => number_initWithComplex
        procedure, pass :: initWithDouble    => number_initWithDouble
        procedure, pass :: initWithInteger   => number_initWithInteger
        procedure, pass :: initWithLogical   => number_initWithLogical
        procedure, pass :: initWithReal      => number_initWithReal

        procedure, pass :: characterValue    => number_characterValue
        procedure, pass :: complexValue      => number_complexValue
        procedure, pass :: doubleValue       => number_doubleValue
        procedure, pass :: integerValue      => number_integerValue
        procedure, pass :: logicalValue      => number_logicalValue
        procedure, pass :: realValue         => number_realValue

        procedure, private, nopass :: assign_character => number_assign_character
        procedure, private, nopass :: assign_complexr  => number_assign_complex
        procedure, private, nopass :: assign_double    => number_assign_double
        procedure, private, nopass :: assign_integer   => number_assign_integer
        procedure, private, nopass :: assign_logical   => number_assign_logical
        procedure, private, nopass :: assign_real      => number_assign_real
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
        type(ObjectLink), dimension(VARIABLE_ARGUMENT_LIST_MAX_LENGTH) :: list
        integer :: count
    contains
        procedure, pass :: initWithObjects => valist_initWithObjects
        procedure, pass :: objectAtIndex   => valist_objectAtIndex
        procedure, pass :: length          => valist_length
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

        module subroutine array_destroy(self)
            class(Array), intent(in out) :: self
        end subroutine

        module subroutine array_initWithFArray_character(self, list)
            class(Array), intent(in out)        :: self
            character, dimension(:), intent(in) :: list
        end subroutine

        module subroutine array_initWithFArray_integer(self, list)
            class(Array), intent(in out)      :: self
            integer, dimension(:), intent(in) :: list
        end subroutine

        module subroutine array_initWithFArray_logical(self, list)
            class(Array), intent(in out)      :: self
            logical, dimension(:), intent(in) :: list
        end subroutine

        module subroutine array_initWithFArray_real(self, list)
            class(Array), intent(in out)   :: self
            real, dimension(:), intent(in) :: list
        end subroutine

        module function array_count(self) result(value)
            class(Array), intent(in) :: self
            integer                  :: value
        end function

        module function array_objectAtIndex(self, index) result(value)
            class(Array), intent(in) :: self
            integer, intent(in)      :: index
            class(Object), pointer   :: value
        end function

        module function array_sortedArrayUsingFunction(self, comparator) result(value)
            class(Array), intent(in) :: self
            interface
                function comparator(value1, value2) result(order)
                    import Object

                    class(Object), pointer, intent(in) :: value1
                    class(Object), pointer, intent(in) :: value2
                    integer                            :: order
                end function
            end interface
            class(Array), pointer :: value
        end function

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
        module subroutine number_initWithCharacter(self, value)
            class(Number), intent(in out) :: self
            character, intent(in)         :: value
        end subroutine

        module subroutine number_initWithComplex(self, value)
            class(Number), intent(in out) :: self
            complex, intent(in)           :: value
        end subroutine

        module subroutine number_initWithDouble(self, value)
            class(Number), intent(in out) :: self
            double precision, intent(in)  :: value
        end subroutine

        module subroutine number_initWithInteger(self, value)
            class(Number), intent(in out) :: self
            integer, intent(in)           :: value
        end subroutine

        module subroutine number_initWithLogical(self, value)
            class(Number), intent(in out) :: self
            logical, intent(in)           :: value
        end subroutine

        module subroutine number_initWithReal(self, value)
            class(Number), intent(in out) :: self
            real, intent(in)              :: value
        end subroutine

        module function number_characterValue(self) result(value)
            class(Number), intent(in) :: self
            character                 :: value
        end function

        module function number_complexValue(self) result(value)
            class(Number), intent(in) :: self
            complex                   :: value
        end function

        module function number_doubleValue(self) result(value)
            class(Number), intent(in) :: self
            double precision          :: value
        end function

        module function number_integerValue(self) result(value)
            class(Number), intent(in) :: self
            integer                   :: value
        end function

        module function number_logicalValue(self) result(value)
            class(Number), intent(in) :: self
            logical                   :: value
        end function

        module function number_realValue(self) result(value)
            class(Number), intent(in) :: self
            real                      :: value
        end function

        module subroutine number_assign_character(instance, value)
            class(Number), intent(out)   :: instance
            character, intent(in)        :: value
        end subroutine

        module subroutine number_assign_complex(instance, value)
            class(Number), intent(out)   :: instance
            complex, intent(in)          :: value
        end subroutine

        module subroutine number_assign_double(instance, value)
            class(Number), intent(out)   :: instance
            double precision, intent(in) :: value
        end subroutine

        module subroutine number_assign_integer(instance, value)
            class(Number), intent(out)   :: instance
            integer, intent(in)          :: value
        end subroutine

        module subroutine number_assign_logical(instance, value)
            class(Number), intent(out)   :: instance
            logical, intent(in)          :: value
        end subroutine

        module subroutine number_assign_real(instance, value)
            class(Number), intent(out)   :: instance
            real, intent(in)             :: value
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
                 o1,  o2,  o3,  o4,  o5,  o6,  o7,  o8,  o9, o10, o11, o12, o13, o14, o15, o16, &
                o17, o18, o19, o20, o21, o22, o23, o24, o25, o26, o27, o28, o29, o30, o31, o32)
            class(VariableArgumentList), intent(in out) :: self
            class(Object), target, optional, intent(in out)     :: &
                 o1,  o2,  o3,  o4,  o5,  o6,  o7,  o8,  o9,  o10, o11, o12, o13, o14, o15, o16, &
                o17, o18, o19, o20, o21, o22, o23, o24, o25, o26, o27, o28, o29, o30, o31, o32
        end subroutine

        module function valist_objectAtIndex(self, index) result(value)
            class(VariableArgumentList), intent(in) :: self
            integer, intent(in)    :: index
            class(Object), pointer :: value
        end function

        module function valist_length(self) result(value)
            class(VariableArgumentList), intent(in) :: self
            integer :: value
        end function
    end interface

    interface assignment(=)
        module procedure number_assign_character
        module procedure number_assign_complex
        module procedure number_assign_double
        module procedure number_assign_integer
        module procedure number_assign_logical
        module procedure number_assign_real

        module procedure string_assign_fstring
    end interface
    public :: assignment(=)
end module
