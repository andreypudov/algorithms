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

module UFoundation

    use Foundation

    implicit none
    private

    type, public :: TUFoundation
    contains
        procedure, nopass :: present
    end type

    interface
        module subroutine presentObjectInit()
        end subroutine

        module subroutine presentObjectEquals()
        end subroutine

        module subroutine presentObjectInheritance()
        end subroutine

        module subroutine presentStringInitWithFString()
        end subroutine

        module subroutine presentStringEquals()
        end subroutine

        module subroutine presentStringAssignFString()
        end subroutine

        module subroutine presentVariableArgumentList()
        end subroutine
    end interface
contains
    subroutine present()
        call presentObjectInit()
        call presentObjectEquals()
        call presentObjectInheritance()

        call presentStringInitWithFString()
        call presentStringEquals()
        call presentStringAssignFString()
        call presentVariableArgumentList()
    end subroutine
end module
