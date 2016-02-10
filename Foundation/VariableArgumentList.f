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

submodule (Foundation) VariableArgumentList
contains
    module subroutine valist_initWithObjects(self, &
             o1,  o2,  o3,  o4,  o5,  o6,  o7,  o8,  o9, o10, o11, o12, o13, o14, o15, o16, &
            o17, o18, o19, o20, o21, o22, o23, o24, o25, o26, o27, o28, o29, o30, o31, o32)
        class(VariableArgumentList), intent(in out) :: self
        class(Object), target, optional, intent(in out)     :: &
            o1,  o2,  o3,  o4,  o5,  o6,  o7,  o8,  o9,  o10, o11, o12, o13, o14, o15, o16, &
            o17, o18, o19, o20, o21, o22, o23, o24, o25, o26, o27, o28, o29, o30, o31, o32

        integer :: index

        index = 1

        if (present(o1)) then
            !self%list(index) = o1
            self%list(index)%link => o1
            index = index + 1
        end if

        if (present(o2)) then
            !self%list(index) = o2
            self%list(index)%link => o2
            index = index + 1
        end if

        if (present(o3)) then
            !self%list(index) = o3
            self%list(index)%link => o3
            index = index + 1
        end if

        if (present(o4)) then
            !self%list(index) = o4
            self%list(index)%link => o4
            index = index + 1
        end if

        if (present(o5)) then
            !self%list(index) = o5
            self%list(index)%link => o5
            index = index + 1
        end if

        if (present(o6)) then
            !self%list(index) = o6
            self%list(index)%link => o6
            index = index + 1
        end if

        if (present(o7)) then
            !self%list(index) = o7
            self%list(index)%link => o7
            index = index + 1
        end if

        if (present(o8)) then
            !self%list(index) = o8
            self%list(index)%link => o8
            index = index + 1
        end if

        if (present(o9)) then
            !self%list(index) = o9
            self%list(index)%link => o9
            index = index + 1
        end if

        if (present(o10)) then
            !self%list(index) = o10
            self%list(index)%link => o10
            index = index + 1
        end if

        if (present(o11)) then
            !self%list(index) = o11
            self%list(index)%link => o11
            index = index + 1
        end if

        if (present(o12)) then
            !self%list(index) = o12
            self%list(index)%link => o12
            index = index + 1
        end if

        if (present(o13)) then
            !self%list(index) = o13
            self%list(index)%link => o13
            index = index + 1
        end if

        if (present(o14)) then
            !self%list(index) = o14
            self%list(index)%link => o14
            index = index + 1
        end if

        if (present(o15)) then
            !self%list(index) = o15
            self%list(index)%link => o15
            index = index + 1
        end if

        if (present(o16)) then
            !self%list(index) = o16
            self%list(index)%link => o16
            index = index + 1
        end if

        if (present(o17)) then
            !self%list(index) = o17
            self%list(index)%link => o17
            index = index + 1
        end if

        if (present(o18)) then
            !self%list(index) = o18
            self%list(index)%link => o18
            index = index + 1
        end if

        if (present(o19)) then
            !self%list(index) = o19
            self%list(index)%link => o19
            index = index + 1
        end if

        if (present(o20)) then
            !self%list(index) = o20
            self%list(index)%link => o20
            index = index + 1
        end if

        if (present(o21)) then
            !self%list(index) = o21
            self%list(index)%link => o21
            index = index + 1
        end if

        if (present(o22)) then
            !self%list(index) = o22
            self%list(index)%link => o22
            index = index + 1
        end if

        if (present(o23)) then
            !self%list(index) = o23
            self%list(index)%link => o23
            index = index + 1
        end if

        if (present(o24)) then
            !self%list(index) = o24
            self%list(index)%link => o24
            index = index + 1
        end if

        if (present(o25)) then
            !self%list(index) = o25
            self%list(index)%link => o25
            index = index + 1
        end if

        if (present(o26)) then
            !self%list(index) = o26
            self%list(index)%link => o26
            index = index + 1
        end if

        if (present(o27)) then
            !self%list(index) = o27
            self%list(index)%link => o27
            index = index + 1
        end if

        if (present(o28)) then
            !self%list(index) = o28
            self%list(index)%link => o28
            index = index + 1
        end if

        if (present(o29)) then
            !self%list(index) = o29
            self%list(index)%link => o29
            index = index + 1
        end if

        if (present(o30)) then
            !self%list(index) = o30
            self%list(index)%link => o30
            index = index + 1
        end if

        if (present(o31)) then
            !self%list(index) = o31
            self%list(index)%link => o31
            index = index + 1
        end if

        if (present(o32)) then
            !self%list(index) = o32
            self%list(index)%link => o32
            index = index + 1
        end if

        self%count = index - 1
    end subroutine

    module function valist_objectAtIndex(self, index) result(value)
        class(VariableArgumentList), intent(in) :: self
        integer, intent(in)    :: index
        class(Object), pointer :: value

        if ((index < 1) .or. (index > self%count)) then
            ! TODO
        end if

        value => self%list(index)%link
    end function

    module function valist_length(self) result(value)
        class(VariableArgumentList), intent(in) :: self
        integer :: value

        value = self%count
    end function
end submodule
