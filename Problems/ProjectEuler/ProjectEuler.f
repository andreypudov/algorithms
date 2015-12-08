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

module MProjectEuler

    use MPEProblem1
    use MPEProblem2
    use MPEProblem3
    use MPEProblem4
    use MPEProblem5
    use MPEProblem6
    use MPEProblem7
    use MPEProblem8
    use MPEProblem9

    use MPEProblem10
    use MPEProblem11
    use MPEProblem12

    implicit none
    private

    type, public :: TProjectEuler
    contains
        procedure, nopass :: present
    end type
contains
    subroutine present()
        type(TPEProblem1) problem1
        type(TPEProblem2) problem2
        type(TPEProblem3) problem3
        type(TPEProblem4) problem4
        type(TPEProblem5) problem5
        type(TPEProblem6) problem6
        type(TPEProblem7) problem7
        type(TPEProblem8) problem8
        type(TPEProblem9) problem9

        type(TPEProblem10) problem10
        type(TPEProblem11) problem11
        type(TPEProblem12) problem12

        !call problem1%present()
        !call problem2%present()
        !call problem3%present()
        !call problem4%present()
        !call problem5%present()
        !call problem6%present()
        !call problem7%present()
        !call problem8%present()
        !call problem9%present()

        !call problem10%present()
        call problem11%present()
        call problem12%present()
    end subroutine
end module
