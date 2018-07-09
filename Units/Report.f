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

module MUReport

    implicit none
    public

contains
    subroutine report(algorithm, version, sequence, start)
        character(len=*), intent(in) :: algorithm
        character(len=*), intent(in) :: version
        character(len=*), intent(in) :: sequence
        real, intent(in)             :: start

        character(len=*), parameter :: format1 = '(t1, a14, a2, a14, a2, a8, a2, f6.3, a)'
        character(len=*), parameter :: format2 = '(t1, a14, a2, a24, a0, a0, a2, f6.3, a)'
        character(len=80) :: format
        real finish

        character(len=14) :: algorithm_
        character(len=24) :: version_
        character(len=8)  :: sequence_

        format     = format1
        algorithm_ = algorithm
        version_   = version
        sequence_  = sequence

        ! increase version field in case sequence is empty
        if (len(trim(sequence_)) == 0) then
            format = format2
        end if

        call cpu_time(finish)
        print format, algorithm_, ': ', version_, ' ',sequence_, ' ', finish - start, "s."
    end subroutine
end module
