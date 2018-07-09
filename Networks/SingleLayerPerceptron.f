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

!
! Introduced by F. Rosenbalt in 1958
!
! Sample structure
!
!         Input Values
!          |   |   |
!          O   O   O   Input Layer
!           \ / \ /    Weight Matrix
!            O   O     Output Layer
!            |   |
!        Output Values
!
! Type                 Feedforward
! Neuron Layers        1 input layer
!                      1 output layer
! Input Value Types    Binary
! Activation Function  Hard limiter
! Learning Method      Supervised
! Learning Algorithm   Hebb learning rule
! Mainly Used In       Single logical operations
!                      Pattern classification
!

module MSingleLayerPerceptron

    implicit none
    private

    type, public :: TSingleLayerPerceptron
    private
        integer, dimension(:,:), allocatable :: weights
        integer :: inputNeurons
        integer :: outputNeurons
    contains
        !procedure :: train
        !procedure :: recognize

        procedure :: init
        procedure :: destroy
    end type
contains
    subroutine init(this, inputNeurons, outputNeurons)
        class(TSingleLayerPerceptron), intent(in out) :: this
        integer, intent(in) :: inputNeurons
        integer, intent(in) :: outputNeurons

        allocate(this%weights(inputNeurons, outputNeurons))
        this%inputNeurons  = inputNeurons
        this%outputNeurons = outputNeurons
    end subroutine

    subroutine destroy(this)
        class(TSingleLayerPerceptron), intent(in out) :: this

        deallocate(this%weights)
        this%inputNeurons  = 0
        this%outputNeurons = 0
    end subroutine
end module
