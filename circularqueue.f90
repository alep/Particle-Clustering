! Copyright (c) 2011 Alejandro Peralta

! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:

! The above copyright notice and this permission notice shall be included in
! all copies or substantial portions of the Software.

! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
! THE SOFTWARE
module circularqueue
  implicit none
  
  type Queue
     integer :: qnext
     integer :: qend
     integer :: qstart

     integer :: qelems ! this is how many elements there are

     integer :: qsize  ! this is how many elements the queue can hold
                       ! at the same time

     integer, pointer :: data(:)
  end type Queue

contains

  subroutine init_queue(q, qsize)
    type (Queue), intent(inout) :: q
    integer, intent(in) :: qsize

    q%qelems = 0
    q%qnext = 0
    q%qstart = 0
    q%qend = 0
    q%qsize = qsize
    q%qelems = 0

    allocate(q%data(0:qsize-1))

  end subroutine init_queue

  subroutine push(q, element)
    type (Queue), intent(inout) :: q
    integer, intent(in) :: element
    integer :: next, end, size, elems

    next = q%qnext
    end = q%qend
    size = q%qsize
    elems = q%qelems

    q%data(next) = element
    q%qnext = mod(next + 1, size) 
    q%qend = mod(end + 1, size)
    q%qelems = elems + 1
  end subroutine push

  subroutine pop(q, element)
    type (Queue), intent(inout) :: q
    integer, intent(out) :: element
    integer :: start, size, elems

    start = q%qstart
    size = q%qsize
    elems = q%qelems

    element = q%data(start)
    q%qstart = mod(start + 1, size)
    q%qelems = elems - 1
  end subroutine pop

  function is_full(q) result(full)
    type (Queue), intent(in) :: q
    logical :: full
    
    if (q%qnext == q%qstart) then 
       full = .true.
    else
       full = .false.
    end if
  end function is_full

  function is_empty(q) result(empty)
    type (Queue), intent(in) :: q
    logical :: empty

    if (q%qelems == 0) then 
       empty = .true.
    else
       empty = .false.
    end if
  end function is_empty

  subroutine printq(q)
    type (Queue), intent(in) :: q
    
    write (*, '(i5,i5,i5,i5,i5)') q%qstart, q%qend, q%qnext, q%qelems, q%qsize
  end subroutine printq

end module circularqueue

