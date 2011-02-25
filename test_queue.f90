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
program test_queue
  use circularqueue
  implicit none

  
  type (Queue) :: my_queue
  integer :: itr = 1
  integer :: elem 
  
  call init_queue(my_queue, 10)
  print *, "is empty: ", is_empty(my_queue), "should be T"

  do itr = 1, 10
     call push(my_queue, itr)
     call printq(my_queue)
  end do
  
  if (is_full(my_queue)) then
     print *, "test passed"
  else
     print *, "something wrong :-("
  end if

  call printq(my_queue)

  do itr = 1, 10
     call printq(my_queue)
     call pop(my_queue, elem)
  end do
  
  if (is_empty(my_queue)) then
     print *, "queue is empty yay!"
  else
     print *, "something wrong :-("
  end if
  
  call push(my_queue, 42)
  call printq(my_queue)
  print *, "is empty", is_empty(my_queue), " == T? Then it's wrong"
  call pop(my_queue, elem)
  print *, "is empty", is_empty(my_queue), " == F? Then it's wrong"


end program test_queue
