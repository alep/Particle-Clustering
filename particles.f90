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
module particles
  use circularqueue
  implicit none

  type Particle
     integer :: id
     integer :: type
     integer :: molecule_id
     integer :: molecule_type

     ! the next values vary on time
     integer, pointer :: neighbours(:)
     integer :: number_of_neighbours
     integer :: cluster  ! -1 means that it isn't in a cluster
                         ! -2 means that it's already in the queue
  end type Particle

contains
  
  function create_particle(id, type, mid, mtype) result(p)
    type (Particle) :: p
    integer, intent(in) :: id, type, mid, mtype

    ! add values to the particle
    p%id = id
    p%type = type
    p%molecule_id = mid
    p%molecule_type = mtype
    p%number_of_neighbours = 0
    p%cluster = -1   
  end function create_particle

  function has_neighbours(p) result(v)
    type (Particle), intent(in) :: p
    logical :: v
    
    if (p%number_of_neighbours > 0) then
       v = .true.
    else
       v = .false.
    end if
  end function has_neighbours

  function is_in_cluster(p) result(v)
    type (Particle), intent(in) :: p
    logical :: v
    
    if (p%cluster /= -1) then
       v = .true.
    else
       v = .false.
    end if
  end function is_in_cluster

  subroutine init_neighbours(p, size)
    type (Particle), intent(in) :: p
    integer, intent(in) :: size

    allocate(p%neighbours(size)) 
  end subroutine init_neighbours

  subroutine print_particle(p)
    type (Particle), intent(in) :: p
    integer :: itr

    write (*, ' (A10,i5,i5,i5,i5,i5, i5)', advance='no') "Particle:", & 
         p%id, p%cluster, p%type, &  
         p%molecule_id, p%molecule_type, p%number_of_neighbours

    if (p%number_of_neighbours > 0) then
       write (*,'(A15)',advance='no') " neighbours:"
    end if
    do itr = 1, p%number_of_neighbours
       write (*, '(i5)', advance='no') p%neighbours(itr)
    end do
    print *, ""

  end subroutine print_particle

  subroutine crawl(startp, parray, qsize, tag)
    type (Particle), intent(in) :: startp
    type (Particle), intent(inout), target :: parray(:)
    integer, intent(in) :: qsize
    integer, intent(in) :: tag
    
    type (Queue) :: q
    type (Particle), pointer :: p, p1
    integer :: id, n, itr

    call init_queue(q, qsize)
    
    if (is_in_cluster(startp)) then
       write (*,'(A)', advance='no') ">>>"
       call print_particle(startp)
       return
    end if

    call push(q, startp%id)

    do while (.not. is_empty(q))
       call pop(q, id)
       ! call print_particle(parray(id))
       p => parray(id)
       p%cluster = tag

       do itr = 1, p%number_of_neighbours
          n = p%neighbours(itr)
          if (n > 0) then 
             p1 => parray(n)
             if ((p1%type == p%type) .and. &
                  (p1%cluster == -1) .and. &
                  (p1%cluster /= -2)) then
                p1%cluster = -2  ! mark as already in the queu
                call push(q, p%neighbours(itr))
             end if
          end if
       end do
    end do
  end subroutine crawl

end module particles
