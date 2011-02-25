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
program cluster
  use particles
  implicit none

  type (Particle), allocatable :: parray(:)
  type (Particle) :: p
  integer :: size, itr
  integer :: error  ! to check the if allocate could get memory

  ! for command line arguments
  integer :: argc 
  character(len=10) :: sizearg
  character(len=256) :: filename

  ! file handling
  integer, parameter :: fh = 42  ! file handle number
  integer, parameter :: fh_conf = 43 

  integer :: ios  ! input/output stat
  integer :: lines  ! number of lines read
  character(len=1024) :: buffer

  ! molecules information
  integer :: id, type, molecule_id, molecule_type, neighbour

  ! to parse the configuration file
  integer :: length
  integer :: numtoks = 0
  character(len=10), dimension(1:64) :: tokens 

  ! check how many command line arguments were passed
  argc = iargc()
  if (argc /= 3) then
     print *, "usage: cluster <particle number> <particles> <configuration file>"
     call exit(1)  ! gnu extension not in standard
  end if

  call getarg(1, sizearg)   ! get the first command line arg
  read(sizearg, '(i10)') size  ! transform string to integer

  call getarg(2, filename)  ! get the second command line arg
  open(fh, file=filename, iostat=ios)

  ! if ios is 0 it means that everything is ok
  if (ios /= 0) then
     print *, "file error, does file exist?"
     call exit(1)
  end if

  ! get memory for array
  allocate(parray(size), stat=error) 
  if (error /= 0) then  ! check if we can allocate that much memory
     print *, "error: couldn't allocate memory for array of size=", size
     call exit(1)
  end if
  
  ios = 0
  lines = 1
  do while ((ios == 0) .and. (lines <= size))
     read(fh, '(A)', iostat=ios) buffer   

     if (ios == 0) then

        read(buffer, '(i5,i6,i6,i5)') id, type, molecule_id, molecule_type

        if (id > size) then
           print *, "a particle has id:", id,  "larger than num of ptcls", size
           call exit(1)  ! 
        end if

        p = create_particle(id, type, molecule_id, molecule_type)

        ! assumption that the particles are ordered by their id
        parray(id) = p
        lines = lines + 1
     end if

  end do    

  ! open configuration file
  call getarg(3, filename)
  open(fh_conf, file=filename, iostat=ios)

  ! if ios is 0 it means that everything is ok
  if (ios /= 0) then
     print *, "file error, does file", filename, "exist?"
     call exit(1)
  end if

  ios = 0
  lines = 1
  do while (ios == 0)
     read(fh_conf, '(A)', iostat=ios) buffer       
     
     if (ios == 0) then
        length = len(buffer)
        numtoks = 0
        call tokenize(length, buffer, 10, tokens, numtoks)
        read(tokens(1), '(i10)') id
        if (id > size) then
           print *, "a particle has id:", id,  "larger than num of ptcls", size
           call exit(1)  ! exit
        end if
        
        ! 5 is not a magic number it derives from the fact that
        ! the first column is the id, then comes position in 
        ! x, y, z, and then some other real that I don't know
        ! what represents
        call init_neighbours(parray(id), numtoks - 5)
        do itr = 6, numtoks
           read(tokens(itr), '(i10)') neighbour 
           parray(id)%neighbours(itr - 5) = neighbour
           parray(id)%number_of_neighbours = numtoks - 5   
        end do
        
        lines = lines + 1
     end if
  end do      

  do itr = 1, size
     p = parray(itr)
     if ((has_neighbours(p)) .and. (.not. is_in_cluster(p))) then
        call crawl(p, parray, size, itr)
     end if
  end do

  do itr = 1, size
     p = parray(itr)
     call print_particle(p)
  end do

  contains

    subroutine tokenize(length, str, toklen, tokens, numtoks)
      ! probably this can be improved using pointers
      ! that reference part of the array
      integer, intent(in) :: length, toklen
      character(len=length), intent(in) :: str
      integer, intent(out) :: numtoks
      character(len=toklen), dimension(1:64), intent(out) :: tokens
      
      integer :: pos
      integer :: idx 
      integer :: bufflen
      
      character(len=toklen) :: token
      character(len=length) :: buffer
      
      idx = 1
      pos = 1
      buffer = str
      bufflen = len_trim(buffer)
      
      do while (bufflen /= 0)
         pos = scan(buffer, ' ')  ! get the position of the fst space    
         token = buffer(1:pos)    ! get the number
         buffer = buffer(pos:bufflen)
         buffer = trim(adjustl(buffer))     
         bufflen = len_trim(buffer)
         tokens(idx) = token          
         idx = idx + 1
      end do
      
      numtoks = idx - 1
    end subroutine tokenize

end program cluster

  
