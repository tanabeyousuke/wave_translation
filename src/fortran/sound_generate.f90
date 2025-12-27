module sound_generate
  use c_if
  use env
  use osc
  use parse
  implicit none

contains
  subroutine play(mt, msc)
    type(c_ptr),intent(in)::mt
    type(music),intent(inout)::msc

    real(c_float),pointer::input(:)
    real,allocatable,dimension(:)::buf
    integer::len,i,i1
    logical::flag

    call c_f_pointer(bp(mt), input, SHAPE=[4410])

    allocate(buf(len))

    msc%synth%slc=.false.
    flag = .false.
    do
       do i = 1, 50
          input(:) = 0
          do i1 = 1, size(msc%synth)
             input(:) = input(:) + msc%synth(i1)%buffer(((i - 1) * 4410) + 1:i * 4410) / size(msc%synth)
          end do
          call sound_write(mt)
       end do
    
       call music_generate(msc)

       
       
    end do
  end subroutine play

  subroutine music_generate(msc)
    type(music),intent(inout)::msc

    integer::i, i1

    do i = 1, size(msc%synth)
       call fill_buffer(msc%synth(i))
    end do
  end subroutine music_generate
    
  subroutine fill_buffer(set)
    type(setting)::set

    character(len=80)::line
    integer::unit_num, iostat_value, scpos, ophead, optail
    integer::i

    set%buffer(:) = 0
    do
       read(unit_num, '(A:)', iostat=iostat_value) line
       do i = 1, len(line)
          if (line(i:i) == char(9)) line(i:i) = ' '
       end do

       if(iostat_value /= 0)then
          
          exit
       end if

       scpos = index(line, ';')
       if(scpos == 0) cycle

       ! optail = 1
       ! call get_token(l

       print *, line
    end do

  end subroutine fill_buffer
end module sound_generate
