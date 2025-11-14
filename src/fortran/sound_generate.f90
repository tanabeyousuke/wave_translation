module sound_generate
  use c_if
  use env
  use osc
  use parse
  implicit none


contains



  subroutine play(mt)
    type(c_ptr),intent(in)::mt

    real(c_float),pointer::input(:)
    real,allocatable,dimension(:)::buf
    integer::len,i

    call c_f_pointer(bp(mt), input, SHAPE=[4410])
    
    len=(60 * 4.0) / (120 * 1) * 44100

    allocate(buf(len))
    do i=1, len
        buf(i) = osc_sin(440 * (i / 44100.0)) * 0.8;
    end do

    i = 0
    do
       if(len - i < 4410)then
          input(1:len-i) = buf(i:len)
       else
          input(1:4410) = buf(i:i+4409)
       end if
       i = i + 4410
       call sound_write(mt)
       if(len - i < 4410) exit
    end do

  end subroutine play
end module sound_generate
