module sound_generate
  use c_if
  use env
  use osc
  use parse
  implicit none

contains
  subroutine play(mt, set)
    type(c_ptr),intent(in)::mt
    type(setting),intent(inout)::set

    real(c_float),pointer::input(:)
    real,allocatable,dimension(:)::buf
    integer::len,i

    call c_f_pointer(bp(mt), input, SHAPE=[4410])
    
    len=(60 * 4.0) / (120 * 1) * 44100

    allocate(buf(len))

    i = 0
    do
       if(set%slc == 1)then
          if(set%len1 - i < 4410)then
             input(1:set%len1-i) = set%note_wave1(i:set%len1)
             exit
          else
             input(1:4410) = set%note_wave1(i:i+4409)
          end if
       else
          if(set%len2 - i < 4410)then
             input(1:set%len2-i) = set%note_wave2(i:set%len2)
             set%slc = 1
             input(set%len1:4410) = set%note_wave1(1:4410-set%len1)
          else
             input(1:4410) = set%note_wave2(i:i+4409)
          end if
       end if
       i = i + 4410
       print *, input
       call sound_write(mt)
    end do
  end subroutine play

  subroutine write(set)
    type(setting),intent(inout)::set
    
    integer i, len

    len = 22050
    allocate(set%note_wave1(len))
    allocate(set%note_wave2(len))
    set%len1 = len
    set%len2 = len
    do i=1, len
       set%note_wave1(i) = osc_sin(261.626 * (i / 44100.0)) * 0.8
       set%note_wave2(i) = osc_sin(261.626 * (i / 44100.0)) * 0.8
    end do
  end subroutine write

end module sound_generate
