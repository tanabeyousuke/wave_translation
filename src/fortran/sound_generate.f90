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
        len=(60 * 4.0) / (120 * 4) * 44100
    print *, len

    allocate(buf(len))

    i = 0
    set%slc=.true.
    do
       ! if(set%slc .eqv. .true.)then
       !    if(set%len1 - i < 4410)then
       !       input(1:set%len1-i) = set%note_wave1(i:set%len1)
       !       input(set%len1-i+1:4410) = set%note_wave2(1:4410-(set%len1-i))
       !       set%slc=.false.
       !       i = 0
       !    else
       !       input(1:4410) = set%note_wave1(i:i+4409)
       !    end if
       ! else
       !    if(set%len2 - i < 4410)then
       !       input(1:set%len2-i) = set%note_wave2(i:set%len2)
       !       input(set%len2-i+1:4410) = set%note_wave1(1:4410-(set%len2-i))
       !       set%slc=.true.
       !       i = 0
       !    else
       !       input(1:4410) = set%note_wave2(i:i+4409)
       !    end if
       ! end if
       i = i + 4410
       call sound_write(mt)
    end do
  end subroutine play

  subroutine fill_buffer(set)
    type(setting)::set

    integer::i, index, start

    start = set%next
    do i=1,5
       index = MODULO(start + i - 2, 5) + 1
       print *, index
    end do
  end subroutine fill_buffer
end module sound_generate
