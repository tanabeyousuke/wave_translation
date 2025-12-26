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

    call c_f_pointer(bp(mt), input, SHAPE=[4410])

    allocate(buf(len))

    msc%synth%slc=.true.
    do
       do i = 1, 50
          input(:) = 0
          do i1 = 1, size(msc%synth)
             input(:) = input(:) + msc%synth(i1)%buffer(((i - 1) * 4410) + 1:i * 4410) / size(msc%synth)
          end do
          call sound_write(mt)
       end do
    
       call buf_fill(msc)
       
    end do
  end subroutine play

  subroutine buf_fill(msc)
    type(music),intent(inout)::msc

    integer::i, i1

    print *, "fill"
    do i = 1, size(msc%synth)

       do i1 = 1, 220500
          msc%synth(i)%buffer(i1) = osc_sin(440.0 * (i1 / 44100.0)) * 0.8
       end do
    end do
  end subroutine buf_fill
    

end module sound_generate
