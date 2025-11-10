module sound_generate
  use iso_c_binding
  use osc
  use env

  implicit none

contains

  subroutine write_wave(buffer, start, env, push, freq)
    type(env_data), intent(in)::env
    real(c_double), pointer, intent(inout)::buffer(:)
    integer, intent(in)::start, push
    real, intent(in)::freq

    integer i

    do i = start, 48000
       buffer(i) = osc_sin((freq/48000) * i) * env_out(env, i - start, push)
    end do
  end subroutine write_wave
end module sound_generate
