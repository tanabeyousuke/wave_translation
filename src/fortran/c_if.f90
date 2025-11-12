module c_if
  use iso_c_binding
  implicit none

  interface
     function audio_init() bind(c, name="audio_init")
       use iso_c_binding
       type(c_ptr) audio_init
     end function audio_init

     function system_setup() bind(c, name="system_setup")
       use iso_c_binding
       type(c_ptr) system_setup
     end function system_setup

     subroutine generate_sine_wave(buffer, num_samples, current_phase, f) bind(c, name="genarate_sine_wave")
       use iso_c_binding
       type(c_ptr),value::buffer
       integer(c_int),value::num_samples
       type(c_ptr),value::current_phase
       real(c_float),value::f
     end subroutine generate_sine_wave
     
     subroutine sound_write(meta) bind(c, name="sound_write")
       use iso_c_binding
       type(c_ptr),value::meta
     end subroutine sound_write
       
     subroutine system_cleanup(meta) bind(c, name="system_cleanup")
       use iso_c_binding
       type(c_ptr),value::meta
     end subroutine system_cleanup
     
     subroutine sound_start(meta) bind(c, name="sound_start")
       use iso_c_binding
       type(c_ptr),value::meta
     end subroutine sound_start
     
     subroutine sound_stop(meta) bind(c, name="sound_stop")
       use iso_c_binding
       type(c_ptr),value::meta
     end subroutine sound_stop

     function bp(meta) bind(c, name="buffer_pointer")
       use iso_c_binding
       type(c_ptr),value::meta
       type(c_ptr)::bp
     end function bp
  end interface
end module c_if
