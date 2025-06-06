program main
  use iso_c_binding
  implicit none
  interface
     function newaudio() bind(C, NAME='audio_init') 
       use iso_c_binding
       type(c_ptr) newaudio
       !pulseaudioのpa_simple_newに相当します。
     end function newaudio
     
     subroutine freeaudio(pa) bind(C, NAME='freeaudio')
       use iso_c_binding
       type(c_ptr) ,value::pa
       !pulseaudioのpa_simple_freeに相当しました！やったね！
     end subroutine freeaudio

     subroutine sound(pa,buffer) bind(C, NAME='write_audio')
       use iso_c_binding
       type(c_ptr) ,value::pa
       type(c_ptr) ,value::buffer
     end subroutine sound

     function buffer(kata) bind(c, name='make_buffer')
       use iso_c_binding
       type(c_ptr) buffer
       integer(c_int) kata
     end function buffer
     
     !後々こ↑こ↓に書くであろう関数
     !write_to_server(波形バッファを変換してサーバーに渡す)
     
  end interface
  
  type(c_ptr) pa, p
  real(c_double) t
  real, pointer::buf(:)
  integer i
  
  pa = newaudio()

  p = buffer(0)
  call c_f_pointer(p, buf, shape=[48000])

  do i = 0, 47999
     t = i / 48000 * 1000
     buf(i) = sin(t) * 0.8
  end do
  
  call sound(pa, p)
  
  call freeaudio(pa)
end program main
