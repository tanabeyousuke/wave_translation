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

     subroutine sound(pa,bufferd,buffer8) bind(C, NAME='write_audio')
       use iso_c_binding
       type(c_ptr) ,value::pa
       type(c_ptr) ,value::bufferd
       type(c_ptr) ,value::buffer8
     end subroutine sound

     function buffer(kata) bind(c, name='make_buffer')
       use iso_c_binding
       type(c_ptr) buffer
       integer(c_int), value::kata
     end function buffer

     subroutine freebuffer(pa) bind(C, NAME='freebuffer')
       use iso_c_binding
       type(c_ptr) ,value::pa
       !pulseaudioのpa_simple_freeに相当しました！やったね！
     end subroutine freebuffer
     
     !後々こ↑こ↓に書くであろう関数
     !write_to_server(波形バッファを変換してサーバーに渡す)
     
  end interface
  
  type(c_ptr) pa, p, buf8
  real(c_double) t
  real(c_double), pointer::buf(:)
  integer i
  
  pa = newaudio()

  p = buffer(0)
  buf8 = buffer(1)
  call c_f_pointer(p, buf, shape=[48000])

  do i = 1, 48000
     t = (i - 1) * 3.1415927 * 2 * (1000.0 / 48000)
     buf(i) = sin(t) * 0.8
  end do

  
  call sound(pa, p, buf8)
  
  call freeaudio(pa)
  call freebuffer(p)
  call freebuffer(buf8)
  print *, "free"
end program main
