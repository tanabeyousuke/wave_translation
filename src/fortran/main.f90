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

     subroutine sound(pa,freq) bind(C, NAME='write_audio')
       use iso_c_binding
       type(c_ptr) ,value::pa
       real(c_double) freq
     end subroutine sound

     !後々こ↑こ↓に書くであろう関数
     !buffer(波形をreal(c_double)で保存するバッファを生成)
     !write_to_server(波形バッファを変換してサーバーに渡す)
     
  end interface
  
  type(c_ptr) pa
  real(c_double) f
  pa = newaudio()
  f = 1000
  call sound(pa, f)
  call freeaudio(pa)
end program main
