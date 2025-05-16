
program main
  use iso_c_binding
  implicit none
  interface
     function newaudio() bind(C, NAME='newaudio') 
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
       real freq
     end subroutine sound
     
  end interface
  
  type(c_ptr) pa
  pa = newaudio()
  call sound(pa, 420.0)
  call freeaudio(pa)
end program main
