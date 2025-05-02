program main
  use iso_c_binding
  implicit none
  interface
     subroutine newaudio(pa) bind(C, NAME='newaudio')
       use iso_c_binding
       type(c_ptr)::pa
       !pulseaudioのpa_simple_newに相当します。
     end subroutine newaudio
     
     subroutine freeaudio(pa) bind(C, NAME='freeaudio')
       use iso_c_binding
       type(c_ptr)::pa
       !pulseaudioのpa_simple_freeに相当するはずなんです..がね..
     end subroutine freeaudio
     

  end interface
  
  type(c_ptr) pa
  print *, pa

  call newaudio(pa)
  call freeaudio(pa)
end program main
