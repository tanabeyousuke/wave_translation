program main
  use iso_c_binding
  implicit none
  interface
     function newaudio() bind(C, NAME='newaudio') 
       use iso_c_binding
       integer(c_intptr_t)::newaudio
       !pulseaudioのpa_simple_newに相当します。
     end function newaudio
     
     subroutine freeaudio(pa) bind(C, NAME='freeaudio')
       use iso_c_binding
       integer(c_intptr_t)::pa
       !pulseaudioのpa_simple_freeに相当するはずなんです..がね..
     end subroutine freeaudio
     
  end interface
  
  integer(c_intptr_t) :: pa
  pa = newaudio()
  print *, pa
  call freeaudio(pa)
end program main
