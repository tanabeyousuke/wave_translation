program main
  implicit none
  interface
     function newaudio() bind(C, NAME='newaudio')
       use iso_c_binding
       type(c_ptr)::newaudio
       !pulseaudioのpa_simple_newに相当します。
     end function newaudio
  end interface
  
  
end program main
