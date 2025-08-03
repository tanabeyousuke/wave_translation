program c_interface
  use iso_c_binding
  implicit none

  interface
     function memory_open() bind(c, name='memory_open')
       use iso_c_binding
       type(c_ptr) memory_open
     end function memory_open

     function shm_address(shm) bind(c, name='shm_address')
       use iso_c_binding
       type(c_ptr) memory_open
       type(c_ptr),value::shm
     end function shm_address
     
     subroutine memory_close(shm) bind(c, name='memory_close')  
       use iso_c_binding
       type(c_ptr), value::shm
     end subroutine memory_close

  end interface
  
  type(c_ptr) shm_data
  shm_data = memory_open()

  call memory_close(shm_data);
  
end program c_interface

     
