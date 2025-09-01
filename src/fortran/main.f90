program c_interface
  use iso_c_binding
  implicit none

  interface
     function login() bind(c, name="share_login") !共有メモリにログインします。
       use iso_c_binding
       type(c_ptr) login
     end function login
     
     subroutine logout(metadata) bind(c, name="share_close") !共有メモリからログアウトします。
       use iso_c_binding
       type(c_ptr),value::metadata
     end subroutine logout
     
     function buffer_allocate() bind(c, name="buffer_malloc") !real(c_double)型配列の予約
       use iso_c_binding
       type(c_ptr) buffer_allocate
     end function buffer_allocate
     
     subroutine buffer_free(p) bind(c, name="free")
       use iso_c_binding
       type(c_ptr),value::p
     end subroutine buffer_free

     ! subroutine usleep(time) bind(c, name="usleep")
     !   use iso_c_binding
     !   integer(c_int),value::time
     ! end subroutine usleep
     
     subroutine memory_write(metadata, buffer) bind(c, name="memory_write") !real(c_double)型の配列を送ってもらう
       use iso_c_binding
       type(c_ptr),value::metadata,buffer
     end subroutine memory_write

  end interface
  
  integer s1
  real,parameter::pi = 3.1415927
  real,parameter::samp = 48000.0

  type(c_ptr) metadata, buffer_p
  real(c_double),pointer::buffer(:)
  

  metadata = login()
  buffer_p = buffer_allocate()
  call c_f_pointer(buffer_p, buffer, SHAPE=[48000])

  do s1=1, 48000
     buffer(s1) = sin(2 * pi * 1000 * (s1 / samp)) * ((48000 - s1) / 48000.0)
  end do
  call memory_write(metadata, buffer_p)

  do s1=1, 48000
     buffer(s1) = sin(2 * pi * 1000 * (s1 / samp)) * ((48000 - s1) / 48000.0)
  end do
  call memory_write(metadata, buffer_p)

  do s1=1, 48000
     buffer(s1) = sin(2 * pi * 1000 * (s1 / samp)) * ((48000 - s1) / 48000.0)
  end do
  call memory_write(metadata, buffer_p)
  
  call buffer_free(buffer_p)
  call logout(metadata)
end program c_interface

