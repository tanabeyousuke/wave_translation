program c_interface
  use c_if
  use sound_generate
  use iso_c_binding
  implicit none

  type(c_ptr) mt
  type(c_ptr) event
  type(c_ptr) bufp

  integer i
  real ro
  
  mt = system_setup()

  bufp = bp(mt)

  call sound_start(mt)
  call play(mt)
  call sound_stop(mt)
  call system_cleanup(mt)
end program c_interface
 
