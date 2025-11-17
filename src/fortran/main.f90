program c_interface
  use c_if
  use sound_generate
  use iso_c_binding
  use parse
  
  implicit none

  ! type(c_ptr) mt
  ! type(c_ptr) event
  ! type(c_ptr) bufp

  ! integer i
  ! real ro
  
  ! mt = system_setup()

  ! bufp = bp(mt)

  ! call sound_start(mt)
  ! call play(mt)
  ! call sound_stop(mt)
  ! call system_cleanup(mt)
  type(music)::music_data

  call setup_music("technopolis.msc", music_data)

end program c_interface
 
