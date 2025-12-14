program c_interface
  use c_if
  use sound_generate
  use iso_c_binding
  use parse
  
  implicit none

  type(music)::music_data
  type(c_ptr) mt
  type(c_ptr) event
  type(c_ptr) bufp
  
  integer i
  real ro

  mt = system_setup()
  bufp = bp(mt)

  call setup_music("technopolis.msc", music_data)
  call fill_buffer(music_data%synth(1))

  ! call write(music_data%synth(1))

  ! call sound_start(mt)
  ! call play(mt, music_data%synth(1))
  ! call sound_stop(mt)

  call system_cleanup(mt)
end program c_interface

 
