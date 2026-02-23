program main
  use c_if
  use sound_generate
  use iso_c_binding
  use parse
  
  implicit none

  type(music)::music_data
  type(c_ptr)::mt
  type(c_ptr)::event
  type(c_ptr)::bufp
  
  integer::i
  real::ro

  mt = system_setup()
  bufp = bp(mt)

  call setup_music("tamabashi.msc", music_data)

  call sound_start(mt)
  call play(mt, music_data)
  call sound_stop(mt)

  call system_cleanup(mt)
end program main
