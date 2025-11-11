program c_interface
  use iso_c_binding
  ! use sound_generate
  implicit none

  interface
     function audio_init() bind(c, name="audio_init")
       use iso_c_binding
       type(c_ptr) audio_init
     end function audio_init

     function system_setup() bind(c, name="system_setup")
       use iso_c_binding
       type(c_ptr) system_setup
     end function system_setup

     subroutine generate_sine_wave(buffer, num_samples, current_phase, f) bind(c, name="genarate_sine_wave")
       use iso_c_binding
       type(c_ptr),value::buffer
       integer(c_int),value::num_samples
       type(c_ptr),value::current_phase
       real(c_float),value::f
     end subroutine generate_sine_wave
     
     subroutine sound_write(meta) bind(c, name="sound_write")
       use iso_c_binding
       type(c_ptr),value::meta
     end subroutine sound_write
       
     subroutine system_cleanup(meta) bind(c, name="system_cleanup")
       use iso_c_binding
       type(c_ptr),value::meta
     end subroutine system_cleanup
     
     subroutine sound_start(meta) bind(c, name="sound_start")
       use iso_c_binding
       type(c_ptr),value::meta
     end subroutine sound_start
     
     subroutine sound_stop(meta) bind(c, name="sound_stop")
       use iso_c_binding
       type(c_ptr),value::meta
     end subroutine sound_stop

     function bp(meta) bind(c, name="buffer_pointer")
       use iso_c_binding
       type(c_ptr),value::meta
       type(c_ptr)::bp
     end function bp
  !    function login() bind(c, name="share_login") !共有メモリにログインします。
  !      use iso_c_binding
  !      type(c_ptr) login
  !    end function login
     
  !    subroutine logout(metadata) bind(c, name="share_close") !共有メモリからログアウトします。
  !      use iso_c_binding
  !      type(c_ptr),value::metadata
  !    end subroutine logout
     
  !    function buffer_allocate() bind(c, name="buffer_malloc") !real(c_double)型配列の予約
  !      use iso_c_binding
  !      type(c_ptr) buffer_allocate
  !    end function buffer_allocate
     
  !    subroutine buffer_free(p) bind(c, name="free")
  !      use iso_c_binding
  !      type(c_ptr),value::p
  !    end subroutine buffer_free
     
  !    subroutine memory_write(metadata, buffer) bind(c, name="memory_write") !real(c_double)型の配列を送ってもらう
  !      use iso_c_binding
  !      type(c_ptr),value::metadata,buffer
  !    end subroutine memory_write

  end interface
  type(c_ptr) mt
  type(c_ptr) event
  type(c_ptr) bufp

  real(c_float),pointer::buf(:)
  integer i
  real ro
  
  mt = system_setup()

  bufp = bp(mt)
  call c_f_pointer(bufp, buf, SHAPE=[4410])

  do i=1, 4410
     ro = 2.0 * 3.1415927 * 1000 / 44100.0 * i
     buf(i) = sin(ro) * 0.8
  end do


  call sound_start(mt)
  do i=1, 100
     call sound_write(mt)
  end do
  call sound_stop(mt)
  call system_cleanup(mt)
  ! integer s1
  ! real,parameter::pi = 3.1415927
  ! real,parameter::samp = 48000.0

  ! type(c_ptr) metadata, buffer_p
  ! real(c_double),pointer::buffer(:)
  ! type(env_data) envelope
  

  ! metadata = login()
  ! buffer_p = buffer_allocate()
  ! call c_f_pointer(buffer_p, buffer, SHAPE=[48000])

  ! call write_wave(buffer, 1, envelope, 24000, 466.16) 

  ! envelope%atk = 1000
  ! envelope%dec = 4000
  ! envelope%sus = 30
  ! envelope%rel = 1000

  ! call memory_write(metadata, buffer_p)
  
  ! call buffer_free(buffer_p)
  ! call logout(metadata)
end program c_interface

