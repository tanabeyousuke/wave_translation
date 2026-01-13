program main
  use env
  implicit none

  integer::i
  real::atk, dec, sus, rel


  atk = 10.0
  dec = 10.0
  sus = 0.4
  rel = 10.0

  do i = 1, 30
     print *, env_out(atk, dec, sus, rel, i, 30, .true.)
  end do

  do i = 31, 50
     print *, env_out(atk, dec, sus, rel, i, 30, .false.)
  end do

end program main
