program main
  use env
  implicit none

  type(env_data) data
  integer i, push

  data%atk = 50
  data%dec = 20
  data%sus = 40
  data%rel = 50

  push = 250
  
  do i = 1, 1000
     print *, env_out(data, i, push)
  end do

end program main
