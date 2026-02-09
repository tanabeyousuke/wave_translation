program main
  use parse
  implicit none
  
  type(setting)::set
  integer::i

  call execute("仕様.txt", 10, set)
end program main
