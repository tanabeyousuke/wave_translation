program main
  use iso_c_binding

  implicit none
  interface
     subroutine test() bind(c)
     end subroutine test
  end interface

  call test
end program main
