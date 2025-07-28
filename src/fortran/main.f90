module c_interface
  implicit none

  interface
     use iso_c_binding
     
     subroutine initalize() bind(c, name='initalize')
     end subroutine initalize
     
     subroutine output_setup(
