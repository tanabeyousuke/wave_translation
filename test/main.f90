program main
  use iso_c_binding
  implicit none
  interface
     function yoyaku(l) bind(c, name='yoyaku')
       use iso_c_binding
       integer(c_int), value::l 
       type(c_ptr) yoyaku
     end function yoyaku
        
     subroutine out(array,length) bind(c, name='output')
       use iso_c_binding
       type(c_ptr), value::array
       integer(c_int), value::length
     end subroutine out
     
     subroutine kaihou(p) bind(c, name='kaihou')
       use iso_c_binding
       type(c_ptr), value::p
     end subroutine kaihou

  end interface

  integer l, i
  integer(c_int) length
  type(c_ptr) p
  integer, pointer::array(:)
  l = 5
  length = l
  p = yoyaku(length)
  CALL C_F_POINTER(p, array, SHAPE=[length])

  do i=1, 5 
     array(i) = i
  end do
  
  call out(p, length)
  call kaihou(p)

end program main
