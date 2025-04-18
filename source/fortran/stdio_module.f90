module stdio
  implicit none
  interface
     subroutine hello() bind(c)
     end subroutine hello
  end interface
end module stdio
