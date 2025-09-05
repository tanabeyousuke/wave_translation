program main
  impricit none

  character(len=256)
  integer iostat_value, unit_num

  unit_num = 10

  open(unit=unit_num, file='test.exe', status='OLD', iostat=iostat_value)

  if(iostat_value /= 0) then
     stop
  end if

end program main
