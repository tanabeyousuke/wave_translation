module parse
  implicit none
  
contains
  subroutine opcode_parse(mnemonic)
    character(*),intent(in)::mnemonic
    integer pos

    pos = index(mnemonic, ' ')
    if(pos == 0)then
       print *, "control", " ", mnemonic
    else
       print *, mnemonic(1:pos), "(", mnemonic(pos + 1:len(mnemonic)), ")"
    end if
  end subroutine opcode_parse

  subroutine mnemonic_parse(mnemonic, scpos)
    character(*),intent(in)::mnemonic
    integer,intent(in)::scpos

    character opcode*10 
    integer headpos

    do headpos=1, scpos
       if(mnemonic(headpos:headpos) /= " ") then
          exit
       end if
    end do

    call opcode_parse(mnemonic(headpos:scpos))
  end subroutine mnemonic_parse

  subroutine execute(filename,unit_num)
    character(*),intent(in)::filename
    integer,intent(in)::unit_num

    character(len=256) line, mnemonic
    integer  iostat_value, scpos

    open(unit=unit_num, file=filename, status='OLD', iostat=iostat_value)
    if(iostat_value /= 0)then
       print *, "error"
       stop
    end if
    
    do
       read(unit_num, '(A:)', iostat=iostat_value) line
       if(iostat_value /= 0) exit

       scpos = index(line, ';')
       if(scpos == 0) cycle

       call mnemonic_parse(line, scpos - 1 )
    end do
  end subroutine execute
end module parse
