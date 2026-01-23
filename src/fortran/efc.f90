module efc
  use parse
  use osc

  implicit none

contains
  subroutine efc_unit_pass(efc, reg, in, out)
    type(effect), intent(inout)::efc(:)
    real, intent(in)::reg(64)
    real, intent(in)::in
    real, intent(out)::out

    integer::i
    real::shield

    shield = in
    do i = 1, size(efc)
       select case(efc(i)%type)
       case(4)
          call dly(efc(i), reg, shield)
       case(7)
          call hcl(efc(i), reg, shield)
       case(8)
          call scl(efc(i), reg, shield)
       end select
    end do
    out = shield

  end subroutine efc_unit_pass

  subroutine dly(efc, reg, inout)
    type(effect), intent(in)::efc
    real, intent(in)::reg(64)
    real, intent(inout)::inout

    integer::reader,writer

    writer = int(efc%data(66151)) + 1
    if(writer > 66150) then
       writer = writer - 66150
    end if

    reader = mod(int(data_real_a(reg, efc%p(1))), 66150)

  end subroutine dly

  subroutine hcl(efc, reg, inout)
    type(effect), intent(in)::efc
    real, intent(in)::reg(64)
    real, intent(inout)::inout

    real::amped
    integer::mode

    mode = int(data_real_a(reg, efc%p(1)))
    amped = inout * data_real_a(reg, efc%p(2))

    if(mode == 1)then
       inout = max(-1.0, min(1.0, amped))
    else if(mode == 2) then
       inout = osc_del(amped)
    end if
  end subroutine hcl
    
  subroutine scl(efc, reg, inout)
    type(effect), intent(in)::efc
    real, intent(in)::reg(64)
    real, intent(inout)::inout

    real::amped

    amped = inout * data_real_a(reg, efc%p(2))

    inout = (2.0/3.14159265) * atan(inout)

  end subroutine scl
 
  function data_real_a(reg, p)
    real, intent(in)::reg(64)
    type(param), intent(in)::p
    real::data_real_a

    if(p%rorv .eqv. .true.)then
       data_real_a = reg(p%reg_num)
    else
       data_real_a = p%value
    end if
  end function data_real_a
  
end module efc
