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
       case(1)
          call low(efc(i), reg, shield)
       case(2)
          call hig(efc(i), reg, shield)
       case(3)
          call bnd(efc(i), reg, shield)
       case(4)
          call dly(efc(i), reg, shield)
       case(5)
          call bit(efc(i), reg, shield)
       case(7)
          call hcl(efc(i), reg, shield)
       case(8)
          call scl(efc(i), reg, shield)
       end select
    end do
    out = shield

  end subroutine efc_unit_pass

  subroutine low(efc, reg, inout)
    type(effect), intent(inout)::efc
    real, intent(in)::reg(64)
    real, intent(inout)::inout

    real::o0, a, b0, b1, b2, a0, a1, a2, in

    o0 = data_real_a(reg, efc%p(1)) / 44100.0
    a = osc_sin(o0) / data_real_a(reg, efc%p(2))
    a0 = 1 + a
    b0 = (1.0 - osc_sin(0.25 + o0)) / (2.0 * a0)
    b1 = (1.0 - osc_sin(0.25 + o0)) / a0
    b2 = b0
    a1 = -2 * osc_sin(0.25 + o0) / a0
    a2 = (1.0 - a) / a0

    in = inout 
    inout = b0 * in + b1 * efc%data(1) + b2 * efc%data(2) - &
         a1 * efc%data(3) - a2 * efc%data(4)

    efc%data(2) = efc%data(1)  
    efc%data(1) = in
    efc%data(4) = efc%data(3)
    efc%data(3) = inout
  end subroutine low

  subroutine hig(efc, reg, inout)
    type(effect), intent(inout)::efc
    real, intent(in)::reg(64)
    real, intent(inout)::inout

    real::o0, a, b0, b1, b2, a0, a1, a2, in

    o0 = data_real_a(reg, efc%p(1)) / 44100.0
    a = osc_sin(o0) / data_real_a(reg, efc%p(2))
    a0 = 1 + a
    b0 = (1.0 + osc_sin(0.25 + o0)) / (2.0 * a0)
    b1 = - (1.0 + osc_sin(0.25 + o0)) / a0
    b2 = b0
    a1 = -2 * osc_sin(0.25 + o0) / a0
    a2 = (1.0 - a) / a0

    in = inout 
    inout = b0 * in + b1 * efc%data(1) + b2 * efc%data(2) - &
         a1 * efc%data(3) - a2 * efc%data(4)

    efc%data(2) = efc%data(1)  
    efc%data(1) = in
    efc%data(4) = efc%data(3)
    efc%data(3) = inout
  end subroutine hig

  subroutine bnd(efc, reg, inout)
    type(effect), intent(inout)::efc
    real, intent(in)::reg(64)
    real, intent(inout)::inout

    real::o0, a, b0, b1, b2, a0, a1, a2, in

    o0 = data_real_a(reg, efc%p(1)) / 44100.0
    a = osc_sin(o0) / data_real_a(reg, efc%p(2))
    a0 = 1 + a
    b0 = a / a0
    b1 = 0.0
    b2 = - b0
    a1 = -2 * osc_sin(0.25 + o0) / a0
    a2 = (1.0 - a) / a0

    in = inout 
    inout = b0 * in + b1 * efc%data(1) + b2 * efc%data(2) - &
         a1 * efc%data(3) - a2 * efc%data(4)

    efc%data(2) = efc%data(1)  
    efc%data(1) = in
    efc%data(4) = efc%data(3)
    efc%data(3) = inout
  end subroutine bnd

  
  subroutine dly(efc, reg, inout)
    type(effect), intent(inout)::efc
    real, intent(in)::reg(64)
    real, intent(inout)::inout

    integer::writer, reader

    writer = mod(int(efc%data(66151)), 66150) + 1
    reader = mod(writer - int(data_real_a(reg, efc%p(1))) + 66150&
         , 66150) + 1

    inout = (inout * data_real_a(reg, efc%p(2))) + &
         (efc%data(reader) * data_real_a(reg, efc%p(3)))
    efc%data(writer) = inout
    

    efc%data(66151) = writer
  end subroutine dly

  subroutine bit(efc, reg, inout)
    type(effect), intent(in)::efc
    real, intent(in)::reg(64)
    real, intent(inout)::inout

    real :: steps, bits_val

    bits_val = data_real_a(reg, efc%p(1))
    
    if (bits_val < 2.0) bits_val = 2.0
    steps = real(int(bits_val))

    inout = nint(inout * steps) / steps
  end subroutine bit

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
