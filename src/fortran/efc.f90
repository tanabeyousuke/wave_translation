module efc
  use parse

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
          call svf(efc(i), reg, 1, shield, shield)
       case(2)
          call svf(efc(i), reg, 2, shield, shield)
       case(3)
          call svf(efc(i), reg, 3, shield, shield)
       end select
    end do
    out = shield

  end subroutine efc_unit_pass

  subroutine svf(efc, reg, slc, in, out)
    type(effect), intent(inout)::efc
    real, intent(in)::reg(64)
    integer, intent(in)::slc
    real, intent(in)::in
    real, intent(out)::out

    real::f_p, q_p, f, q

    f = data_real_a(reg, efc%p(1))
    q = data_real_a(reg, efc%p(2))

    f_p = 2.0 * sin(3.14159265 * f / 44100)
    
    q_p = 1.0 / q
    
    efc%data(1) = efc%data(3) + efc%data(1) * efc%data(5)
    efc%data(2) = in - efc%data(3) - efc%data(2) * efc%data(5)
    efc%data(3) = efc%data(1) * efc%data(4) + efc%data(5)


    out = efc%data(slc)

  end subroutine svf

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
