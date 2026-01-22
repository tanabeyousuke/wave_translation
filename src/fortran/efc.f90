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
  
  subroutine svf_process(input, cutoff, q_res, low, band, high)
    real, intent(in)  :: input, cutoff, q_res
    real, intent(inout) :: low, band
    real, intent(out) :: high
    
    real :: f, q
    
    ! パラメータの変換 (簡易的な近似)
    ! f は 2 * sin(pi * cutoff / sample_rate) に相当
    f = cutoff 
    q = 1.0d0 / q_res
    
    ! フィルタ計算（1サンプル分）
    high = input - low - (q * band)
        band = band + (f * high)
        low  = low  + (f * band)

    end subroutine svf_process

end module efc
