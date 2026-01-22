module env
  implicit none

contains
  function env_out(atk, dec, sus, rel, time, last, push)
    real,intent(in)::atk,dec,sus,rel
    integer,intent(in)::time,last
    logical,intent(in)::push

    real::env_out

    real::m, x
    integer::count

    if(push .eqv. .false.) then
       count = time - last
       m = 0 - (sus / rel)
       x = m * count + sus
       if (x < 0) x = 0
       env_out = x
    else if(time < atk) then
       count = time
       m = 1 / atk
       env_out = m * count
    else if(time - atk < dec) then
       count = time - atk
       m = (sus - 1.0) / dec
       env_out = m * count + 1
    else
       env_out = sus
    end if
    
  end function env_out


  subroutine svf_process(input, cutoff, q_res, low, band, high)
        double precision, intent(in)  :: input, cutoff, q_res
        double precision, intent(inout) :: low, band
        double precision, intent(out) :: high
        
        double precision :: f, q

        ! パラメータの変換 (簡易的な近似)
        ! f は 2 * sin(pi * cutoff / sample_rate) に相当
        f = cutoff 
        q = 1.0d0 / q_res

        ! フィルタ計算（1サンプル分）
        high = input - low - (q * band)
        band = band + (f * high)
        low  = low  + (f * band)

    end subroutine svf_process

end module svf_module

end module env
