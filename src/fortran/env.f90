module env
  implicit none

contains
  function env_out(atk, dec, sus, rel, time, last, push)
    real,intent(in)::atk,dec,sus,rel
    integer,intent(in)::time,last
    logical,intent(in)::push

    real::env_out

    real::m
    integer::count

    if(push .eqv. .false.) then
       count = time - last
       m = 0 - (sus / rel)
       env_out = m * count + sus
    else if(time < atk) then
       count = time
       m = 1 / atk
       env_out = m * count
    else if(time - atk < dec) then
       count = time - atk
       m = 1 - ((1.0 - sus) / dec)
       env_out = m * count + 1
    else
       env_out = sus
    end if

  end function env_out

end module env
