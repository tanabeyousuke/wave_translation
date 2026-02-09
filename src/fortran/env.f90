module env
  use parse
  implicit none

contains
  function env_out(env, time, last, push, reg)
    type(envelope),intent(in)::env
    integer,intent(in)::time,last
    logical,intent(in)::push
    real,intent(in)::reg(64)

    real::env_out

    real::m, x
    integer::count

    if(push .eqv. .false.) then
       count = time - last
       m = 0 - (data_real(env%p(5)) / data_real(env%p(6)))
       x = m * count + data_real(env%p(5))
       if (x < 0) x = 0
       env_out = x + data_real(env%p(1))
    else if(time < data_real(env%p(3))) then
       count = time
       m = data_real(env%p(2)) / data_real(env%p(3)) 
       env_out = m * count
    else if(time - data_real(env%p(3)) < data_real(env%p(4))) then
       count = time - data_real(env%p(3))
       m = (data_real(env%p(5)) - 1.0) / data_real(env%p(4))
       env_out = m * count + data_real(env%p(2))
    else
       env_out = data_real(env%p(5))
    end if

  end function env_out
end module env
