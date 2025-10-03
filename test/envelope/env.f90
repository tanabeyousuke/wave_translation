module env
  implicit none

  type::env_data
     real atk
     real dec
     real sus
     real rel
  end type env_data
     
contains

  function env_out(data, x, push)
    type(env_data),intent(in)::data
    integer, intent(in)::push, x
    real env_out

    integer phase, pt
    real rgx(2)

    if(push < x)then
       pt = x - push 
       if(pt < data%rel)then
          phase = 4
       else
          phase = 5
       end if
    else
       if(x < data%atk)then
          phase = 1
          pt = x
       else if(x < (data%dec + data%atk)) then
          phase = 2
          pt = x - data%atk
       else
          phase = 3
          pt = 0
       end if
    end if

    select case(phase)
    case(1)
       rgx(1) = (100 / data%atk) * pt
    case(2)
       rgx(1) = -((100 - data%sus) / data%dec) * pt + 100
    case(3)
       rgx(1) = data%sus
    case(4)
       rgx(1) = -(data%sus / data%rel) * pt + data%sus
    case(5)
       rgx(1) = 0
    end select

    env_out = rgx(1)
    
  end function env_out
end module env
