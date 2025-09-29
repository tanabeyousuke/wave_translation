module env
  implicit none

  type::env_data
     real atk
     real dec
     real sus
     real rel
     real start
     real last
     integer phase
  end type env_data
     
contains

  function env_out(data, push)
    type(env_data),intent(inout)::data
    integer, intent(in)::push
    real,intent(in)::x
    real env_out

    real rgx(2) 
    
    if(push == 1)then
       data%start = data%start + 1
    else
       data%last = data%last + 1
    end if
    
    select case(data%phase)
    case(0)
       if (push == 1) then
          data%phase = 1
          env_out = 0
       end if
    case(1)
       if(atk > data%start) data%phase = 2
       env_out = (100 * data%start) / data%atk
    case(2)
       if(dec > data%start) data%phase = 3
       rgx(1) = (data%sus - 100) / data%dec
       env_out = rgx(1) * (data%start - data%atk) + 100
    case(3)
       if(push == 0) then
          data%phase = 4
          data%last = 0
       end if
       env_out = data%sus
    case(4)
       if(push == 1) then
          data%phase = 1
          data%start = 0
       end if
       rgx(1) = data%sus / data%rel * -1
       rgx(2) = rgx(1) * data%last
       if(rgx(2) <= 0) then
          data%phase = 0
       end if
       env_out = rgx(2)
    end select
  end function env_out
end module env
