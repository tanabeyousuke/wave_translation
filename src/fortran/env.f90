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
       if(data%phase == 4 .or. data%phase == 0) data%phase = 1
    end if

    select case(data%phase)
    case(1)
       if(atk > x) data%phase = 2
       env_out = (100 * x) / data%atk
    case(2)
       if(dec > x) data%phase = 3
       rgx(1) = (data%sus - 100) / data%dec
       env_out = rgx(1) * (x - data%atk) + 100
    case(3)
       if(push == 0) data%phase = 4
       env_out = data%sus
    case(4)
       if(push == 1) data%phase = 1

       rgx(1) = data%sus / data&rel
       rgx(2) = rgx(1) * (x - 
