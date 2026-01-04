module osc
  implicit none
contains
  function osc_sin(x)
    real,intent(in)::x
    real osc_sin

    osc_sin = sin((2 * 3.14159265) * (x - aint(x)))
  end function osc_sin

  function osc_del(x)
    real, intent(in)::x
    real osc_del

    real i
    
    i = x - aint(x)

    if(i < 0.25)then
       osc_del = 4 * i
    else if(i < 0.75) then
       osc_del = -4 * i + 2
    else
       osc_del = 4 * i - 4
    end if
  end function osc_del

  function osc_saw(x)
    real, intent(in)::x
    real osc_saw

    real i

    i = x - aint(x)

    osc_saw = i * 2 - 1
  end function osc_saw
  
  function osc_sqr(x)
    real, intent(in)::x
    real osc_sqr

    real i

    i = x - aint(x)

    if(i < 0.5)then
       osc_sqr = 1
    else
       osc_sqr = -1
    end if
  end function osc_sqr

end module osc
