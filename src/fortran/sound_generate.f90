module sound_generate
  use c_if
  use env
  use osc
  use parse
  implicit none

contains
  subroutine play(mt, msc)
    type(c_ptr),intent(in)::mt
    type(music),intent(inout)::msc

    real(c_float),pointer::input(:)
    real,allocatable,dimension(:)::buf
    integer::len,i,i1
    logical::flag

    call c_f_pointer(bp(mt), input, SHAPE=[4410])

    allocate(buf(len))

    msc%synth%slc=.false.
    flag = .false.
    do
       do i = 1, 50
          input(:) = 0
          do i1 = 1, size(msc%synth)
             input(:) = input(:) + msc%synth(i1)%buffer(((i - 1) * 4410) + 1:i * 4410) / size(msc%synth)
          end do
          call sound_write(mt)
       end do
    
       call music_generate(msc)
       
    end do
  end subroutine play

  subroutine music_generate(msc)
    type(music),intent(inout)::msc

    integer::i, i1

    do i = 1, size(msc%synth)
       call fill_buffer(msc%synth(i))
    end do
  end subroutine music_generate
    
  subroutine fill_buffer(set)
    type(setting)::set

    character(len=80)::line
    character(len=5)::operate
    integer::unit_num, iostat_value, scpos, endpos, ophead, optail
    integer::i, space, rgx(5)

    unit_num = set%unit_num
    set%buffer(:) = 0

    set%writed = .false.
    set%space = 0
    do
       if(set%rest /= 0)then
          call rest(set)
       end if

      if(set%writed .eqv. .true.)then
         exit
      end if

       read(unit_num, '(A:)', iostat=iostat_value) line
       endpos = index(line, "end:")
       if(iostat_value /= 0 .or. endpos /= 0)then
          set%slc = .true.
          exit
       end if

       do i = 1, len(line)
          if (line(i:i) == char(9)) line(i:i) = ' '
       end do

       scpos = index(line, ';')
       if(scpos == 0) cycle

       optail = 1
       call get_token(line, ophead, optail, scpos)
       operate = line(ophead:optail)
       
       select case(trim(operate))
       case("mov")
          optail = optail + 1
          call get_token(line, ophead, optail, scpos)
          operate = trim(line(ophead:optail))
          read(operate(2:4), *) rgx(1)

          optail = optail + 1
          call get_token(line, ophead, optail, scpos)
          operate = trim(line(ophead:optail))
          print *, operate
          read(operate, *) set%reg(rgx(1))

       case("kon")
          optail = optail + 1
          call get_token(line, ophead, optail, scpos)
          operate = trim(line(ophead:optail))

          read(operate, *) rgx(1)

          optail = optail + 1
          call get_token(line, ophead, optail, scpos)
          operate = trim(line(ophead:optail))

          select case(operate)
          case("c")
             rgx(2) = -9
          case("cis")
             rgx(2) = -8
          case("d")
             rgx(2) = -7
          case("dis")
             rgx(2) = -6
          case("e")
             rgx(2) = -5
          case("f")
             rgx(2) = -4
          case("fis")
             rgx(2) = -3
          case("g")
             rgx(2) = -2
          case("gis")
             rgx(2) = -1
          case("a")
             rgx(2) = 0
          case("ais")
             rgx(2) = 1
          case("h")
             rgx(2) = 2
          end select

          set%vce(rgx(1))%pn = rgx(2)

          optail = optail + 1
          call get_token(line, ophead, optail, scpos)
          operate = trim(line(ophead:optail))
          
          read(operate, *) set%vce(rgx(1))%oct
          
          set%vce(rgx(1))%count = 0
          set%vce(rgx(1))%play = .true.
          set%vce(rgx(1))%push = .true.
          
       case("kof")
          optail = optail + 1
          call get_token(line, ophead, optail, scpos)
          operate = trim(line(ophead:optail))

          read(operate, *) rgx(1)

          set%vce(rgx(1))%push = .false.

       case("rst")
          
          optail = optail + 1
          call get_token(line, ophead, optail, scpos)
          operate = trim(line(ophead:optail))

          read(operate, *) rgx(1)
          set%rest = rgx(1)
      end select

    end do

  end subroutine fill_buffer

  subroutine rest(set)
    type(setting),intent(inout)::set

    integer::i, i1, i2
    real::data
    real,allocatable::f(:)
    
    allocate(f(set%vce_num))

    do i1 = 1, set%vce_num
       
    end do

    do i = 1, set%rest
       
    end do


  end subroutine rest
  
end module sound_generate
