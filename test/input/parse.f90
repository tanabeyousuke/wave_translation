module parse !パーサです。シンセサイザの設定や演奏の実行などを処理します。
  implicit none
     
  type::oscillator
     integer f!波形
     real g!オシレータのゲイン
  end type oscillator
 
  type::filter
     integer t!フィルタの種類
     real f!カットオフ周波数
     real r!レゾナンス
  end type filter
  
  type::envelope
     real env_unit(4)
     real out
  end type envelope
  
  type::lfo
     real lfo_unit(4)
  end type lfo
  
  type::setting
     type(oscillator), allocatable::osc(:)
     type(filter), allocatable::flt(:)
     type(envelope), allocatable::env(:)
     type(lfo), allocatable::lfo(:)
     integer osc_n
     integer flt_n
     integer env_n
     integer lfo_n
     real amp
  end type setting
  
contains
  function num_reg(op)
    character(*), intent(in)::op
    real num
    real num_reg

    if(op(1:1) == "r") then
       read(op(2:4), *) num
       num = num * (-1)
       num_reg = num
    else
       read(op, *) num
       num_reg = num
    end if
  end function num_reg

  subroutine get_token(line, head, tail, scpos)
    character(len=80),intent(in)::line
    integer,intent(in)::scpos
    integer,intent(inout)::head, tail
    
    integer ophead, optail, i

    do i = tail, scpos
       if(line(i:i) /= ' ') then
          ophead = i
          exit
       end if
    end do
    
    do i = ophead + 1, scpos
       if(line(i:i) == ' ' .or. line(i:i) == ';') then
          optail = i - 1
          
          exit
       end if
    end do

    head = ophead
    tail = optail
  end subroutine get_token

  subroutine module_num_setting(unit_num, set)
    integer,intent(in)::unit_num
    type(setting),intent(inout)::set

    character(len=80) line
    character(len=5) operate
    integer iostat_value, scpos, setpos, ophead, optail
    integer i, ro, rgx(10)
    logical lrgx(5)
    
    do
       read(unit_num, '(A:)', iostat=iostat_value) line
       
       scpos = index(line, ";")
       setpos = index(line, "setting:")
       
       if(setpos /= 0) exit
       
       if(scpos == 0) cycle

       optail = 1
       call get_token(line, ophead, optail, scpos)
       
       operate = trim(line(ophead:optail))

       select case(operate)
       case("osc")
          optail = optail + 1
          call get_token(line, ophead, optail, scpos)
          operate = line(ophead:optail)
          read(operate, *) set%osc_n

       case("flt")
          optail = optail + 1
          call get_token(line, ophead, optail, scpos)

          read(line(ophead:optail), *) set%flt_n

       case("env")
          optail = optail + 1
          call get_token(line, ophead, optail, scpos)

          read(line(ophead:optail), *) set%env_n

       case("lfo")
          optail = optail + 1
          call get_token(line, ophead, optail, scpos)

          read(line(ophead:optail), *) set%lfo_n
       end select
    end do

  end subroutine module_num_setting
  
  subroutine synth_setting(unit_num, set)
    integer, intent(in)::unit_num
    type(setting), intent(inout)::set
    
    character(len=80) line
    character(len=5) operate
    integer iostat_value, scpos, playpos, ophead, optail
    integer i, ro, osc_num, flt_num, env_num, lfo_num, rgx(5)
    logical lrgx(5)

    ophead = 0

    allocate(set%osc(set%osc_n))
    allocate(set%flt(set%flt_n))
    allocate(set%env(set%env_n))
    allocate(set%lfo(set%lfo_n))
    osc_num = 0
    flt_num = 0
    env_num = 0
    lfo_num = 0
    
    do
       read(unit_num, '(A:)', iostat=iostat_value)line
       
       playpos = index(line, "play:")
       if(playpos /= 0) exit
       
       scpos = index(line, ';')
       if(scpos == 0) cycle
       
       lrgx(2) = .false.
       
       optail = 1
       call get_token(line, ophead, optail, scpos)
       operate = line(ophead:optail)
       
       print *, operate
       select case(trim(operate))
       case("osc")
          osc_num = osc_num + 1

          optail = optail + 1
          call get_token(line, ophead, optail, scpos)
          
          operate = trim(line(ophead:optail))
          select case(operate)
          case("sin")
             rgx(1) = 1
          case("del")
             rgx(1) = 2
          case("sqr")
             rgx(1) = 3
          case("saw")
             rgx(1) = 4
          end select
          
          set%osc(osc_num)%f = rgx(1)

          optail = optail + 1
          call get_token(line, ophead, optail, scpos)
          operate = trim(line(ophead:optail))
          set%osc(osc_num)%g = num_reg(operate)

       case("flt")
          flt_num = flt_num + 1

          optail = optail + 1
          call get_token(line, ophead, optail, scpos)

          operate = trim(line(ophead:optail))
          select case(operate)
          case("low")
             rgx(1) = 1
          case("hig")
             rgx(1) = 2
          end select

          set%flt(flt_num)%t = rgx(1)

          optail = optail + 1
          call get_token(line, ophead, optail, scpos)
             
          operate = trim(line(ophead:optail))
          set%flt(flt_num)%f = num_reg(operate)

          optail = optail + 1
          call get_token(line, ophead, optail, scpos)
             
          operate = trim(line(ophead:optail))
          set%flt(flt_num)%r = num_reg(operate)
 
       case("env")
          optail = optail + 1
          call get_token(line, ophead, optail, scpos)
          
          operate = trim(line(ophead:optail))
          read(operate, *) rgx(1)
          if(env_num < rgx(1)) then
             env_num = env_num + 1 
          end if
          
          optail = optail + 1
          call get_token(line, ophead, optail, scpos)

          operate = trim(line(ophead:optail))

          select case(operate)
          case("atk")
             rgx(1) = 1
          case("dec")
             rgx(1) = 2
          case("sus")
             rgx(1) = 3
          case("rel")
             rgx(1) = 4
          case("out")
             rgx(1) = 5
          end select

          optail = optail + 1
          call get_token(line, ophead, optail, scpos)

          operate = trim(line(ophead:optail))

          if(rgx(1) < 5) then
             set%env(env_num)%env_unit(rgx(1)) = num_reg(operate)
          else
             set%env(env_num)%out = num_reg(operate)
          end if
           
       case("lfo")
          lfo_num = lfo_num + 1

          optail = optail + 1
          call get_token(line, ophead, optail, scpos)
          
          operate = trim(line(ophead:optail))

          select case(operate)
          case("sin")
             rgx(1) = 1
          case("del")
             rgx(1) = 2
          case("saw")
             rgx(1) = 3
          case("sqr")
             rgx(1) = 4
          case("rnd")
             rgx(1) = 5
          end select
          
          set%lfo(lfo_num)%lfo_unit(1) = rgx(1)

          do i = 2, 4
             optail = optail + 1
             call get_token(line, ophead, optail, scpos)
             operate = line(ophead:optail)

             set%lfo(lfo_num)%lfo_unit(i) = num_reg(operate)
          end do
          
       case("amp")
          optail = optail + 1
          call get_token(line, ophead, optail, scpos)
          operate = line(ophead:optail)
          
          set%amp = num_reg(operate)
       end select
    end do
  end subroutine synth_setting
  
  subroutine execute(filename, unit_num)
    character(*),intent(in)::filename
    integer unit_num

    character(len=80) line, mnemonic
    integer  iostat_value, scpos, setpos
    type(setting) set

    open(unit=unit_num, file=filename, status='OLD', iostat=iostat_value)
    if(iostat_value /= 0)then
       print *, "error"
       stop
    end if

    do
       read(unit_num, '(A:)', iostat=iostat_value) line
       if(iostat_value /= 0) exit
       ! print *, line

       setpos = index(line, "module_num:")
       if(setpos /= 0) exit
       
    end do
    
    call module_num_setting(unit_num, set)

    call synth_setting(unit_num, set)
    
    close(unit_num)
  end subroutine execute

end module parse
