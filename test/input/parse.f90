module parse !パーサです。シンセサイザの設定や演奏の実行などを処理します。
  implicit none
     
  type::oscillator
     integer,allocatable::f(:)!波形
     real,allocatable::g(:)!オシレータのゲイン
     integer n
  end type oscillator
 
  type::filter
     integer, allocatable::t(:)!フィルタの種類
     real, allocatable::f(:)!カットオフ周波数
     real, allocatable::r(:)!レゾナンス
     integer n
  end type filter
  
  type::env
     real env_unit(4)
     real out
  end type env
  
  type::lfo
     real lfo_unit(3)
     real out
  end type lfo
  
  type::setting
     type(oscillator) osc
     type(filter) flt
     type(env), allocatable::envelope(:)
     type(lfo), allocatable::lfo(:)
     integer osc_n
     integer flt_n
     integer env_n
     integer lfo_n
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
  
  subroutine module_num_set(unit_num, set)
    integer unit_num
    type(setting) set

    character(len=80) line
    character(len=5) operate
    integer iostat_value, scpos, setpos, ophead, optail
    integer i, ro, rgx(5)
    logical lrgx(5)
    do
       read(unit_num, '(A:)', iostat=iostat_value) line
       
       scpos = index(line, ";")
       setpos = index(line, "setting:")
       
       if(setpos < scpos) exit
       
       if(setpos == 0) cycle
       
       do i = 1, scpos
          if(line(i:i) /= ' ') then
             ophead = i
             exit
          end if
       end do
       
       do i = ophead, scpos
          lrgx(1) = line(i:i) == ' ' .or. line(i:i) == ';'
          if( lrgx(1)) then
             optail = i - 1
             exit
          end if
       end do
       
       operate = trim(line(ophead:optail))
       select case(operate)
       case("osc")
          do i = optail + 1, scpos
             if(line(i:i) /= ' ') then
                ophead = i
                exit
             end if
          end do
          
          do i = ophead, scpos
             lrgx(1) = line(i:i) == ' ' .or. line(i:i) == ';'
             if( lrgx(1)) then
                optail = i - 1
                exit
             end if
          end do
          
          read(line(ophead:optail), *) set%osc_n

       case("flt")
          do i = optail + 1, scpos
             if(line(i:i) /= ' ') then
                ophead = i
                exit
             end if
          end do
          
          do i = ophead, scpos
             lrgx(1) = line(i:i) == ' ' .or. line(i:i) == ';'
             if( lrgx(1)) then
                optail = i - 1
                exit
             end if
          end do

          read(line(ophead:optail), *) set%flt_n

       case("env")
          do i = optail + 1, scpos
             if(line(i:i) /= ' ') then
                ophead = i
                exit
             end if
          end do
          
          do i = ophead, scpos
             lrgx(1) = line(i:i) == ' ' .or. line(i:i) == ';'
             if( lrgx(1)) then
                optail = i - 1
                exit
             end if
          end do
          
          read(line(ophead:optail), *) set%env_n

       case("lfo")
          do i = optail + 1, scpos
             if(line(i:i) /= ' ') then
                ophead = i
                exit
             end if
          end do
          
          do i = ophead, scpos
             lrgx(1) = line(i:i) == ' ' .or. line(i:i) == ';'
             if( lrgx(1)) then
                optail = i - 1
                exit
             end if
          end do

          read(line(ophead:optail), *) set%lfo_n
       end select
    end do
  end subroutine module_num_set
  
  subroutine synth_setting(unit_num, set)
    integer unit_num
    type(setting) set
    
    character(len=80) line
    character(len=5) operate
    integer iostat_value, scpos, playpos, ophead, optail
    integer i, ro, rgx(5)
    logical lrgx(5)

    set%osc_n = 0
    set%flt_n = 0
    set%env_n = 0
    set%lfo_n = 0

    ophead = 0

    do
       read(unit_num, '(A:)', iostat=iostat_value)line
    
       playpos = index(line, "play:")
       if(playpos /= 0) exit

       scpos = index(line, ';')
       if(scpos == 0) cycle

       lrgx(2) = .false.

       operate = line(ophead:optail)
       select case(trim(operate))
       case("osc")
          rgx(1) = set%osc%n + 1
          set%osc%n = rgx(1)
          if (set%osc%n - 1 /= 0) then
             call reallocate_integer(set%osc%f, rgx(1))
             call reallocate_real(set%osc%g, rgx(1))
          else
             allocate(set%osc%f(rgx(1)))
             allocate(set%osc%g(rgx(1)))
          end if
          
          do i = optail + 1, scpos
             if(line(i:i) /= ' ' .and. (lrgx(2) .eqv. .false.)) then
                ophead = i
                exit
             end if
          end do
          
          do i = ophead, scpos
             lrgx(1) = line(i:i) == ' ' .or. line(i:i) == ';'
             if( lrgx(1)) then
                optail = i - 1
                exit
             end if
          end do

          operate = trim(line(ophead:optail))
          select case(operate)
          case("sin")
             rgx(2) = 1
          case("del")
             rgx(2) = 2
          case("sqr")
             rgx(2) = 3
          case("saw")
             rgx(2) = 4
          end select
          
          set%osc%f(rgx(1)) = rgx(2)

          do i = optail + 1, scpos
             if(line(i:i) /= ' ' .and. (lrgx(2) .eqv. .false.)) then
                ophead = i
                exit
             end if
          end do
          
          do i = ophead, scpos
             lrgx(1) = line(i:i) == ' ' .or. line(i:i) == ';'
             if( lrgx(1)) then
                optail = i - 1
                exit
             end if
          end do

          operate = trim(line(ophead:optail))
          set%osc%g(rgx(1)) = num_reg(operate)
          ! print *, set%osc%g(rgx(1)), set%osc%g(rgx(1) - 1)

       case("flt")
          rgx(1) = set%flt%n + 1
          set%flt%n = rgx(1)
          if (rgx(1) -  1 /= 0) then
             call reallocate_integer(set%flt%t, rgx(1))
             call reallocate_real(set%flt%f, rgx(1))
             call reallocate_real(set%flt%r, rgx(1))
          else
             allocate(set%flt%t(rgx(1)))
             allocate(set%flt%f(rgx(1)))
             allocate(set%flt%r(rgx(1)))
          end if

          do i = optail + 1, scpos
             if(line(i:i) /= ' ' .and. (lrgx(2) .eqv. .false.)) then
                ophead = i
                exit
             end if
          end do
          
          do i = ophead, scpos
             lrgx(1) = line(i:i) == ' ' .or. line(i:i) == ';'
             if( lrgx(1)) then
                optail = i - 1
                exit
             end if
          end do

          operate = trim(line(ophead:optail))
          select case(operate)
          case("low")
             rgx(2) = 1
          case("hig")
             rgx(2) = 2
          end select

          set%flt%t(rgx(1)) = rgx(2)

          do i = optail + 1, scpos
             if(line(i:i) /= ' ' .and. (lrgx(2) .eqv. .false.)) then
                ophead = i
                exit
             end if
          end do
          
          do i = ophead, scpos
             lrgx(1) = line(i:i) == ' ' .or. line(i:i) == ';'
             if( lrgx(1)) then
                optail = i - 1
                exit
             end if
          end do
             
          operate = trim(line(ophead:optail))
          set%flt%f(rgx(1)) = num_reg(operate)

          do i = optail + 1, scpos
             if(line(i:i) /= ' ' .and. (lrgx(2) .eqv. .false.)) then
                ophead = i
                exit
             end if
          end do
          
          do i = ophead, scpos
             lrgx(1) = line(i:i) == ' ' .or. line(i:i) == ';'
             if( lrgx(1)) then
                optail = i - 1
                exit
             end if
          end do
             
          operate = trim(line(ophead:optail))
          set%flt%r(rgx(1)) = num_reg(operate)
          

       ! case("env")
       !     do i = optail + 1, scpos
       !       if(line(i:i) /= ' ' .and. (lrgx(2) .eqv. .false.)) then
       !          ophead = i
       !          exit
       !       end if
       !    end do
          
       !    do i = ophead, scpos
       !       lrgx(1) = line(i:i) == ' ' .or. line(i:i) == ';'
       !       if( lrgx(1)) then
       !          optail = i - 1
       !          exit
       !       end if
       !    end do

       !    operate = trim(line(ophead:optail))
       !    read(operate, *) rgx(2)
       !    rgx(1) = set%env_num
       !    if(rgx(1) < rgx(2)) then
       !       set%env_num = rgx(1) + 1
       !       rgx(1) = set%env_num
       !       if (allocated(set%oscillator%list)) then
       !          call reallocate_env(set%envelope, rgx(1))
       !       else
       !          allocate(set%envelope(rgx(1)))
       !       end if
       !    end if

       !    do i = optail + 1, scpos
       !       if(line(i:i) /= ' ' .and. (lrgx(2) .eqv. .false.)) then
       !          ophead = i
       !          exit
       !       end if
       !    end do
          
       !    do i = ophead, scpos
       !       lrgx(1) = line(i:i) == ' ' .or. line(i:i) == ';'
       !       if( lrgx(1)) then
       !          optail = i - 1
       !          exit
       !       end if
       !    end do

       !    operate = trim(line(ophead:optail))

       !    select case(operate)
       !    case("atk")
       !       rgx(2) = 1
       !    case("dec")
       !       rgx(2) = 2
       !    case("sus")
       !       rgx(2) = 3
       !    case("rel")
       !       rgx(2) = 4
       !    case("out")
       !       rgx(2) = 5
       !    end select

       !    do i = optail + 1, scpos
       !       if(line(i:i) /= ' ' .and. (lrgx(2) .eqv. .false.)) then
       !          ophead = i
       !          exit
       !       end if
       !    end do
          
       !    do i = ophead, scpos
       !       lrgx(1) = line(i:i) == ' ' .or. line(i:i) == ';'
       !       if( lrgx(1)) then
       !          optail = i - 1
       !          exit
       !       end if
       !    end do

       !    operate = trim(line(ophead:optail))

       !    if(rgx(2) /= 5) then
       !       set%envelope(rgx(1))%env_unit(rgx(2)) = num_reg(operate)
       !    else
       !       set%envelope(rgx(1))%out = num_reg(operate)
       !    end if
           
       ! case("lfo")
       !    rgx(1) = set%lfo_num + 1
       !    set%lfo_num = rgx(1)
       !    if (allocated(set%lfo)) then
       !       call reallocate_lfo(set%lfo, rgx(1))
       !    else
       !       allocate(set%lfo(rgx(1)))
       !    end if

       !    do i = optail + 1, scpos
       !       if(line(i:i) /= ' ' .and. (lrgx(2) .eqv. .false.)) then
       !          ophead = i
       !          exit
       !       end if
       !    end do
          
       !    do i = ophead, scpos
       !       lrgx(1) = line(i:i) == ' ' .or. line(i:i) == ';'
       !       if( lrgx(1)) then
       !          optail = i - 1
       !          exit
       !       end if
       !    end do
          
       !    operate = trim(line(ophead:optail))

       !    select case(operate)
       !    case("sin")
       !       rgx(2) = 1
       !    case("del")
       !       rgx(2) = 2
       !    case("saw")
       !       rgx(2) = 3
       !    case("sqr")
       !       rgx(2) = 4
       !    case("rnd")
       !       rgx(2) = 5
       !    end select
          
       !    set%lfo(rgx(1))%lfo_unit(1) = rgx(2)

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

    do
       read(unit_num, '(A:)', iostat=iostat_value) line
       if(iostat_value /= 0) exit
       ! print *, line

       setpos = index(line, "setting:")
       if(setpos /= 0) exit
    end do

    call synth_setting(unit_num, set)
    
    close(unit_num)
  end subroutine execute


end module parse
