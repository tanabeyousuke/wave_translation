module parse !パーサです。シンセサイザの設定や演奏の実行などを処理します。
  implicit none

  type::param
     real::value
     integer::reg_num
     
     logical::rorv
  end type param
  
  type::lowfrequency
     integer::form !波形
     type(param)::p(4) !周波数、振幅、オフセット、出力
  end type lowfrequency

  type::effect
     integer::type
     type(param)::p(5)
     real,allocatable::array(:)
  end type effect
  
  type::oscillator
     integer::type !sin, del, sqr, saw, rnd
     type(param)::p(2) !ピッチ倍率、ゲイン
  end type oscillator
  
  type::envelope
     type(param)::p(7)!オフセット、最大値、atk, dec, sus, rel, 出力
  end type envelope

  type::setting
     type(oscillator),allocatable::osc(:)
     type(envelope),allocatable::env(:)
     type(lowfrequency),allocatable::lfo(:) !ゲインにつなぐLFOのパラメータ 周波数、振幅、オフセット、出力
     type(effect),allocatable::efc(:) !エフェクト 
     type(param)::amp
  end type setting
  
contains
  subroutine num_reg(op, p)!レジスタ番号を実数に直します。
    character(*), intent(in)::op
    type(param),intent(inout)::p

    if(op(1:1) == "r") then
       read(op(2:4), *) p%reg_num
       p%rorv = .true.
    else
       read(op, *) p%value
       p%rorv = .false.
    end if
  end subroutine num_reg
 
  subroutine get_token(line, head, tail, scpos) !スペースで囲まれた部分の先端と後端を取り出します。実際の使い方は下に
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

  subroutine synth_setting(unit_num, set)
    integer, intent(in)::unit_num
    type(setting), intent(inout)::set
    
    character(len=80) line
    character(len=5) operate
    integer iostat_value, scpos, playpos, ophead, optail
    integer i, ro, rgx(5)
    logical lrgx(5)
    type(oscillator)::osc
    type(envelope)::env
    type(lowfrequency)::lfo !ゲインにつなぐLFOのパラメータ 周波数、振幅、オフセット、出力
    type(effect)::efc !エフェクト 

    ophead = 0

    allocate(set%osc(0))
    allocate(set%env(0))
    allocate(set%lfo(0))
    allocate(set%efc(0))
    
    do
       read(unit_num, '(A:)', iostat=iostat_value)line
       do i = 1, len(line)
          if (line(i:i) == char(9)) line(i:i) = ' '
       end do
       
       playpos = index(line, "play:")
       if(playpos /= 0) exit
       
       scpos = index(line, ';')
       if(scpos == 0) cycle
       
       lrgx(2) = .false.
       
       optail = 1
       call get_token(line, ophead, optail, scpos)
       operate = line(ophead:optail)
       
       select case(trim(operate))
       case("osc")
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
          case("rnd")
             rgx(1) = 5
          end select

          osc%type=rgx(1)

          do i = 1, 2
             optail = optail + 1
             call get_token(line, ophead, optail, scpos)
             operate = line(ophead:optail)

             call num_reg(operate, osc%p(i))
          end do
          
          set%osc = [set%osc, osc]

       case("env")
          do i = 1, 7
             optail = optail + 1
             call get_token(line, ophead, optail, scpos)
             operate = line(ophead:optail)

             call num_reg(operate, env%p(i))
          end do
          
          set%env = [set%env, env]

       case("lfo")
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
          
          lfo%form = rgx(1)

          do i = 1, 3
             optail = optail + 1
             call get_token(line, ophead, optail, scpos)
             operate = line(ophead:optail)

             call num_reg(operate, lfo%p(i))
          end do
          set%lfo = [set%lfo, lfo]

       case("efc");
          optail = optail + 1
          call get_token(line, ophead, optail, scpos)

          operate = trim(line(ophead:optail))

          select case(operate)
          case("low")
             rgx(1) = 1
          case("hig")
             rgx(1) = 2
          case("bnd")
             rgx(1) = 3
          case("dly")
             rgx(1) = 4
          case("bit")
             rgx(1) = 5
          case("com")
             rgx(1) = 6
          case("hcl")
             rgx(1) = 7
          case("scl")
             rgx(1) = 8
          case("fbc")
             rgx(1) = 9
          end select

          efc%type = rgx(1)

          do i = 1, 5
             optail = optail + 1
             call get_token(line, ophead, optail, scpos)
             operate = line(ophead:optail)

             call num_reg(operate, efc%p(i))
             if(scpos - optail < 2)then
                exit
             end if
          end do
          set%efc = [set%efc, efc]

       case("amp")
          optail = optail + 1
          call get_token(line, ophead, optail, scpos)
          operate = line(ophead:optail)
          
          call num_reg(operate, set%amp) 
       end select
    end do
  end subroutine synth_setting
  
  subroutine execute(filename, unit_num, set)
    character(*),intent(in)::filename
    integer unit_num
    type(setting),intent(inout)::set

    character(len=80) line, mnemonic
    integer  iostat_value, scpos, modpos

    open(unit=unit_num, file=filename, status='OLD', iostat=iostat_value)
    if(iostat_value /= 0)then
       print *, "error"
       stop
    end if

    call synth_setting(unit_num, set)
    
    close(unit_num)

  end subroutine execute

end module parse
