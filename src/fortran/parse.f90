module parse !パーサです。シンセサイザの設定や演奏の実行などを処理します。
  implicit none

  type::param
     real::value
     integer::reg_num
     
     logical::rorv
  end type param

  type::voice
     logical::play
     logical::push
     integer::count
     integer::pn
     integer::oct
  end type voice

  type::lfo
     integer::form !波形
     type(param)::p(4) !周波数、振幅、オフセット、出力
  end type lfo

  type::effect
     integer::type
     type(param)::p(5)
     real,allocatable::array(:)
  end type effect
  
  type::setting
     type(param)::osc_g(5) !オシレータのゲイン 1から順にsin、三角、矩形、鋸、ノイズ
     type(param)::env(5) !エンベロープのパラメータ 1から順にatk,dec,sus,rel,出力
     type(lfo),allocatable::lfo(:) !ゲインにつなぐLFOのパラメータ 周波数、振幅、オフセット、出力
     type(effect),allocatable::efc(:) !エフェクト 
     integer::lfo_num
     integer::efc_num
     integer::vce_num

     type(param)::amp

     type(voice),allocatable::vce(:)
     
     integer::unit_num 
     real::buffer(220500)
     real::reg(64)
     integer::space
     integer::rest
     logical::writed
     logical::slc

  end type setting

  type music
     type(setting), allocatable::synth(:)
  end type music
  
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
       do i = 1, len(line)
          if (line(i:i) == char(9)) line(i:i) = ' '
       end do
       
       setpos = index(line, "setting:")
       if(setpos /= 0) exit
       
       scpos = index(line, ';')
       if(scpos == 0) cycle
       

       optail = 1
       call get_token(line, ophead, optail, scpos)
       
       operate = trim(line(ophead:optail))

       select case(operate)
       case("lfo")
          optail = optail + 1
          call get_token(line, ophead, optail, scpos)

          read(line(ophead:optail), *) set%lfo_num
       case("efc")
          optail = optail + 1
          call get_token(line, ophead, optail, scpos)

          read(line(ophead:optail), *) set%efc_num
       case("vce")
          optail = optail + 1
          call get_token(line, ophead, optail, scpos)

          read(line(ophead:optail), *) set%efc_num
       end select
    end do
  end subroutine module_num_setting
  
  subroutine synth_setting(unit_num, set)
    integer, intent(in)::unit_num
    type(setting), intent(inout)::set
    
    character(len=80) line
    character(len=5) operate
    integer iostat_value, scpos, playpos, ophead, optail
    integer i, ro, lfo_num, efc_num, rgx(5)
    logical lrgx(5)

    ophead = 0

    allocate(set%lfo(set%lfo_num))
    allocate(set%efc(set%efc_num))
    allocate(set%vce(set%vce_num))
    lfo_num = 0
    efc_num = 0

    do i = 1, 5
       set%osc_g(i)%value = 0
       set%osc_g(i)%rorv = .false.
    end do

    do i = 1, 5
       set%env(i)%value = 0
       set%env(i)%rorv = .false.
    end do
      
    
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

          optail = optail + 1
          call get_token(line, ophead, optail, scpos)
          operate = trim(line(ophead:optail))
          call num_reg(operate, set%osc_g(rgx(1)))

       case("env")
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

          call num_reg(operate, set%env(rgx(1)))
           
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
          
          set%lfo(lfo_num)%form = rgx(1)

          do i = 1, 3
             optail = optail + 1
             call get_token(line, ophead, optail, scpos)
             operate = line(ophead:optail)

             call num_reg(operate, set%lfo(lfo_num)%p(i))
          end do

       case("efc");
          efc_num = efc_num + 1
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

          set%efc(efc_num)%type = rgx(1)

          do i = 1, 5
             optail = optail + 1
             call get_token(line, ophead, optail, scpos)
             operate = line(ophead:optail)

             call num_reg(operate, set%efc(efc_num)%p(i))
             if(scpos - optail < 2)then
                exit
             end if
          end do

       case("amp")
          optail = optail + 1
          call get_token(line, ophead, optail, scpos)
          operate = line(ophead:optail)
          
          call num_reg(operate, set%amp) 
       end select
    end do
  end subroutine synth_setting
  
  subroutine execute(filename, unit_num)
    character(*),intent(in)::filename
    integer unit_num

    character(len=80) line, mnemonic
    integer  iostat_value, scpos, modpos
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

       modpos = index(line, "module_num:")
       if(modpos /= 0) exit
       
    end do
    
    call module_num_setting(unit_num, set)

    call synth_setting(unit_num, set)
    
    close(unit_num)

  end subroutine execute
  subroutine setup_music(filename, m)
    character(*),intent(in)::filename
    type(music),intent(inout)::m

    integer unit_num, synth_unit_num, iostat_value, synth_iostat_value, scpos, num, i
    character(len=80) line, file

    unit_num = 10
    open(unit=unit_num, file=filename, status='OLD', iostat=iostat_value)
    if(iostat_value /= 0)then
       print *, "error: music file open failed"
       stop
    end if

    read(unit_num, '(A:)', iostat=iostat_value) line
    read(line, *) num
    allocate(m%synth(num))
    
    do i = 1, num
       read(unit_num, '(A:)', iostat=iostat_value) line
       
       synth_unit_num = 10 + i
       open(unit=synth_unit_num, file=line, status='OLD', iostat=synth_iostat_value)
       if(iostat_value /= 0)then
          print *, "error: ", line, "open_failed"
          stop
       end if
       
       m%synth(i)%unit_num = synth_unit_num

       call module_num_setting(m%synth(i)%unit_num, m%synth(i))
       call synth_setting(m%synth(i)%unit_num, m%synth(i))
       
    end do
  end subroutine setup_music
  
  subroutine next_notes_read(set, array)
    type(setting),intent(inout)::set
    real,allocatable,intent(inout)::array(:)

    

  end subroutine next_notes_read

end module parse
