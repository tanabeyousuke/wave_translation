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
     integer::time
     integer::last
     integer::pn
     integer::oct
     real,allocatable::phase(:)
  end type voice
  
  type::lowfrequency
     integer::form !波形
     type(param)::p(4) !周波数、振幅、オフセット、出力
     real::phase
  end type lowfrequency

  type::effect
     integer::type
     type(param)::p(5)
     real,allocatable::data(:)
  end type effect

  type::oscillator
     integer::type !sin, del, sqr, saw, rnd
     type(param)::p(2) !ピッチ倍率、ゲイン
  end type oscillator

  type::envelope
     type(param)::p(7)!オフセット、倍率、atk, dec, sus, rel, 出力
  end type envelope

  type::setting
     type(oscillator),allocatable::osc(:)
     type(envelope),allocatable::env(:)
     type(lowfrequency),allocatable::lfo(:) !ゲインにつなぐLFOのパラメータ 周波数、振幅、オフセット、出力
     type(effect),allocatable::efc(:) !エフェクト 

     type(param)::amp

     type(voice)::vce
     
     type(param)::pitch
     
     integer::unit_num 
     real::buffer(4410)
     real::reg(64)
     integer::writed, rest, start, last, pc
     logical::ready, slc
     character(len=10), allocatable::label(:)
     integer, allocatable::label_addr(:)
  end type setting
  
  type::mnemonic
     integer::oprt
     integer::oprd_i(3)
     real::oprd_r(3)
     character(len=10)::label
  end type mnemonic

  type music
     type(setting), allocatable::synth(:)
     type(mnemonic), allocatable::all_mnm(:)
  end type music

contains
  subroutine num_reg(op, p)!レジスタ番号を実数に直します。
    character(*), intent(in)::op
    type(param),intent(inout)::p

    integer::rp

    rp = index(op, "r")
    if(rp /= 0) then
       read(op(rp+1:), *) p%reg_num
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
       if(line(i:i) == ' ' .or. i == scpos)then
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
    integer i, ro, lfo_num, efc_num, rgx(5)
    logical lrgx(5)
    type(oscillator)::osc
    type(envelope)::env
    type(lowfrequency)::lfo !ゲインにつなぐLFOのパラメータ 周波数、振幅、オフセット、出力
    type(effect)::efc !エフェクト 

    set%unit_num = unit_num

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
             allocate(efc%data(4))
             efc%data = 0
          case("hig")
             rgx(1) = 2
             allocate(efc%data(4))
             efc%data = 0
          case("bnd")
             rgx(1) = 3
             allocate(efc%data(3))
             efc%data = 0
          case("dly")
             rgx(1) = 4
             allocate(efc%data(2 + 66150))
             efc%data = 0
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
          if(allocated(efc%data))then
             deallocate(efc%data)
          end if
          
       case("amp")
          optail = optail + 1
          call get_token(line, ophead, optail, scpos)
          operate = line(ophead:optail)

          call num_reg(operate, set%amp) 
       case("pch")
          optail = optail + 1
          call get_token(line, ophead, optail, scpos)
          operate = line(ophead:optail)

          call num_reg(operate, set%pitch) 
       end select
    end do

    set%buffer(:) = 0
    set%reg(:) = 0
    set%writed = 0
    set%rest = 0
    set%ready = .false.
    set%slc = .false.
    allocate(set%vce%phase(size(set%osc)))

  end subroutine synth_setting

  subroutine assemble(m)
    type(music),intent(inout)::m

    integer::nu, ni, sp, ep, rp, oh, ot, i, i1, i2, rgx(5), nm
    character(len=80)::line
    character(len=10)::operate
    type(mnemonic)::mnm

    nm = 1
    allocate(m%all_mnm(1))

    do i = 1, size(m%synth)
       nu = m%synth(i)%unit_num
       allocate(m%synth(i)%label(0))
       allocate(m%synth(i)%label_addr(0))
       
       m%synth(i)%start = nm
       m%synth(i)%pc = nm
       do
          read(nu, '(A:)', iostat=ni) line
          if(ni /= 0) then
             print *, "error: assemble, file read", nm, "at", nu
             exit
          end if
          
          ep = index(line, "end:")
          if(ep /= 0) then
             exit
          end if

          do i1 = 1, len(line)
             if(line(i:i) == char(9)) line(i:i) = ' '
          end do

          ot = 1
          sp = index(line, ';')
          if(sp == 0) then
             sp = index(line, ':')
             if (sp /= 0) then
                call get_token(line, oh, ot, sp)
                m%synth(i)%label = [character(len=10) :: m%synth(i)%label, line(oh:ot)]
                m%synth(i)%label_addr = [m%synth(i)%label_addr, nm]
             end if
             cycle
          end if
          

          call get_token(line, oh, ot, sp)
          operate = line(oh:ot)

          mnm%oprt = 0
          mnm%oprd_i = 0
          mnm%oprd_r = 0
          mnm%label = ""

          select case(operate)
          case("mov")
             ot = ot + 1
             call get_token(line, oh, ot, sp)
             operate = line(oh:ot)
             rp = index(operate, "r")
             read(operate(rp+1:), *) mnm%oprd_i(1)
             
             ot = ot + 1
             call get_token(line, oh, ot, sp)
             operate = line(oh:ot)
             rp = index(operate, "r")
             if (rp == 0) then
                read(operate, *) mnm%oprd_r(1)
                mnm%oprt = 1
             else
                read(operate(rp+1:), *) mnm%oprd_i(2)
                mnm%oprt = 2
             end if
          case("add")
             ot = ot + 1
             call get_token(line, oh, ot, sp)
             operate = line(oh:ot)
             rp = index(operate, "r")
             read(operate(rp+1:), *) mnm%oprd_i(1)

             ot = ot + 1
             call get_token(line, oh, ot, sp)
             operate = line(oh:ot)
             rp = index(operate, "r")
             if (rp == 0) then
                read(operate, *) mnm%oprd_r(1)
                mnm%oprt = 3
             else
                read(operate(rp+1:), *) mnm%oprd_i(2)
                mnm%oprt = 4
             end if
          case("sub")
             ot = ot + 1
             call get_token(line, oh, ot, sp)
             operate = line(oh:ot)
             rp = index(operate, "r")
             read(operate(rp+1:), *) mnm%oprd_i(1)

             ot = ot + 1
             call get_token(line, oh, ot, sp)
             operate = line(oh:ot)
             rp = index(operate, "r")
             if (rp == 0) then
                read(operate, *) mnm%oprd_r(1)
                mnm%oprt = 5
             else
                read(operate(rp+1:), *) mnm%oprd_i(2)
                mnm%oprt = 6
             end if
          case("mul")
             ot = ot + 1
             call get_token(line, oh, ot, sp)
             operate = line(oh:ot)
             rp = index(operate, "r")
             read(operate(rp+1:), *) mnm%oprd_i(1)

             ot = ot + 1
             call get_token(line, oh, ot, sp)
             operate = line(oh:ot)
             rp = index(operate, "r")
             if (rp == 0) then
                read(operate, *) mnm%oprd_r(1)
                mnm%oprt = 7
             else
                read(operate(rp+1:), *) mnm%oprd_i(2)
                mnm%oprt = 8
             end if
           case("div")
             ot = ot + 1
             call get_token(line, oh, ot, sp)
             operate = line(oh:ot)
             rp = index(operate, "r")
             read(operate(rp+1:), *) mnm%oprd_i(1)

             ot = ot + 1
             call get_token(line, oh, ot, sp)
             operate = line(oh:ot)
             rp = index(operate, "r")
             if (rp == 0) then
                read(operate, *) mnm%oprd_r(1)
                mnm%oprt = 9
             else
                read(operate(rp+1:), *) mnm%oprd_i(2)
                mnm%oprt = 10
             end if

          case("jmp")
             ot = ot + 1
             call get_token(line, oh, ot, sp)
             operate = line(oh:ot)
             read(operate, *) mnm%label
             mnm%oprt = 11
          case("joz") !jump over zero
             ot = ot + 1
             call get_token(line, oh, ot, sp)
             operate = line(oh:ot)
             rp = index(operate, "r")
             read(operate(rp+1:), *) mnm%oprd_i(1)
             
             ot = ot + 1
             call get_token(line, oh, ot, sp)
             operate = line(oh:ot)
             read(operate, *) mnm%label
             mnm%oprt = 12
          case("juz") !jump under zero
             ot = ot + 1
             call get_token(line, oh, ot, sp)
             operate = line(oh:ot)
             rp = index(operate, "r")
             read(operate(rp+1:), *) mnm%oprd_i(1)
             
             ot = ot + 1
             call get_token(line, oh, ot, sp)
             operate = line(oh:ot)
             read(operate, *) mnm%label
             mnm%oprt = 13

          case("key")
             ot = ot + 1
             call get_token(line, oh, ot, sp)
             operate = line(oh:ot)

             select case(operate)
             case("ces")
                rgx(2) = -10
             case("c")
                rgx(2) = -9
             case("cis")
                rgx(2) = -8
             case("des")
                rgx(2) = -8
             case("d")
                rgx(2) = -7
             case("dis")
                rgx(2) = -6
             case("es")
                rgx(2) = -6
             case("e")
                rgx(2) = -5
             case("f")
                rgx(2) = -4
             case("fis")
                rgx(2) = -3
             case("ges")
                rgx(2) = -3
             case("g")
                rgx(2) = -2
             case("gis")
                rgx(2) = -1
             case("as")
                rgx(2) = -1
             case("a")
                rgx(2) = 0
             case("ais")
                rgx(2) = 1
             case("b")
                rgx(2) = 1
             case("h")
                rgx(2) = 2
             case("his")
                rgx(2) = 3
             end select
             
             mnm%oprd_i(1) = rgx(2)

             ot = ot + 1
             call get_token(line, oh, ot, sp)
             operate = line(oh:ot)
             read(operate, *) mnm%oprd_i(2)
             
             ot = ot + 1
             call get_token(line, oh, ot, sp)
             operate = line(oh:ot)
             read(operate, *) mnm%oprd_i(3)
             
             mnm%oprt = 14
          case("rst")
             ot = ot + 1
             call get_token(line, oh, ot, sp)
             operate = line(oh:ot)
             read(operate, *) mnm%oprd_i(1)

             mnm%oprt = 15
             
          case("prt")
             ot = ot + 1
             call get_token(line, oh, ot, sp)
             operate = line(oh:ot)
             read(operate, *) mnm%label
             mnm%oprt = 16
          end select

          m%all_mnm(nm) = mnm
          if(nm == size(m%all_mnm))then
             m%all_mnm = [m%all_mnm, m%all_mnm]
          end if
          nm = nm + 1
       end do

       m%synth(i)%last = nm - 1
       
       do i1 = m%synth(i)%start, m%synth(i)%last
          if(m%all_mnm(i1)%oprt >= 11 .and. m%all_mnm(i1)%oprt <= 13) then
             do i2 = 1, size(m%synth(i)%label)
                if(m%all_mnm(i1)%label == m%synth(i)%label(i2))then
                   m%all_mnm(i1)%oprd_i(3) = m%synth(i)%label_addr(i2)
                end if
             end do
             if (m%all_mnm(i1)%oprd_i(3) == 0) then
                print *, "assemble error at", i1
                print *, m%all_mnm(i1)%label, "is not existing"
                exit
             end if
          end if
       end do

    end do


  end subroutine assemble
  
  subroutine setup_music(filename, m)
    character(*),intent(in)::filename
    type(music),intent(inout)::m

    integer unit_num, synth_unit_num, iostat_value, synth_iostat_value, scpos, num, i
    character(len=80) line, file
    type(setting)::synth

    unit_num = 10
    open(unit=unit_num, file=filename, status='OLD', iostat=iostat_value)
    if(iostat_value /= 0)then
       print *, "error: music file open failed"
       stop
    end if

    allocate(m%synth(0))
    
    i = 1
    do
       read(unit_num, '(A:)', iostat=iostat_value) line
       if(iostat_value /= 0)then
          print *, "That's likely the end of music file."
          exit
       end if

       synth_unit_num = 10 + i
       open(unit=synth_unit_num, file=line, status='OLD', iostat=synth_iostat_value)
       if(iostat_value /= 0)then
          print *, "error: ", line, "open_failed"
          
       end if
       
       call synth_setting(synth_unit_num, synth)
       
       m%synth = [m%synth, synth]
       i = i + 1

       deallocate(synth%osc)
       deallocate(synth%env)
       deallocate(synth%lfo)
       deallocate(synth%efc)
       deallocate(synth%vce%phase)
    end do

    call assemble(m)
  end subroutine setup_music

  function data_real(reg, p)
    real, intent(in)::reg(64)
    type(param), intent(in)::p
    real::data_real

    if(p%rorv .eqv. .true.)then
       data_real = reg(p%reg_num)
    else
       data_real = p%value
    end if
  end function data_real
end module parse
