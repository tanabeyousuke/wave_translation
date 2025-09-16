module parse !パーサです。シンセサイザの設定や演奏の実行などを処理します。
  implicit none
     
  type::osc
     real,allocatable::list(:,:)
     real pch
     integer num
  end type osc
 
  type::flt
     real, allocatable::list(:,:)
     integer num
  end type flt
  
  type::env
     real env_unit(4)
     integer out
  end type env
  
  type::lfo
     real lfo_unit(3)
     integer out
  end type lfo
  
  type::setting
     type(osc)::oscillator
     type(flt)::filter
     type(env), allocatable::envelope(:)
     type(lfo), allocatable::lfo(:)
     integer env_num
     integer lfo_num
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
       read(op, *)num
       num_reg = num
    end if
  end function num_reg
    
  subroutine synth_setting(unit_num, set)
    integer, intent(inout)::unit_num
    type(setting),intent(out)::set

    character(len=80) line
    character(len=5) operate
    integer iostat_value, scpos, playpos, ophead, optail
    integer i, ro, rgx(5)
    logical lrgx(5)

    set%oscillator%num = 0
    set%filter%num = 0
    allocate(set%filter%list(1,3))
    set%env_num = 0
    set%lfo_num = 0

    ophead = 0

    do
       read(unit_num, '(A:)', iostat=iostat_value)line
    
       playpos = index(line, "play:")
       if(playpos /= 0) exit

       scpos = index(line, ';')
       if(scpos == 0) cycle

       lrgx(2) = .false.
       do i = 1, scpos
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
       
       operate = line(ophead:optail)
       select case(trim(operate))
       case("osc")
          rgx(1) = set%oscillator%num + 1
          set%oscillator%num = rgx(1)
          if (allocated(set%oscillator%list)) then
             allocate(set%oscillator%list(rgx(1), 2), source=set%oscillator%list)
          else
             allocate(set%oscillator%list(rgx(1), 2))
          end if

          print *, "allocate"

          do i = optail, scpos
             if(line(i:i) /= ' ') then
                ophead = i
             end if
             
             lrgx(1) = line(i:i) == ' ' .or. line(i:i) == ';'
             if(ophead < i .and. lrgx(1)) then
                optail = i
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
          
          set%oscillator%list(rgx(1),1) = rgx(2)

          do i = optail, scpos
             if(line(i:i) /= ' ') then
                ophead = i
             end if
             
             lrgx(1) = line(i:i) == ' ' .or. line(i:i) == ';'
             if(ophead < i .and. lrgx(1)) then
                optail = i
             end if
          end do
          
          operate = trim(line(ophead:optail))
          set%oscillator%list(rgx(1),2) = num_reg(operate)
       case("flt")
          print *, "flt"
          rgx(1) = set%filter%num + 1
          set%filter%num = rgx(1)
          allocate(set%filter%list(rgx(1),3), source=set%filter%list)

          do i = optail, scpos
             if(line(i:i) /= ' ') then
                ophead = i
             end if
             
             lrgx(1) = line(i:i) == ' ' .or. line(i:i) == ';'
             if(ophead < i .and. lrgx(1)) then
                optail = i
             end if
          end do

          operate = trim(line(ophead:optail))
          select case(operate)
          case("low")
             rgx(2) = 1
          case("hig")
             rgx(2) = 2
          end select

          set%filter%list(rgx(1),1) = rgx(2)

          do ro = 1, 2
             do i = optail, scpos
                if(line(i:i) /= ' ') then
                   ophead = i
                end if
                
                lrgx(1) = line(i:i) == ' ' .or. line(i:i) == ';'
                if(ophead < i .and. lrgx(1)) then
                   optail = i
                end if
             end do
             
             operate = trim(line(ophead:optail))
             set%filter%list(rgx(1),ro + 1) = num_reg(operate)
          end do
       end select


       ! case(env)
       !    do i = optail, scpos
       !       if(line(i:i) /= ' ') then
       !          ophead = i
       !       end if
             
       !       lrgx(1) = line(i:i) == ' ' or line(i:i) == ';'
       !       if(ophead < i .and. lrgx) then
       !          optail = i
       !       end if
       !    end do
          
          
       !    operate = trim(line(ophead:optail))
       !    set%filter%list(rgx(1):ro + 1) = num_reg(operate)
          
          ! read(operate, *) rgx
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

       setpos = index(line, "setting:")
       if(setpos /= 0) exit
       
    end do
    call synth_setting(unit_num, set)
    close(unit_num)
  end subroutine execute


end module parse
