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
     real out
  end type env
  
  type::lfo
     real lfo_unit(3)
     real out
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
       read(op, *) num
       num_reg = num
    end if
  end function num_reg
  
  subroutine reallocate_real_2d(list, new_x, new_y)
    real, allocatable, intent(inout)::list(:, :)
    integer new_x, new_y

    real, allocatable::buffer(:, :)

    allocate(buffer(new_x, new_y), mold=list)

    list = buffer
    deallocate(buffer)
  end subroutine reallocate_real_2d

  subroutine reallocate_env(list, new_size)
    type(env), allocatable, intent(inout)::list(:)
    integer, intent(in)::new_size

    type(env), allocatable::buffer(:)

    allocate(buffer(new_size), mold=list)

    list = buffer
    deallocate(buffer)
  end subroutine reallocate_env

  subroutine reallocate_lfo(list, new_size)
    type(lfo), allocatable, intent(inout)::list(:)
    integer, intent(in)::new_size

    type(lfo), allocatable::buffer(:)

    allocate(buffer(new_size), mold=list)

    list = buffer
    deallocate(buffer)
  end subroutine reallocate_lfo
 
  subroutine synth_setting(unit_num, set)
    integer, intent(inout)::unit_num
    type(setting),intent(out)::set

    character(len=80) line
    character(len=5) operate
    integer iostat_value, scpos, playpos, ophead, optail
    integer i, ro, rgx(5)
    logical lrgx(5)

    type(osc)::osc_d


    set%oscillator%num = 0
    set%filter%num = 0
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
             call reallocate_real_2d(set%oscillator%list, rgx(1), 2)
          else
             allocate(set%oscillator%list(rgx(1), 2))
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
          
          set%oscillator%list(rgx(1),1) = rgx(2)

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
          set%oscillator%list(rgx(1),2) = num_reg(operate)

       case("flt")
          rgx(1) = set%filter%num + 1
          set%filter%num = rgx(1)
          if (allocated(set%filter%list)) then
             call reallocate_real_2d(set%filter%list, rgx(1), 3)
          else
             allocate(set%filter%list(rgx(1), 3))
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

          set%filter%list(rgx(1),1) = rgx(2)

          do ro = 1, 2
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
             set%filter%list(rgx(1),ro + 1) = num_reg(operate)
          end do

       case("env")
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
          read(operate, *) rgx(2)
          rgx(1) = set%env_num
          if(rgx(1) < rgx(2)) then
             set%env_num = rgx(1) + 1
             rgx(1) = set%env_num
             if (allocated(set%oscillator%list)) then
                call reallocate_env(set%envelope, rgx(1))
             else
                allocate(set%envelope(rgx(1)))
             end if
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
          case("atk")
             rgx(2) = 1
          case("dec")
             rgx(2) = 2
          case("sus")
             rgx(2) = 3
          case("sus")
             rgx(2) = 4
          case("out")
             rgx(2) = 5
          end select

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

          if(rgx(2) /= 5) then
             set%envelope(rgx(1))%env_unit(rgx(2)) = num_reg(operate)
          else
             set%envelope(rgx(1))%out = num_reg(operate)
          end if
           
       case("lfo")
          rgx(1) = set%lfo_num + 1
          set%lfo_num = rgx(1)
          if (allocated(set%lfo)) then
             call reallocate_lfo(set%lfo, rgx(1))
          else
             allocate(set%lfo(rgx(1))
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
          case("saw")
             rgx(2) = 3
          case("sqr")
             rgx(2) = 4
          case("rnd")
             rgx(2) = 5
          end select
          
          set%lfo(rgx(1))%lfo_unit(1)
          

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

       setpos = index(line, "setting:")
       if(setpos /= 0) exit
       
    end do
    call synth_setting(unit_num, set)
    close(unit_num)
  end subroutine execute


end module parse
