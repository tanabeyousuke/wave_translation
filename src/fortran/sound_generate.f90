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
    set%space = 44100
    do
      if(set%writed .eqv. .true.)then
         exit
      end if

       read(unit_num, '(A:)', iostat=iostat_value) line
       endpos = index(line, "end:")
       if(iostat_value /= 0 .or. endpos /= 0)then
          set%slc = .true.
          exit
       end if

       if(set%rest /= 0)then
          call rest(set)
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

    integer::i, i1, leng, seek
    real::data,rrgx(5)
    logical::ext
    real,allocatable::f(:)
    
    allocate(f(set%vce_num))
    print *, "a"

    do i = 1, set%vce_num
       rrgx(1) = 440.0 * 2 ** (set%vce(i)%oct - 4)
       f(i) = rrgx(1) * 2 ** set%vce(i)%pn
    end do
    
    ext = .false.
    
    if(set%rest < set%space)then
       leng = set%rest
    else
       leng = set%space
       set%writed = .true.
    end if
    
seek = 220500 - set%space

    do i = 1, set%vce_num
       ! 各ボイスごとに書き込み開始位置をリセット
       seek = 220500 - set%space 
       
       do i1 = 1, leng
          ! 3. 境界チェック（配列サイズを越えないように）
          if (seek >= 1 .and. seek <= 220500) then
             
             ! 4. インデックスを i に修正、かつ実数計算(44100.0)にする
             rrgx(1) = osc_sin(f(i) * (set%vce(i)%count / 44100.0)) * reg_value(set, set%osc_g(1))
             ! ... (rrgx(2)〜(5)も同様に i1 ではなく i を使用) ...
             
             set%buffer(seek) = set%buffer(seek) + sum(rrgx)
          end if
          
          seek = seek + 1
          set%vce(i)%count = set%vce(i)%count + 1
       end do
    end do

    set%rest = set%rest - leng
    set%space = set%space - leng
    if (allocated(f)) deallocate(f)
  end subroutine rest

  function reg_value(set, p)
    type(setting), intent(in)::set
    type(param), intent(in)::p
    real::reg_value
    
    if(p%rorv .eqv. .true.)then
       reg_value = set%reg(p%reg_num)
    else
       reg_value = p%value
    end if
  end function reg_value
  
end module sound_generate
