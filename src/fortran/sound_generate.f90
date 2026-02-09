module sound_generate
  use c_if
  use env
  use osc
  use parse
  use efc
  implicit none

contains
  subroutine play(mt, msc)
    type(c_ptr),intent(in)::mt
    type(music),intent(inout)::msc

    real(c_float),pointer::input(:)
    real,allocatable,dimension(:)::buf
    integer::len,flag,i,i1
    real::dummy(4410)

    call c_f_pointer(bp(mt), input, SHAPE=[4410])

    len = 44100
    allocate(buf(len))

    dummy = 0
    do i = 1, 10
       input = dummy
       call sound_write(mt)
    end do
    
    msc%synth%slc=.false.
    do
       input(:) = 0
       do i1 = 1, size(msc%synth)
          input(:) = input(:) + msc%synth(i1)%buffer / size(msc%synth)
       end do
       call sound_write(mt)

       if(all(msc%synth%slc))then
          exit
       else
          call music_generate(msc)
       end if

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
    type(setting),intent(inout)::set

    character(len=80)::line
    character(len=10)::operate
    integer::unit_num, iostat_value, scpos, endpos, ophead, optail
    integer::i, space, rgx(5)

    unit_num = set%unit_num
    set%buffer(:) = 0

    set%ready = .false.
    set%writed = 0
    
    do
       if(set%ready .eqv. .true.) then
          exit
       end if

       if(set%vce%play .eqv. .true.) then
          call write_buffer(set)
          cycle
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
          read(operate, *) set%reg(rgx(1))

       case("key")
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

          set%vce%pn = rgx(2)

          optail = optail + 1
          call get_token(line, ophead, optail, scpos)
          operate = trim(line(ophead:optail))
          
          read(operate, *) set%vce%oct

          optail = optail + 1
          call get_token(line, ophead, optail, scpos)
          operate = trim(line(ophead:optail))

          read(operate, *) rgx(1)
          
          set%vce%count = rgx(1)
          set%vce%time = 0
          set%vce%phase = 0
          set%vce%play = .true.
          set%vce%push = .true.

       case("rst")
          
          optail = optail + 1
          call get_token(line, ophead, optail, scpos)
          operate = trim(line(ophead:optail))

          read(operate, *) rgx(1)
          
          
          set%vce%count = set%vce%count + rgx(1)

          set%vce%play = .true.
          set%vce%push = .false.
       case("prt")
          
          optail = optail + 1
          call get_token(line, ophead, optail, scpos)
          operate = trim(line(ophead:optail))

          print *, operate
       end select

       
    end do

  end subroutine fill_buffer

  subroutine write_buffer(set)
    type(setting),intent(inout)::set

    integer::i, i1, leng, space, time, pos, n
    real::f, signal(5), prm_wav, effected_wav, out, env(4)

    n = (set%vce%pn) + (set%vce%oct - 4) * 12 
    f = 440.0 * (2**(n/12.0))
 
    space = 4410 - set%writed
    
    if(space < set%vce%count - set%vce%time)then
       leng = space
    else
       leng = set%vce%count - set%vce%time
    end if

    do i = 1, leng
       do i1 = 1, size(set%lfo)
          if (set%lfo(i1)%p(4)%rorv .eqv. .true.)then
             select case(set%lfo(i1)%form)
             case(1)
                set%reg(set%lfo(i1)%p(4)%reg_num) = &
                     osc_sin(set%lfo(i1)%phase) * &
                     data_real(set, set%lfo(i1)%p(2)) + &
                     data_real(set, set%lfo(i1)%p(3))
             case(2)
                set%reg(set%lfo(i1)%p(4)%reg_num) = &
                     osc_del(set%lfo(i1)%phase) * &
                     data_real(set, set%lfo(i1)%p(2)) + &
                     data_real(set, set%lfo(i1)%p(3))
             case(3)
                set%reg(set%lfo(i1)%p(4)%reg_num) = &
                     osc_saw(set%lfo(i1)%phase) * &
                     data_real(set, set%lfo(i1)%p(2)) + &
                     data_real(set, set%lfo(i1)%p(3))
             case(4)
                set%reg(set%lfo(i1)%p(4)%reg_num) = &
                     osc_sqr(set%lfo(i1)%phase) * &
                     data_real(set, set%lfo(i1)%p(2)) + &
                     data_real(set, set%lfo(i1)%p(3))
             end select
          end if
          set%lfo(i1)%phase = set%lfo(i1)%phase + &
               mod(data_real(set, set%lfo(i1)%p(1)) / 44100.0, 1.0)   
       end do
      
       pos = set%writed + i
       time = set%vce%time + i

       signal(1) = osc_sin(set%vce%phase) * data_real(set, set%osc_g(1)) / 100
       signal(2) = osc_del(set%vce%phase) * data_real(set, set%osc_g(2)) / 100
       signal(3) = osc_saw(set%vce%phase) * data_real(set, set%osc_g(3)) / 100
       signal(4) = osc_sqr(set%vce%phase) * data_real(set, set%osc_g(4)) / 100
       signal(5) = osc_rnd() * data_real(set, set%osc_g(5)) / 100

       set%vce%phase = set%vce%phase + ((f + data_real(set, set%pitch))&
            / 44100.0)
       if(set%vce%phase > 1.0)then
          set%vce%phase = set%vce%phase - 1
       end if

       do i1 = 1, 4 
          env(i1) = data_real(set, set%env(i1))
       end do

       prm_wav = sum(signal) * &
            env_out(env(1), env(2), env(3), env(4), &
            time, set%vce%last, set%vce%push)
          
       
       call efc_unit_pass(set%efc, set%reg, prm_wav, effected_wav)

       out = effected_wav * data_real(set, set%amp) / 100

       set%buffer(pos) = out
       
    end do
    set%vce%time = set%vce%time + leng

    set%writed = set%writed + leng


    if(set%writed == 4410) then
       set%ready = .true.
    else
       set%vce%play = .false.
       if(set%vce%push .eqv. .true.) set%vce%last = time
    end if

  end subroutine write_buffer

end module sound_generate
