! example.f90
program main
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env, only: stdout => output_unit, stderr => error_unit
    use :: sdl2
    implicit none

    integer, parameter :: SCREEN_WIDTH  = 640
    integer, parameter :: SCREEN_HEIGHT = 480

    type(c_ptr)     :: window
    type(c_ptr)     :: renderer
    type(sdl_event) :: event
    type(sdl_rect)  :: rect
    integer         :: rc
    logical         :: is_running

    ! Initialise SDL.
    if (sdl_init(SDL_INIT_VIDEO) < 0) then
        write (stderr, '("SDL Error: ", a)') sdl_get_error()
        stop
    end if

    ! Create the SDL window.
    window = sdl_create_window('Fortran SDL 2.0' // c_null_char, &
                               SDL_WINDOWPOS_UNDEFINED, &
                               SDL_WINDOWPOS_UNDEFINED, &
                               SCREEN_WIDTH, &
                               SCREEN_HEIGHT, &
                               SDL_WINDOW_SHOWN)

    if (.not. c_associated(window)) then
        write (stderr, '("SDL Error: ", a)') sdl_get_error()
        stop
    end if

    ! Create the renderer.
    renderer = sdl_create_renderer(window, -1, 0)

    ! Set position and size of the rectangle.
    rect = sdl_rect(50, 50, 250, 250)

    ! Event loop.
    is_running = .true.

    do while (is_running)
        ! Catch events.
        do while (sdl_poll_event(event) > 0)
            select case (event%type)
                case (SDL_QUITEVENT)
                    is_running = .false.
            end select
        end do

        ! Fill screen black.
        rc = sdl_set_render_draw_color(renderer, &
                                       uint8(0), &
                                       uint8(0), &
                                       uint8(0), &
                                       uint8(SDL_ALPHA_OPAQUE))
        rc = sdl_render_clear(renderer)

        ! Fill the rectangle.
        rc = sdl_set_render_draw_color(renderer, &
                                       uint8(127), &
                                       uint8(255), &
                                       uint8(0), &
                                       uint8(SDL_ALPHA_OPAQUE))
        rc = sdl_render_fill_rect(renderer, rect)

        ! Render to screen and wait 20 ms.
        call sdl_render_present(renderer)
        call sdl_delay(20)
    end do

    ! Quit gracefully.
    call sdl_destroy_renderer(renderer)
    call sdl_destroy_window(window)
    call sdl_quit()
end program main
