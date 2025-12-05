module audio_streamer_mod
    ! ISO C Bindingモジュールと、SDL2 Fortranバインディングモジュールを使用
    use, intrinsic :: iso_c_binding
    use :: sdl2 ! ユーザーの環境にあるFortran-SDL2バインディングモジュールを想定
    implicit none

    ! ========== 定数定義 (Constants) ==========
    integer, parameter :: sample_rate = 44100_c_int      ! サンプリングレート (Hz)
    real(c_float), parameter :: frequency = 440.0_c_float ! 再生する音の初期周波数 (Hz)
    real(c_float), parameter :: volume = 0.2_c_float      ! 音量 (0.0 から 1.0)
    integer, parameter :: buffer_size_ms = 100           ! 一度にキューに投入するデータの時間 (ミリ秒)
    
    ! 最大キューサイズ (バイト) - 10秒分
    integer(c_size_t), parameter :: max_queue_duration_seconds = 10
    integer(c_size_t), parameter :: max_queue_size = &
        int(sample_rate * sizeof(real(c_float)) * 1 * max_queue_duration_seconds, kind=c_size_t)
    
    ! SDL_AudioSpecをモジュールレベルで保持 (Cのwant/haveに対応)
    type(sdl_audio_spec), allocatable, save :: want_spec, have_spec
    
    ! ========== データ構造体 (Derived Types: Cのmeta_sysに相当) ==========
    ! 全てのストリーミング状態とSDLデバイス情報を保持します。
    type audio_meta_t
        real(c_double) :: current_phase = 0.0_c_double ! 現在のサイン波の位相
        logical :: running = .false.                  ! ストリーミング継続フラグ
        integer :: samples_per_buffer                 ! 一度に生成するサンプル数
        integer(c_size_t) :: buffer_size_bytes        ! バッファのバイトサイズ
        ! オーディオデータバッファ (Cのfloat* audio_bufferに対応)
        real(c_float), allocatable :: audio_buffer(:)
        ! SDLオーディオデバイスID (CのSDL_AudioDeviceID* devに対応)
        integer(c_uint) :: dev_id
    end type audio_meta_t

contains

! ========== サイン波生成サブルーチン (generate_sine_wave) ==========
subroutine generate_sine_wave(buffer, num_samples, current_phase, f)
    use, intrinsic :: iso_c_binding
    implicit none

    real(c_float), intent(out) :: buffer(num_samples)
    integer, intent(in) :: num_samples
    real(c_double), intent(inout) :: current_phase
    real(c_float), intent(in) :: f

    real(c_double) :: phase_increment, pi_2
    integer :: i
    
    ! PIの定義
    real(c_double), parameter :: pi = acos(-1.0_c_double)
    real(c_double), parameter :: volume_d = real(volume, kind=c_double)

    ! 2 * π * 周波数 / サンプリングレート
    phase_increment = 2.0_c_double * pi * real(f, kind=c_double) / real(sample_rate, kind=c_double)
    pi_2 = 2.0_c_double * pi

    ! サイン波生成ループ
    do i = 1, num_samples
        ! サイン波を生成し、音量をかける
        buffer(i) = real(sin(current_phase) * volume_d, kind=c_float)
        
        ! 位相を進める
        current_phase = current_phase + phase_increment
        
        ! 位相を 0 から 2π の範囲に保つ
        if (current_phase >= pi_2) then
            current_phase = current_phase - pi_2
        end if
    end do
end subroutine generate_sine_wave

! ========== システム全体設定関数 (system_setup: Cのaudio_initとsystem_setupを統合) ==========
function system_setup() result(meta)
    use, intrinsic :: iso_c_binding
    use :: sdl2
    use, intrinsic :: iso_fortran_env, only: stderr => error_unit
    
    type(audio_meta_t), allocatable :: meta
    integer :: rc
    character(len=1) :: c_null
    
    c_null = c_null_char

    ! SDLの初期化
    rc = sdl_init(sdl_init_audio)
    if (rc < 0) then
        write (stderr, '("SDLの初期化に失敗しました: ", a)') sdl_get_error() // c_null
        return
    end if
    
    ! メタデータ構造体を割り当て
    allocate(meta)
    
    ! バッファのサイズ計算
    meta%samples_per_buffer = (sample_rate * buffer_size_ms) / 1000
    meta%buffer_size_bytes = int(meta%samples_per_buffer * sizeof(real(c_float)), kind=c_size_t)
    
    ! オーディオバッファを割り当て
    allocate(meta%audio_buffer(meta%samples_per_buffer))
    
    ! SDL_AudioSpec構造体を割り当てて設定
    allocate(want_spec, have_spec)
    
    ! want_specをゼロクリアし、設定
    want_spec = sdl_audio_spec() 
    want_spec%freq = sample_rate
    want_spec%format = sdl_audio_f32sys
    want_spec%channels = 1
    want_spec%samples = 0 ! プッシュ方式のため 0

    ! デバイスを開く (NULLでデフォルトデバイス、0でプッシュ方式)
    meta%dev_id = sdl_open_audio_device(c_null_char, 0, want_spec, have_spec, sdl_audio_allow_any_change)

    if (meta%dev_id == 0) then
        write (stderr, '("オーディオデバイスのオープンに失敗しました: ", a)') sdl_get_error() // c_null
        deallocate(meta%audio_buffer)
        deallocate(meta)
        call sdl_quit()
        return
    end if

    ! 成功
    meta%running = .true.
    
end function system_setup

! ========== オーディオキューへのデータ投入サブルーチン (sound_write) ==========
subroutine sound_write(meta)
    use, intrinsic :: iso_c_binding
    use :: sdl2
    use, intrinsic :: iso_fortran_env, only: stderr => error_unit
    implicit none

    type(audio_meta_t), intent(inout) :: meta
    integer(c_int) :: rc
    character(len=1) :: c_null
    
    c_null = c_null_char

    ! SDL_QueueAudio(dev, buffer, len)
    ! Fortran-SDL2バインディングが配列を直接受け入れることを想定
    rc = sdl_queue_audio(meta%dev_id, meta%audio_buffer, meta%buffer_size_bytes)
    
    if (rc < 0) then
        write (stderr, '("オーディオキューへのプッシュに失敗しました: ", a)') sdl_get_error() // c_null
        meta%running = .false. ! 失敗したら終了
    end if
end subroutine sound_write

! ========== クリーンアップ処理サブルーチン (system_cleanup) ==========
subroutine system_cleanup(meta)
    use, intrinsic :: iso_c_binding
    use :: sdl2
    implicit none

    type(audio_meta_t), intent(inout) :: meta
    
    if (allocated(meta%audio_buffer)) then
        deallocate(meta%audio_buffer)
    end if
    
    ! デバイスを閉じる
    if (meta%dev_id /= 0) then
        call sdl_close_audio_device(meta%dev_id)
    end if
    
    ! グローバルに割り当てたSDL_AudioSpecを解放
    if (allocated(want_spec)) then
        deallocate(want_spec)
    end if
    if (allocated(have_spec)) then
        deallocate(have_spec)
    end if

    ! SDLを終了
    call sdl_quit()
    
end subroutine system_cleanup

end module audio_streamer_mod

! ========== メインプログラム (main) ==========
program main_sdl_audio_stream
    use, intrinsic :: iso_c_binding, only: c_int, c_size_t, c_float
    use, intrinsic :: iso_fortran_env, only: stdout => output_unit
    ! SDLのAPI関数はsdl2モジュールからインポート
    use :: sdl2, only: sdl_pause_audio_device, sdl_get_queued_audio_size, sdl_delay
    use :: audio_streamer_mod
    implicit none

    type(audio_meta_t), allocatable :: meta
    integer :: i
    integer(c_size_t) :: queued_size, max_queue_threshold
    integer(c_int) :: size_int
    
    ! セットアップ
    meta = system_setup()
    if (.not. allocated(meta) .or. .not. meta%running) then
        write (*, '(a)') 'システムセットアップに失敗しました。終了します。'
        stop 1
    end if

    ! 初期データ投入: 5バッファ分 (キューの開始)
    do i = 1, 5
        if (.not. meta%running) exit
        ! サイン波を生成
        call generate_sine_wave(meta%audio_buffer, meta%samples_per_buffer, &
                                & meta%current_phase, frequency)
        call sound_write(meta)
    end do
    
    ! 再生開始 (一時停止を解除)
    call sdl_pause_audio_device(meta%dev_id, 0)
    
    i = 0
    ! メインストリーミングループ (周波数を変化させながらデータを供給)
    do
        i = i + 1
        if (i > 1000) then
            write (*, '(a)') 'end'
            exit
        end if
        
        ! 周波数を徐々に変化させる (Cコードの 330 + i に相当)
        call generate_sine_wave(meta%audio_buffer, meta%samples_per_buffer, &
                                & meta%current_phase, real(330 + i, kind=c_float))
        
        ! キューサイズを取得
        queued_size = sdl_get_queued_audio_size(meta%dev_id)
        
        ! 出力用にintに変換
        size_int = int(queued_size, kind=c_int) 
        write (stdout, '(i0)') size_int
        
        ! キューが空になりすぎないように待機
        ! Cコードの条件を再現 (MAX_QUEUE_SIZE / 5 または buffer_size_bytes * 5)
        max_queue_threshold = int(max_queue_size / 5, kind=c_size_t)
        
        do while (queued_size < max_queue_threshold .and. queued_size < (meta%buffer_size_bytes * 5))
            call sdl_delay(1)
            queued_size = sdl_get_queued_audio_size(meta%dev_id)
        end do
        
        ! 新しいバッファをキューに投入
        call sound_write(meta)
        
        if (.not. meta%running) exit ! sound_writeでエラーが発生した場合
    end do
    
    write (*, '(a)') '再生キューに残ったデータがなくなるのを待機中...'

    ! 再生キューが空になるのを待つ
    do 
        queued_size = sdl_get_queued_audio_size(meta%dev_id)
        size_int = int(queued_size, kind=c_int)
        
        write (stdout, '(i0, a)', advance='no') size_int, '/'
        call sdl_delay(100)
        
        if (queued_size == 0) exit
    end do
    
    write (*, '(a)') '' ! 改行
    
    ! 終了時の処理
    ! 一時停止
    call sdl_pause_audio_device(meta%dev_id, 1) 
    
    write (*, '(a)') '再生が完了し、終了します。'

    ! クリーンアップ
    call system_cleanup(meta)
    
    ! メタデータ構造体を解放
    if (allocated(meta)) deallocate(meta)

end program main_sdl_audio_stream
