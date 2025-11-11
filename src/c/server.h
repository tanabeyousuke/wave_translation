typedef struct
{
  SDL_AudioSpec *want, *have;
  SDL_AudioDeviceID *dev;
} meta_sdl;

typedef struct
{
  double current_phase;
  int running; // ストリーミング継続フラグ
  
  int samples_per_buffer;
  size_t buffer_size_bytes;
  float* audio_buffer;
  
  meta_sdl *ms;
} meta_sys;

// ========== 関数プロトタイプ宣言 ==========
meta_sdl* audio_init(void);
meta_sys* system_setup(void);
void generate_sine_wave(float* buffer, int num_samples, double* current_phase, float f);
void sound_write(meta_sys *meta);
void system_cleanup(meta_sys *meta);
void sound_start(meta_sys *meta);
void sound_stop(meta_sys *meta);
void* buffer_pointer(meta_sys *meta);
