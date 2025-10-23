#include <SDL2/SDL.h>
#include <stdio.h>
#include <math.h>

#define SAMPLE_RATE 44100      // サンプリングレート (Hz)
#define FREQUENCY 440.0        // 再生する音の周波数 (A4の音、Hz)
#define VOLUME 0.2             // 音量 (0.0 から 1.0)
#define DURATION_SECONDS 2     // 再生時間 (秒)
#define BUFFER_SIZE_MS 100     // 一度にキューに投入するデータの時間 (ミリ秒)
#define MAX_QUEUE_SIZE 441000  // キューの最大サイズ (バイト)。例: 10秒分のデータ


void generate_sine_wave(float* buffer, int num_samples, double* current_phase)
{
  double phase_increment = 2.0 * M_PI * FREQUENCY / SAMPLE_RATE;
  
  for(int i = 0; i < num_samples; ++i)
    {
      buffer[i] = sin(*current_phase) * VOLUME;
      
      *current_phase += phase_increment;
      if(*current_phase >= 2.0 * M_PI)
	{
	  *current_phase -= 2.0 * M_PI;
	}
    }
}

typedef struct{
  SDL_AudioSpec *want, *have;
  SDL_AudioDeviceID* dev;
} meta_sdl;

typedef struct{
  double current_phase;
  
  int total_samples_to_generate;
  int generated_samples;
  
  int samples_per_buffer;
  size_t buffer_size_bytes;
  float* audio_buffer;
  meta_sdl *ms;
} meta_sys;

meta_sdl* audio_init(void)
{
  meta_sdl *init = malloc(sizeof(meta_sdl));
  init->want = malloc(sizeof(SDL_AudioSpec));
  init->have = malloc(sizeof(SDL_AudioSpec));

  init->dev = malloc(sizeof(SDL_AudioDeviceID));

  SDL_zero(*init->want); // 構造体をゼロクリア
  
  init->want->freq = SAMPLE_RATE;          // サンプリングレート
  init->want->format = AUDIO_F32SYS;       // 32bit浮動小数点数フォーマット
  init->want->channels = 1;                // モノラル
  init->want->samples = 0;                 // コールバックを使用しないため、この値は無視されることが多いが、0に設定

  *init->dev = SDL_OpenAudioDevice(NULL, 0, init->want, init->have, SDL_AUDIO_ALLOW_ANY_CHANGE);
  
  if (*init->dev == 0)
    {
      printf("オーディオデバイスのオープンに失敗しました: %s\n", SDL_GetError());
      SDL_Quit();
      free(init->want);
      free(init->have);
      free(init->dev);
      return 0;
    }
  return init;
}

void system_setup(meta_sys *systemdata)
{
  systemdata = malloc(sizeof(meta_sys));
  systemdata->current_phase = 0.0;
  
  systemdata->total_samples_to_generate = SAMPLE_RATE * DURATION_SECONDS;
  systemdata->generated_samples = 0;
  
  systemdata->samples_per_buffer = (SAMPLE_RATE * BUFFER_SIZE_MS) / 1000;
  systemdata->buffer_size_bytes = systemdata->samples_per_buffer * sizeof(float);
  systemdata->audio_buffer = (float*)malloc(systemdata->buffer_size_bytes);
  
  if (systemdata->audio_buffer == NULL)
    {
      fprintf(stderr, "メモリ確保に失敗しました。\n");
      exit(1);
    }
  
  if (SDL_Init(SDL_INIT_AUDIO) < 0)
    {
      fprintf(stderr, "SDLの初期化に失敗しました: %s\n", SDL_GetError());
      free(systemdata->audio_buffer);
      exit(1);
    }
  
  systemdata->ms = audio_init();
  if(systemdata->ms == 0)
    {
      exit(1);
    }
}  

int main(int argc, char* argv[])
{
  meta_sys *meta;
  system_setup(meta);
  
  printf("サイン波 (%.0f Hz) の再生データを生成・キューに投入中...\n", FREQUENCY);
  
  while (meta->generated_samples < meta->total_samples_to_generate)
    {
      int samples_to_generate = meta->samples_per_buffer;
      if (meta->generated_samples + samples_to_generate > meta->total_samples_to_generate)
	{
	  samples_to_generate = total_samples_to_generate - generated_samples;
	}
      
      while (SDL_GetQueuedAudioSize(*meta->ms->dev) > MAX_QUEUE_SIZE * 0.8)
	{
	  SDL_Delay(10); 
	}
      
      generate_sine_wave(meta->audio_buffer, samples_to_generate, &meta->current_phase);
      
      size_t bytes_to_queue = samples_to_generate * sizeof(float);
      if (SDL_QueueAudio(*ms->dev, audio_buffer, bytes_to_queue) < 0)
	{
	  fprintf(stderr, "オーディオキューへのプッシュに失敗しました: %s\n", SDL_GetError());
	  break;
	}
      
      generated_samples += samples_to_generate;
    }
  
  SDL_PauseAudioDevice(*ms->dev, 0); // 0で再生、1で一時停止
  printf("再生を開始しました。\n");
  
  printf("再生キューが空になるのを待機中...\n");
  while (SDL_GetQueuedAudioSize(*ms->dev) > 0)
    {
      SDL_Delay(100); 
    }
  
  printf("再生が完了しました。\n");
  SDL_CloseAudioDevice(*ms->dev);    // デバイスを閉じる
  SDL_Quit();                   // SDLを終了
  free(audio_buffer);
  
  return 0;
}
