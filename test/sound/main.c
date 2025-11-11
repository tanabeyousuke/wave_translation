#include <SDL2/SDL.h>
#include <stdio.h>
#include <stdlib.h> // malloc, free, exit のために必要
#include <math.h>

#define SAMPLE_RATE 44100      // サンプリングレート (Hz)
#define FREQUENCY 440.0        // 再生する音の周波数 (Hz)
#define VOLUME 0.2             // 音量 (0.0 から 1.0)
#define BUFFER_SIZE_MS 100     // 一度にキューに投入するデータの時間 (ミリ秒)

#define MAX_QUEUE_DURATION_SECONDS 10
#define MAX_QUEUE_SIZE (SAMPLE_RATE * sizeof(float) * 1 * MAX_QUEUE_DURATION_SECONDS) 


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

// ========== サイン波生成関数 ==========
void generate_sine_wave(float* buffer, int num_samples, double* current_phase, float f)
{
  // 2 * π * 周波数 / サンプリングレート
  double phase_increment = 2.0 * M_PI * f / SAMPLE_RATE;
  
  for(int i = 0; i < num_samples; ++i)
    {
      // サイン波を生成し、音量をかける
      buffer[i] = sin(*current_phase) * VOLUME;
      
      // 位相を進める
      *current_phase += phase_increment;
      
      // 位相を 0 から 2π の範囲に保つ
      if(*current_phase >= 2.0 * M_PI)
	{
	  *current_phase -= 2.0 * M_PI;
	}
    }
}

// ========== SDLオーディオ初期化 ==========
meta_sdl* audio_init(void)
{
  meta_sdl *init = (meta_sdl*)malloc(sizeof(meta_sdl));
  if (!init) return NULL;
  
  init->want = (SDL_AudioSpec*)malloc(sizeof(SDL_AudioSpec));
  init->have = (SDL_AudioSpec*)malloc(sizeof(SDL_AudioSpec));
  init->dev  = (SDL_AudioDeviceID*)malloc(sizeof(SDL_AudioDeviceID));
  
  if (!init->want || !init->have || !init->dev)
    {
      fprintf(stderr, "SDL初期化のためのメモリ確保に失敗しました。\n");
      free(init->want); free(init->have); free(init->dev); free(init);
      return NULL;
    }
  
  SDL_zero(*init->want); // 構造体をゼロクリア
  
  init->want->freq = SAMPLE_RATE;             // サンプリングレート
  init->want->format = AUDIO_F32SYS;          // 32bit浮動小数点数フォーマット
  init->want->channels = 1;                   // モノラル
  init->want->samples = 0;                    // コールバックを使用しないため 0
  
  // デバイスを開く (NULLでデフォルトデバイス、0でプッシュ方式)
  *init->dev = SDL_OpenAudioDevice(NULL, 0, init->want, init->have, SDL_AUDIO_ALLOW_ANY_CHANGE);
  
  if (*init->dev == 0)
    {
        fprintf(stderr, "オーディオデバイスのオープンに失敗しました: %s\n", SDL_GetError());
        free(init->want); free(init->have); free(init->dev); free(init);
        return NULL;
    }
  return init;
}

// ========== システム全体初期化 ==========
meta_sys* system_setup(void)
{
    // SDLの初期化はオーディオデバイスを開く前に行う
    if (SDL_Init(SDL_INIT_AUDIO | SDL_INIT_EVENTS) < 0)
      {
        fprintf(stderr, "SDLの初期化に失敗しました: %s\n", SDL_GetError());
        return NULL;
      }
    
    meta_sys *systemdata = (meta_sys*)malloc(sizeof(meta_sys));
    if (!systemdata)
      {
        fprintf(stderr, "システム構造体のメモリ確保に失敗しました。\n");
        SDL_Quit();
        return NULL;
      }
    
    systemdata->current_phase = 0.0;
    systemdata->running = 0; // 初期状態では停止
    
    // バッファのサイズ計算
    systemdata->samples_per_buffer = (SAMPLE_RATE * BUFFER_SIZE_MS) / 1000;
    systemdata->buffer_size_bytes = systemdata->samples_per_buffer * sizeof(float);
    systemdata->audio_buffer = (float*)malloc(systemdata->buffer_size_bytes);
    
    if (systemdata->audio_buffer == NULL)
      {
        fprintf(stderr, "オーディオバッファのメモリ確保に失敗しました。\n");
        free(systemdata);
        SDL_Quit();
        return NULL;
      }
    
    systemdata->ms = audio_init();
    if(systemdata->ms == NULL)
      {
        free(systemdata->audio_buffer);
        free(systemdata);
        SDL_Quit();
        return NULL;
      }
    return systemdata; 
} 

// ========== オーディオキューへのデータ投入 ==========
void sound_write(meta_sys *meta)
{
  size_t bytes_to_queue = meta->buffer_size_bytes;
  if (SDL_QueueAudio(*meta->ms->dev, meta->audio_buffer, bytes_to_queue) < 0)
    {
      fprintf(stderr, "オーディオキューへのプッシュに失敗しました: %s\n", SDL_GetError());
      meta->running = 0; // 失敗したら終了
    }
}

// ========== クリーンアップ処理 ==========
void system_cleanup(meta_sys *meta)
{
  if (meta)
    {
      if (meta->ms)
	{
	  // デバイスを閉じる
	  if (*meta->ms->dev != 0)
	    {
	      SDL_CloseAudioDevice(*meta->ms->dev);
            }
	  free(meta->ms->want);
	  free(meta->ms->have);
	  free(meta->ms->dev);
	  free(meta->ms);
        }
      free(meta->audio_buffer);
      free(meta);
    }
  // SDLを終了
  SDL_Quit();
} 

// ========== メイン関数 ==========
int main(int argc, char* argv[])
{
    meta_sys *meta = system_setup();
    if (meta == NULL) return 1;

    SDL_Event event;
    
    meta->running = 1;

    for(int i = 0; i < 5 && meta->running; ++i) {
        sound_write(meta); 
    }
    
    SDL_PauseAudioDevice(*meta->ms->dev, 0); 
    
    int i = 0;
    while (1)
      {
	i = i + 1;
	if(i == 1000)
	  {
	    printf("end\n");
	    break;
	  }
	
	generate_sine_wave(meta->audio_buffer, meta->samples_per_buffer, &meta->current_phase, 330 + i);
	
	Uint32 queued_size = SDL_GetQueuedAudioSize(*meta->ms->dev);
	printf("%d\n", queued_size);
	while(queued_size < (MAX_QUEUE_SIZE / 5) && queued_size < (meta->buffer_size_bytes * 5))
	  {
	    SDL_Delay(1);
	  }
	
	sound_write(meta);
      }
    
    printf("再生キューに残ったデータがなくなるのを待機中...\n");

    int size;
    do
      {
	size = SDL_GetQueuedAudioSize(*meta->ms->dev);
	
	printf("%d/", size); 
	SDL_Delay(100); 
      }
    while (size > 0);

     
    // 終了時の処理
    SDL_PauseAudioDevice(*meta->ms->dev, 1); // 一時停止
    
    printf("再生が完了し、終了します。\n");

    system_cleanup(meta);

    return 0;
}
