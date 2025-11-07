#ifndef INCLUDE
#define INCLUDE

#include <SDL2/SDL.h>
#include <stdio.h>
#include <stdlib.h> // malloc, free, exit のために必要
#include <math.h>

#include "server.h"

#define SAMPLE_RATE 44100      // サンプリングレート (Hz)
#define FREQUENCY 440.0        // 再生する音の周波数 (Hz)
#define VOLUME 0.2             // 音量 (0.0 から 1.0)
#define BUFFER_SIZE_MS 100     // 一度にキューに投入するデータの時間 (ミリ秒)

#define MAX_QUEUE_DURATION_SECONDS 10
#define MAX_QUEUE_SIZE (SAMPLE_RATE * sizeof(float) * 1 * MAX_QUEUE_DURATION_SECONDS) 

#endif

