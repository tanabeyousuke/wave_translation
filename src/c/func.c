#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <math.h>
#include <pulse/error.h>  /* pulseaudio */
#include <pulse/simple.h> /* pulseaudio */

#define APP_NAME "pulseaudio_sample"
#define STREAM_NAME "play"
#define DATA_SIZE 1024
#define SAMPRATE 48000
#define PI 3.141592653

pa_simple* newaudio(){
  int pa_errno, pa_result, read_bytes;
  
  pa_sample_spec ss;
  ss.format = PA_SAMPLE_S16LE;
  ss.rate = 48000;
  ss.channels = 1;
  
  pa_simple* pa = pa_simple_new(NULL, APP_NAME, PA_STREAM_PLAYBACK, NULL, STREAM_NAME, &ss, NULL, NULL, &pa_errno);
  printf("%d\n", pa);
  return pa;
}
  
void freeaudio(pa_simple* pa){
  printf("%d\n",pa); 
  pa_simple_free(pa);
}


void write_audio(pa_simple *pa, double freq){
    int i;
    int16_t n;
    double d;
    uint8_t *buf;
    buf = (uint8_t *)malloc(SAMPRATE * 2);
    uint8_t *ptr = (uint8_t *)buf;

    for(i = 0; i < SAMPRATE; i++, ptr += 2)
    {
        d = sin(2 * PI * freq * ((double)i / SAMPRATE)) * 0.8;
        n = (int16_t)round(d * 32767);

        //16bit LE
        ptr[0] = (uint8_t)n;
        ptr[1] = n >> 8;
    }

    printf("write..");
    fflush(stdout);

    pa_simple_write(pa, buf, SAMPRATE * 2, NULL);

    printf("done\n");

    free(buf);
}
