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
	 
pa_simple* audio_init(){	 
  pa_simple *ps;
  pa_sample_spec ss;
  int err;
  uint8_t *buf;
  
  //作成
  
  ss.format = PA_SAMPLE_S16LE;
  ss.rate = SAMPRATE;
  ss.channels = 1;
  
  ps = pa_simple_new(NULL, "pulse-test", PA_STREAM_PLAYBACK,
		     NULL, "pulse-test-stream", &ss, NULL, NULL, &err);
  
  if(!ps)
    {
      printf("error: %d\n", err);
      exit(1);
    }
}

void* make_buffer(int kata)
{
  int size;
  if(kata == 0)
    {
      size = sizeof(double) * SAMPRATE;
    }
  else
    {
      size = 4 * SAMPRATE;
    }
  void* buf = malloc(size);
  return buf;
}

void freeaudio(pa_simple* pa){
  printf("%d\n",pa); 
  pa_simple_free(pa);
}

void freebuffer(void *p)
{
  free(p);
}


void write_audio(pa_simple *ps,double *buffer,uint8_t *buf)
{
  int i;
  int16_t n;
  double d;
  uint8_t *ptr = (uint8_t *)buf;
  
  
  for(i = 0; i < SAMPRATE; i++, ptr += 2)
    {
      /* d = sin(i * PI * 2 * (1000.0 / SAMPRATE)) * 0.8; */
      d = buffer[i];
      n = (int16_t)round(d * 32767);
      
      //16bit LE
      ptr[0] = (uint8_t)n;
      ptr[1] = n >> 8;
    }
  
  
  printf("write..");
  fflush(stdout);
  
  pa_simple_write(ps, buf, SAMPRATE * 2, NULL);
  
  
  printf("done\n");
}
