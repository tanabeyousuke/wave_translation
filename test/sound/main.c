#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <pulse/simple.h>

#define SAMP 48000
#define PI 3.1415927

typedef struct{
  pa_simple *ps;
  pa_sample_spec ss;
  int err;
} pa_simple_meta;

void sound_init(pa_simple_meta* metadata)
{
  metadata->ss.format = PA_SAMPLE_FLOAT32LE;
  metadata->ss.rate = SAMP;
  metadata->ss.channels = 1;

  metadata->ps = pa_simple_new(NULL, "pulse_test", PA_STREAM_PLAYBACK, NULL, "pulse-test-stream", &(metadata->ss), NULL, NULL, &(metadata->err));

  if(!(metadata->ps))
    {
      printf("error:%d\n", metadata->err);
      exit(1);
    }
}

void sound_exit(pa_simple_meta* metadata)
{
  pa_simple_drain(metadata->ps, NULL);
  pa_simple_free(metadata->ps);
}

int main(void)
{
  pa_simple_meta metadata;
  sound_init(&metadata);

  float *buf;
  buf = (float *)malloc(sizeof(float) * SAMP); 

  double d;
  for(int i = 0; i < SAMP; i++)
    {
      d = sin(2 * PI * 1000 * ((double)i / SAMP)) * 0.8;
      buf[i] = (float)d;
    }

  pa_simple_write(metadata.ps, buf, sizeof(float) * SAMP, NULL);
  free(buf);

  sound_exit(&metadata);
  return 0;
}
