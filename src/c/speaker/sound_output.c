#include "include.h"

void sound_init(pa_simple_meta* metadata)
{
  metadata->ss.format = PA_SAMPLE_FLOAT32LE;
  metadata->ss.rate = 48000;
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
