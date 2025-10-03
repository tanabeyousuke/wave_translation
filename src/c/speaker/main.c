#include "include.h"
#define SAMP 48000

int main(void)
{
  pa_simple_meta pa_meta;
  shm_meta share_meta;
  share_memory *share_data;
  float* wave_output;

  size_t size = sizeof(share_memory);
  const char *name = "/wt_share_memory";
  const char *sem_name = "/wt_semaphore";
  share_data = share_setting(&share_meta, size, name, sem_name); 
  share_data->exit == true;

  sound_init(&pa_meta);

  wave_output = malloc(sizeof(float) * SAMP);

  while(share_data->exit == false)
    {
      sem_wait(share_meta.sem);
      
      for(int i = 0; i < 48000; i++)
	{
	  wave_output[i] = share_data->wave[i];
	  printf("%f\n", wave_output[i]);
	}
      sem_post(share_meta.sem);

      pa_simple_write(pa_meta.ps, wave_output, sizeof(float) * SAMP, NULL);
    }

  sound_exit(&pa_meta);
  share_close(&share_meta);
  return 0;
}

