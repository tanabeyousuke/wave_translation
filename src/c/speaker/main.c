#include "include.h"
#define SAMP 48000

int main(void)
{
  pa_simple_meta pa_meta;
  shm_meta share_meta;
  share_memory *share_data;

  size_t size = sizeof(share_memory);
  const char *name = "/wt_share_memory";
  const char *sem_name = "/wt_semaphore";
  share_data = share_setting(&share_meta, size, name, sem_name); 
  
  sound_init(&pa_meta);

  while(share_data->exit == false)
    {
      sem_wait(share_meta.sem);

      pa_simple_write(pa_meta.ps, share_data->wave, sizeof(float) * SAMP, NULL);
      for(int i = 0; i < SAMP; i++)
	share_data->wave[i] = 0;
      sem_post(share_meta.sem);
    }

  sound_exit(&pa_meta);
  share_close(&share_meta);
  return 0;
}

