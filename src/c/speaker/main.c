#include "include.h"
#define SAMP 48000

typedef struct
{
  bool flag;
  bool exit;
  double wave[SAMP];
} shared_wave;

int main(void)
{
  //ここから共有メモリ関係

  const char *shm_name = "/wave_translation's_memory";
  int fd = shm_open(shm_name, O_CREAT | O_RDWR, 0666);
  if(fd == -1)
    {
      perror("shm_open");
      exit(1);
    }
  
  size_t shm_size = sizeof(shared_wave);
  if (ftruncate(fd, shm_size) == -1) 
    {
      perror("ftruncate");
      exit(1);
    }

  void *shm_addr = mmap(NULL, shm_size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
  if (shm_addr == MAP_FAILED)
    {
      perror("mmap");
      exit(1);
    }
  shared_wave *wave_receive = (shared_wave *)shm_addr;
  wave_receive->flag = false;
  wave_receive->exit = false;

  //ここまで共有メモリ関係、ここからpulseaudio

  pa_simple *ps;
  pa_sample_spec ss;
  int err;
  uint8_t *buffer;

  ss.format = PA_SAMPLE_S16LE;
  ss.rate = SAMP;
  ss.channels = 1;

  ps = pa_simple_new(NULL, "pulse_test", PA_STREAM_PLAYBACK, NULL, "pulse-test-stream", &ss, NULL, NULL, &err);

  if(!ps)
    {
      fprintf(stderr, "error: %d\n", err);
      return 1;
    }

  buffer = (uint8_t *)malloc(SAMP * 2);
  double v;
  int16_t n;

  while(wave_receive->exit == false)
    {
      while(wave_receive->flag == false)
	{
	  usleep(100);
	}
      
      for(int i = 0; i < SAMP; i++)
	{
	  //v = 0.8 * sin(2 * M_PI * 1000 * ((double)i / SAMP));
	  v = wave_receive->wave[i];
	  n = (int16_t)round(v * 32767);
	  
	  buffer[2 * i] = (uint8_t)n;
	  buffer[2 * i + 1] = n >> 8;
	}
      
      wave_receive->flag = false;
      pa_simple_write(ps, buffer, SAMP * 2, NULL);
    }

  //ここから店じまいの処理
  if (munmap(shm_addr, shm_size) == -1)
    {
      perror("munmap");
      exit(1);
    }
  
  close(fd);
  
  if(shm_unlink(shm_name) == -1)
    {
      perror("shm_unlink");
      exit(1);
    }

  /* free(buffer); */
  /* pa_simple_drain(ps, NULL); */

  /* pa_simple_free(ps); */
  return 0;
}
