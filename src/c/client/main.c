#include "include.h"
#define SAMP 48000

typedef struct
{
  bool flag;
  bool exit;
  double wave[SAMP];
}shared_wave;

int main(void)
{
  //共有メモリを開く
  const char *shm_name = "/wave_translation's_memory";
  int shm_fd = shm_open(shm_name, O_RDWR, 0);
  if(shm_fd == -1)
    {
      perror("shm_open");
      exit(1);
    }

  size_t memory_size = sizeof(shared_wave);
  void *shm_addr = mmap(NULL, sizeof(shared_wave), PROT_READ | PROT_WRITE, MAP_SHARED, shm_fd, 0);
  if(shm_addr == MAP_FAILED)
    {
      perror("mmap");
      close(shm_fd);
      exit(EXIT_FAILURE);
    }
  shared_wave *wave_send = (shared_wave *)shm_addr;
 
  //音波の生成

  for(int ro = 0; ro < 3; ro++)
    {
      while(wave_send->flag == true)
	{
	  usleep(100);
	}
      
      for(int i = 0; i < SAMP; i++)
	{
	  double bairitu = (SAMP - (double)i) / SAMP;
	  wave_send->wave[i] = 0.8 * sin(2 * M_PI * 768 * ((double)i / SAMP));
	}
      
      wave_send->flag = true;
    }
  while(wave_send->flag == true)
    {
      usleep(100);
    }  
  wave_send->exit = true;
  
  //共有メモリを閉じる 
  if(munmap(shm_addr, memory_size) == -1)
    {
      perror("munmap");
    }

  if(close(shm_fd) == -1)
    {
      perror("close");
    }

  return 0;
}
