#include "include.h"
#define SAMP 48000

typedef struct
{
  bool flag;
  bool exit;
  double wave[SAMP];
}shared_wave;

typedef struct
{
  int shm_fd;
  size_t memory_size;
  void* shm_addr;
}shm_data;

shm_data* memory_open(void)
{
  shm_data* shm = malloc(sizeof(shm_data));

  const char *shm_name = "/wave_translation's memory";
  shm->shm_fd = shm_open(shm_name, O_RDWR, 0);
  if(shm->shm_fd == -1)
    {
      perror("shm_open");
      exit(1);
    }

  shm->memory_size = sizeof(shared_wave);
  shm->shm_addr = mmap(NULL, shm->memory_size, PROT_READ | PROT_WRITE, MAP_SHARED, shm->shm_fd, 0);
  if(shm->shm_addr == MAP_FAILED)
    {
      perror("mmap");
      close(shm->shm_fd);
      exit(EXIT_FAILURE);
    }

  return shm;
}

void* shm_address(shm_data* shm)
{
  return shm->shm_addr;
}

void memory_close(shm_data* shm)
{
  shared_wave *a = (shared_wave *)shm->shm_addr;
  a->exit = true;

  if(munmap(shm->shm_addr, shm->memory_size) == -1)
    {
      perror("munmap");
    }
  
  if(close(shm->shm_fd) == -1)
    {
      perror("close");
    }

  free(shm);
}
