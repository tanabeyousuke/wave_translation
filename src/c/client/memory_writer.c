#include "include.h"

void* share_open(shm_meta* metadata, size_t size, const char* name, const char* sem_name)
{
  metadata->name = name;
  
  int fd = shm_open(name, O_CREAT | O_RDWR, 0666);
  if(fd == -1)
    {
      exit(1);
    }
  metadata->fd = fd;
  
  metadata->mem = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
  if(metadata->mem == MAP_FAILED)
    {
      close(fd);
      exit(1);
    }

  metadata->sem = sem_open(sem_name, 0);
  if(metadata->sem == SEM_FAILED)
    {
      perror("semaphore");
      munmap(metadata->mem, metadata->size);
      close(metadata->fd);
      exit(1);
    }
  metadata->sem_name = sem_name;
  sem_post(metadata->sem);

  return metadata->mem; 
}

void* share_login(void)
{
  shm_meta* metadata = malloc(sizeof(shm_meta));
  size_t size = sizeof(share_memory);
  const char *name = "/wt_share_memory";
  const char *sem_name = "/wt_semaphore";

  share_open(metadata, size, name, sem_name);
  return metadata;
}

void share_close(shm_meta* metadata)
{
  share_memory* mem = metadata->mem;

  sem_wait(metadata->sem);
  for(int i = 0; i < SAMP; i++)
    {
      mem->wave[i] = 0;
    }
  mem->exit = true;
  sem_post(metadata->sem);

  munmap(metadata->mem, metadata->size);
  close(metadata->fd);
  sem_close(metadata->sem);
  free(metadata);
}

void memory_write(shm_meta* metadata, double* buffer)
{
  share_memory* memory = (share_memory *)metadata->mem;
  float* wave = memory->wave;

  sem_wait(metadata->sem);
  
  for(int i = 0; i < SAMP; i++)
    {
      wave[i] = (float)buffer[i];
    }

  sem_post(metadata->sem);
}

void* buffer_malloc(void)
{
  double* buffer = malloc(sizeof(double) * SAMP);
  return buffer;
}

