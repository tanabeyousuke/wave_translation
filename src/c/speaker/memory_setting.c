#include "include.h"

void* share_setting(shm_meta* metadata, size_t size, const char* name, const char* sem_name)
{
  metadata->name = name;

  int fd = shm_open(name, O_CREAT | O_RDWR, 0666);
  if (fd == -1)
    {
      perror("shm_open");
      exit(1);
    }
  metadata->fd = fd;

  if(ftruncate(fd, size) == -1)
    {
      perror("ftruncate");
      close(fd);
      exit(1);
    }

  metadata->mem = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
  if(metadata->mem == MAP_FAILED)
    {
      perror("mmap");
      close(fd);
      exit(1);
    }

  metadata->sem = sem_open(sem_name, O_CREAT, 0644, 0);
  if(metadata->sem == SEM_FAILED)
    {
      perror("semaphore");
      munmap(metadata->mem, metadata->size);
      close(metadata->fd);
      shm_unlink(metadata->name);
      exit(1);
    }
  metadata->sem_name = sem_name;
  
  return metadata->mem;
}

void share_close(shm_meta* metadata)
{
  munmap(metadata->mem, metadata->size);
  close(metadata->fd);
  shm_unlink(metadata->name);
  sem_close(metadata->sem);
  sem_unlink(metadata->sem_name);
}
