#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <semaphore.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdbool.h>
#include <string.h>

typedef struct {
  char bun[100];
  bool flag;
} share_memory;

typedef struct {
  size_t size;
  int fd;
  const char* name;
  sem_t* sem;
  const char* sem_name;
  void* mem;
} shm_meta;

void* share_open(shm_meta* metadata, size_t size, const char* name, const char *sem_name)
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

  return metadata->mem; 
}

void share_close(shm_meta* metadata)
{
  munmap(metadata->mem, metadata->size);
  close(metadata->fd);
  sem_close(metadata->sem);
}

int main(void)
{
  shm_meta metadata;
  size_t size = sizeof(share_memory);
  const char *name  = "/my_shared_memory";
  const char *sem_name = "/sem_name";

  share_memory* data = share_open(&metadata, size, name, sem_name);
  
  const char *message = "bokuwa ongakuka dentaku katateni";

  sem_wait(metadata.sem);
  strcpy(data->bun, message);
  sem_post(metadata.sem);
  share_close(&metadata);
  
  return 0;
}
  
