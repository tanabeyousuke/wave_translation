#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <semaphore.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdbool.h>

typedef struct {
  char bun[100];
  bool flag;
} share_memory;

typedef struct {
  size_t size;
  int fd;
  const char* name;
  sem_t *sem;
  const char* sem_name;
  void* mem;
} shm_meta;

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

  metadata->sem = sem_open(sem_name, O_CREAT, 0644, 1);
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


int main(void)
{
  shm_meta metadata;
  size_t size = sizeof(share_memory);
  const char *name = "/my_shared_memory";
  const char *sem_name = "/sem_name";

  share_memory* data = share_setting(&metadata, size, name, sem_name);

  usleep(100);

  sem_wait(metadata.sem);

  printf("%s\n", data->bun);

  share_close(&metadata);
  
  return 0;
}
  
