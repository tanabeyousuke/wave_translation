#ifndef MEMORY_WRITER
#define MEMORY_WRITER

#include "config.h"
typedef struct{
  bool exit;
  float wave[SAMP];
}share_memory;

typedef struct {
  size_t size;
  int fd;
  const char *name;
  sem_t* sem;
  const char* sem_name;
  void* mem;
} shm_meta;

//c側で呼び出す奴ら
void* share_open(shm_meta* metadata, size_t size, const char* name, const char* sem_name);

//fortran側で呼び出す奴ら
void* share_login(void);
void share_close(shm_meta* metadata);
void* buffer_malloc(void);
void memory_write(shm_meta* metadata, double* buffer);

#endif
