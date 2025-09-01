typedef struct {
  bool exit;
  double wave[48000];
} share_memory;

typedef struct {
  size_t size;
  int fd;
  const char *name;
  sem_t *sem;
  const char* sem_name;
  void* mem;
} shm_meta;

void* share_setting(shm_meta* metadata, size_t size, const char* name, const char* sem_name);
void share_close(shm_meta* metadata);

