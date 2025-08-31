typedef struct{
  pa_simple *ps;
  pa_sample_spec ss;
  int err;
} pa_simple_meta;

void sound_init(pa_simple_meta* metadata);
void sound_exit(pa_simple_meta* metadata);
