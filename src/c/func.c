#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <pulse/error.h>  /* pulseaudio */
#include <pulse/simple.h> /* pulseaudio */

# define APP_NAME "pulseaudio_sample"
# define STREAM_NAME "play"
# define DATA_SIZE 1024

void newaudio(pa_simple* pa){
  int pa_errno, pa_result, read_bytes;
  
  pa_sample_spec ss;
  ss.format = PA_SAMPLE_S16LE;
  ss.rate = 48000;
  ss.channels = 1;
  
  pa = pa_simple_new(NULL, APP_NAME, PA_STREAM_PLAYBACK, NULL, STREAM_NAME, &ss, NULL, NULL, &pa_errno);
  if(pa == NULL){
    fprintf(stderr, "error: setup failed\n");
    exit(1);
  }
}
  
void freeaudio(pa_simple* pa){
  pa_simple_free(pa);
}
