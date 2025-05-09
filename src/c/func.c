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

intptr_t newaudio(){
  int pa_errno, pa_result, read_bytes;
  
  pa_sample_spec ss;
  ss.format = PA_SAMPLE_S16LE;
  ss.rate = 48000;
  ss.channels = 1;
  
  pa_simple* pa = pa_simple_new(NULL, APP_NAME, PA_STREAM_PLAYBACK, NULL, STREAM_NAME, &ss, NULL, NULL, &pa_errno);
  printf("%d\n",pa);
  return (intptr_t)pa;
}
  
void freeaudio(intptr_t pa){
  printf("%d\n",pa); 
  pa_simple_free((pa_simple*)pa);
}
