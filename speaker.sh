#!/usr/bin/bash

cd src/c/speaker
gcc main.c memory_setting.c sound_output.c -lpulse -lpulse-simple -o speaker -l m
mv speaker ../../../
