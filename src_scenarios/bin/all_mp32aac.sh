#!/bin/bash
# convert all .mp3 file to aac
# ffmpeg -y -hide_banner -loglevel error -i 0110.mp3 -c:a aac_at -ar 22050 -b:a 64k 0110.aac

for file in *.mp3; do ffmpeg -y -hide_banner -loglevel error -i "$file" -c:a aac_at -ar 22050 -b:a 64k "${file%.mp3}.aac"; done   