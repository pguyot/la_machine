#!/bin/bash
# ffmpeg -y -hide_banner -loglevel error -i 01salusalucebarbara.aac -c:a libmp3lame -qscale:a 2 01salusalucebarbara.mp3

for file in *.aac; do ffmpeg -y -hide_banner -loglevel error -i "$file" -c:a libmp3lame -qscale:a 2 "${file%.}.mp3"; done   