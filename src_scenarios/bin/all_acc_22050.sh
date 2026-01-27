#!/bin/bash
# convert all .aac file to 22050

shopt -s nullglob

for file in *.aac; do
	tmp="${file%.aac}.tmp.aac"
	if ffmpeg -y -hide_banner -loglevel error -i "$file" -c:a aac_at -ar 22050 -b:a 64k "$tmp"; then
		mv "$tmp" "$file"
	else
		echo "ffmpeg failed for $file, preserving original"
		rm -f "$tmp"
	fi
done