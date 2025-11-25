#!/bin/bash
# convert all .aac file to 22050

for file in *.aac; do
	mv "$file" "${file%.aac}2.aac"
	ffmpeg -y -hide_banner -loglevel error -i "${file%.aac}2.aac" -c:a aac_at -ar 22050 -b:a 64k "$file"
	rm "${file%.aac}2.aac"
done   