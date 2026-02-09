#!/bin/bash

MAX_RETRIES=60
retries=0

# Keep scanning until a device is found
while true; do
    device=$(find /dev -name "tty.usbmodem*" | head -n 1) # cu.usbmodem

    # Check if a device was found
    if [ -n "$device" ]; then
        echo "Found machine device: $device"

        ~/Library/Arduino15/packages/esp32/tools/esptool_py/5.1.0/esptool --chip esp32c3 --port "$device" write_flash 0x230000 _build/generated/sounds.bin
        exit $?
    else
        retries=$((retries + 1))
        if [ "$retries" -ge "$MAX_RETRIES" ]; then
            echo "No machine found after ${MAX_RETRIES} attempts. Giving up."
            exit 1
        fi
        echo "No machine found. Scanning again in 1 second... (attempt ${retries}/${MAX_RETRIES})"
        sleep 1
    fi
done


