#!/bin/bash

# Keep scanning until a device is found
while true; do
    device=$(find /dev -name "tty.usbmodem*" | head -n 1) # cu.usbmodem
    
    # Check if a device was found
    if [ -n "$device" ]; then
        echo "Found machine device: $device"

              ~/Library/Arduino15/packages/esp32/tools/esptool_py/5.1.0/esptool --chip esp32c3 --port $device write_flash 0x230000 _build/generated/sounds.bin

        exit 0
    else
        echo "No machine found. Scanning again in 1 seconds..."
        sleep 1
    fi
done


