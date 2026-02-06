#!/bin/bash

# Script to continuously scan for USB modem devices until one is found

echo "Scanning for machine and download when found..."
echo "Press Ctrl+C to stop scanning."

# Keep scanning until a device is found
while true; do
    device=$(find /dev -name "tty.usbmodem*" | head -n 1) # cu.usbmodem
    
    # Check if a device was found
    if [ -n "$device" ]; then
        echo "Found machine device: $device"

        ~/Library/Arduino15/packages/esp32/tools/esptool_py/5.1.0/esptool --port $device read_flash 0x0 0x800000 la_machine_moods_as_$(date +"%d-%m-%Y").img

        exit 0
    else
        echo "No machine found. Scanning again in 1 seconds..."
        sleep 1
    fi
done


