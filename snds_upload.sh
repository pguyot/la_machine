#!/bin/bash

~/Library/Arduino15/packages/esp32/tools/esptool_py/5.1.0/esptool --chip esp32c3 --port /dev/cu.usbmodem101 write_flash 0x230000 _build/generated/sounds.bin
