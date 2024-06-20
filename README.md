Source code for La Machine
==========================

![Build](https://github.com/pguyot/la_machine/actions/workflows/build.yaml/badge.svg)

https://la-machine.fr/

https://github.com/pguyot/la_machine/assets/168407/3031f52c-ed63-48e6-9180-fe7e649e0025

La Machine software is written in Erlang and runs on [AtomVM](https://atomvm.net/)
virtual machine.

Flashing
--------

Please note that La Machine can only be flashed when it is not deep sleeping.
You can action its button to wake it up. In case of necessity, reboot and reset
buttons can eventually be used.

GitHub action builds an optimized image of La Machine including AtomVM virtual
machine with necessary codecs. It is artifact named `la_machine.img` of
[Build workflow](https://github.com/pguyot/la_machine/actions/workflows/build.yaml?query=branch%3Amain).

It can be flashed with ESP-IDF esptool on macOS as follows:

    esptool.py --chip esp32c3 --port /dev/cu.usbmodem* write_flash 0 ~/Downloads/la_machine.img

The port may vary on your platform.

This image differs from standard AtomVM deployments as Erlang code is in a
single partition (boot.avm) with only La Machine code. This means that you
should not use `rebar3 esp32_flash` with default parameters (see below).

Building and flashing La Machine with a pristine AtomVM build
-------------------------------------------------------------

You need to flash AtomVM virtual machine with its libraries and
[`atomvm_esp_adf`](https://github.com/pguyot/atomvm_esp_adf) driver (the sound
codecs). You can build AtomVM from source or use the latest esp32c3 build of
[`atomvm_esp_adf`'s build action on main branch](https://github.com/pguyot/atomvm_esp_adf/actions/workflows/build.yml?query=branch%3Amain).

This build can be flashed with the following command on macOS:

    esptool.py --chip esp32c3 --port /dev/cu.usbmodem* write_flash 0 ~/Downloads/atomvm-esp-adf-esp32c3-v5.2.1.img

To compile source code of La Machine, you need Erlang/OTP (a recent version
will do) and rebar3. With a single line, you can compile, pack and flash
La Machine code at default offset (`0x210000`):

    rebar3 atomvm esp32_flash -p /dev/cu.usbmodem*

Building and flashing La Machine with optimized AtomVM build
------------------------------------------------------------

Alternatively, you can use the optimized build of AtomVM above and flash
La Machine. The partition map differ from standard AtomVM deployments and
you need to pack La Machine code with AtomVM libraries. You can use either the
full package [`atomvmlib.avm`](https://github.com/atomvm/AtomVM/releases/download/v0.6.2/atomvmlib-v0.6.2.avm) or
only `estdlib.avm` and `eavmlib.avm`.

    rebar3 atomvm packbeam -p -e ~/Downloads/atomvmlib-v0.6.2.avm
    rebar3 atomvm esp32_flash -p /dev/cu.usbmodem* -o 0x130000
