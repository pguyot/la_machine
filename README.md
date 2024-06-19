Source code for La Machine
=====

https://la-machine.fr/

https://github.com/pguyot/la_machine/assets/168407/3031f52c-ed63-48e6-9180-fe7e649e0025

Requirements
-----

La Machine is an Erlang application running [AtomVM](https://atomvm.net/).
To compile it, you need Erlang/OTP (a recent version will do) and rebar3.

You will also need to flash AtomVM virtual machine with its libraries and with
[`atomvm_esp_adf`](https://github.com/pguyot/atomvm_esp_adf) driver.

Build and flash
-----

This source code can then be compiled and flashed with rebar3.

The following command should work on macOS, please adjust the serial port name
on other Unix.

    $ rebar3 atomvm esp32_flash -p /dev/cu.usbmodem*

Please note that La Machine can be flashed when it is not deep sleeping. It can
be flashed while playing a sound, simply wake it up by pressing the button. In
the worst case scenario, reboot and reset buttons can be used.
