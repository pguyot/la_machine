# Flashing and Calibration

## Flashing a full image

Flash the complete image with esptool:

```bash
esptool.py --chip esp32c3 --port /dev/cu.usbmodem* write_flash 0 la_machine.img
```

The full image includes an empty NVS partition. Because the NVS is empty,
La Machine will enter a self-test and calibration sequence on first boot
instead of normal operation.

## Self-test and calibration procedure

After flashing, La Machine goes through three boot cycles before reaching
normal operation.

### Boot 1: Battery check and audio prompt

1. La Machine boots with a 5-second countdown.
2. It checks that the battery is either at 100% or actively charging.
   **The USB cable is probably plugged in at this point since it was used
   for flashing.**
3. It plays `_selftest/start.mp3` to signal the operator.
4. It goes to deep sleep, waiting up to 60 seconds for the button to be
   pressed.

### Boot 2: Servo calibration

**Before pressing the button, unplug the USB cable.** This ensures the
battery is tested without external power.

Make sure La Machine is positioned normally, with USB side facing down
and button side facing up.

Press the button within 60 seconds. La Machine will:

1. Measure battery voltage.
2. Check that the accelerometer responds and confirms La Machine is in its
   normal resting orientation (USB side down, button side up).
3. Power on the servo and run the calibration algorithm: it moves the servo
   through its range, finding the exact duty cycle where the arm contacts the
   button. From this measurement, it computes two values:
   - **closed_duty**: the servo position for the fully closed (resting) arm.
   - **interrupt_duty**: the servo position where the arm contacts the button.
4. Save the calibration result and battery reading to NVS.
5. Go to deep sleep for 5 seconds.

If the 60-second timer expires without a button press, the self-test is
recorded as failed.

### Boot 3: Audio and serial report

On the next boot, La Machine plays an audio report:

- **Two sounds** (`success_44kHz.mp3` then `success_48kHz.mp3`): calibration
  succeeded. Both sample rates are tested to validate audio playback.
- **One sound** (`failure.mp3`): calibration failed.

After the sound is played, La Machine also reports the self-test result and
calibration values on the serial port. It does so immediately in case of
success, and repeatedly for 10 seconds in case of failure, allowing the
operator to replug the USB cable if needed.

After the report, La Machine marks the self-test as complete and enters normal
operation on subsequent boots.

## Reading the self-test result after failure

If self-test failed, you can read the NVS partition to inspect the failure
reason:

```bash
esptool.py --chip esp32c3 --port /dev/cu.usbmodem* read_flash 0x9000 0x6000 nvs.bin
```

The NVS partition is at offset `0x9000` with a size of `0x6000` (24 KB). The
configuration contains the self-test result as a text string (e.g.
"Battery not charging (unplugged ?)", "Button OFF on initial wakeup",
"Button is initially off", "Accelerometer: not in resting position",
"Accelerometer not responding", "Unexpected 85% duty: ..."). This string is
typically found in the first bytes of the NVS partition.

You can also guess the failure cause based on when the failure sound is played:

- If the three beeps of the start sound are not played and the failure sound
  is played immediately, the problem may be with the battery not being
  detected as charging or at 100%.

- If the operator presses the button quickly after the three beeps but nothing
  happens until the failure sound is played after one minute, La Machine was
  not woken up by the button.

- If the failure sound plays shortly after pressing the button without moving
  the arm, the problem may be related to the accelerometer or to the battery
  voltage.

- If the arm moves and does not retract back into La Machine, the problem may
  be related to the servo range (it failed to turn off the button).

## Re-running the self-test

To re-run calibration, erase the NVS partition and reboot:

```bash
esptool.py --chip esp32c3 --port /dev/cu.usbmodem* erase_region 0x9000 0x6000
```

This puts La Machine back in the uncalibrated state, and the self-test
sequence starts again on next boot.
