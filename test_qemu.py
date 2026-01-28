#
# This file is part of La Machine
#
# Copyright 2024 Paul Guyot <pguyot@kallisys.net>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# SPDX-License-Identifier: Apache-2.0
#

from pytest_embedded import Dut
import pytest
import os
import shutil
from esptool import merge_bin
from collections import namedtuple


def create_flash_image():
    path = "flash_image.bin"
    args = namedtuple(
        "args",
        "chip addr_filename format output fill_flash_size flash_mode flash_size flash_freq target_offset",
    )(
        **{
            "chip": "esp32c3",
            "addr_filename": [
                (0x0, open("image/build/bootloader/bootloader.bin", "rb")),
                (0x8000, open("image/partitions-tests.bin", "rb")),
                (0x10000, open("image/build/atomvm-esp32.bin", "rb")),
                (0x130000, open("image/la_machine_tests.avm", "rb")),
                (0x270000, open("image/sounds.bin", "rb")),
            ],
            "format": "raw",
            "output": path,
            "fill_flash_size": "16MB",
            "flash_mode": "dio",
            "flash_size": "16MB",
            "flash_freq": "keep",
            "target_offset": 0,
        }
    )
    merge_bin(args)


def pytest_generate_tests(metafunc):
    create_flash_image()
    metafunc.parametrize(
        "embedded_services, qemu_prog_path, qemu_cli_args, qemu_image_path, qemu_extra_args",
        [
            (
                "qemu",
                shutil.which("qemu-system-riscv32"),
                "-nographic -machine esp32c3",
                "flash_image.bin",
                "-nic user,model=open_eth -drive file=image/test/qemu_esp32c3_efuse.bin,if=none,format=raw,id=efuse -global driver=nvram.esp32c3.efuse,property=drive,value=efuse",
            )
        ],
        indirect=True,
    )


def test_atomvm_esp_adf(dut, redirect):
    dut.expect_unity_test_output(timeout=40)
    assert len(dut.testsuite.testcases) > 0
    assert dut.testsuite.attrs["failures"] == 0
    assert dut.testsuite.attrs["errors"] == 0
