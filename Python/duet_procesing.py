#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Oct  9 16:45:42 2023

@author: ntutland
"""

import os
import sys
import subprocess
import shutil
from time import sleep
sys.path.insert(0,"/Users/ntutland/Documents/Projects/fastfuels-sdk-python/fastfuels_sdk")
import exports as exp 

def get_surface_fuels(duet_path, ff_path, zarr, param_path):
    run_duet(ff_path,duet_path)
    calibrate_duet(zarr,duet_path,ff_path,param_path)

def run_duet(qf_path,duet_path):
    shutil.copytree(qf_path, duet_path, dirs_exist_ok = True)
    os.chdir(duet_path)
    with subprocess.Popen(
        ["./duet"], stdout=subprocess.PIPE
    ) as process:

        def poll_and_read():
            print(f"{process.stdout.read1().decode('utf-8')}")

        while process.poll() != 0:
            poll_and_read()
            sleep(1)
        if process.poll()==0:
            print('DUET run successfully')

def calibrate_duet(zroot,duet_path,qf_path,param_path):
    dc = exp.DuetCalibrator(zroot,duet_path,param_path)
    dc.calibrate_with_sb40(["grass","litter"])
    dc.to_file()
    dc.replace_quicfire_surface_fuels()