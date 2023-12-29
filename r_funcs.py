#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Aug 18 08:52:18 2023

@author: ntutland
"""

import os
os.environ['R_HOME'] = "/Library/Frameworks/R.framework/Resources"
from rpy2.robjects.packages import importr

utils = importr('utils')
base = importr('base')

# utils.chooseCRANmirror(ind=74)
# utils.install_packages('terra')

terra = importr('terra')

def terra_crop(rst_path, shp_path, out_path):
    rst = terra.rast(rst_path)
    vec = terra.vect(shp_path)
    crp = terra.crop(rst, vec)
    terra.writeRaster(crp,out_path,overwrite=True)
    print("\n** Raster cropped using R terra::crop **\n")
    