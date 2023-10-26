#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jul 20 10:14:09 2023

@author: ntutland
"""

import os
os.environ["FASTFUELS_API_KEY"] = "sxk-b78b909a-383c-4972-b480-749f9f926a4b"
import fastfuels_sdk as ff
import geopandas as gpd
import json
import zarr
import TTRS_QUICFire_Support as ttrs
import math
import sys

OG_PATH = os.getcwd()
shp_name = "Chips_bounds.shp"
fgrid_name = "fuelgrid.zip"
fgrid_mutable = "fuelgrid_mutable.zarr"
qf_path = os.path.join(OG_PATH,"ff_out")
shp_path = os.path.join(OG_PATH,"Sample_Sites",shp_name)
fgrid_path = os.path.join(OG_PATH,fgrid_name)
fgrid_mutable_path = os.path.join(OG_PATH,fgrid_mutable)
duet_path = os.path.join(OG_PATH,"Duet")

# Load a spatial data file
gdf = gpd.read_file(shp_path).to_crs(5070)
geojson = json.loads(gdf.to_json())

# Create a dataset
dataset = ff.create_dataset(name="Chips",
                            description="Dixie Fire - sample site 1",
                            spatial_data=geojson)

# Create a treelist from a dataset
treelist = dataset.create_treelist(name="Chips",
                                   description="Dixie Fire - sample site 1")

# Wait for a treelist to finish generating
treelist.wait_until_finished(verbose=True)

# Create a fuelgrid from a treelist
fuelgrid = treelist.create_fuelgrid(name="Chips",
                                    description="Dixie Fire - sample site 1",
                                    distribution_method="realistic",
                                    horizontal_resolution=2,
                                    vertical_resolution=1,
                                    border_pad=0)

# Wait for a fuelgrid to finish generating
fuelgrid.wait_until_finished(verbose=True)

# Download the Fuelgrid zarr data
fuelgrid.download_zarr(fgrid_path)

# Load the immutable zarr store
zarr_immutable = zarr.open(fgrid_path, mode='r')

# Create a mutable zarr store
zarr_mutable = zarr.open(fgrid_mutable_path, mode='w')

# Copy the data from the immutable zarr store to the mutable zarr store
zarr.copy_all(zarr_immutable, zarr_mutable)

ff.export_zarr_to_duet(zarr_immutable, duet_path, seed=47, wind_dir=180, wind_var=360, duration=5)
ff.export_zarr_to_quicfire(zarr_immutable, qf_path)

sys.path.insert(0,"/Users/ntutland/Documents/Projects/fastfuels-sdk-python/fastfuels_sdk")
import exports as exp
import numpy as np

dc = exp.DuetCalibrator(zarr_immutable, duet_path, param_dir="/Users/ntutland/Documents/Projects/DUET_Calibration")
dc.calibrate_max_min("litter", 5.0, 1.0)
dc.calibrate_with_sb40("grass")

exp.calibrate_duet(zarr_immutable, duet_path, 
                   param_dir="/Users/ntutland/Documents/Projects/DUET_Calibration", 
                   litter_max = 5.0, litter_min = 1.0,
                   keep_duet = "grass")


def print_factors(x):
   print("The factors of",x,"are:")
   for i in range(1, x + 1):
       if x % i == 0:
           print(i)


print_factors(4826304)

cell_nums = (252,252,76)
dims = (2,252,252)
rhof = ttrs.import_fortran_dat_file("ff_out/treesrhof.dat", cell_nums, order="C")
# terr = ttrs.import_topo_dat_file("ff_out/topo.dat", cell_nums)
# duet = ttrs.import_fortran_dat_file("Duet/surface_rhof.dat", cell_nums)
duet = exp._read_dat_file("Duet","surface_rhof.dat",dims, order = "F")
calib = exp._read_dat_file("Duet","surface_rhof_calibrated.dat", (252,252), order = "F")
# duet_depth = ttrs.import_topo_dat_file("Duet/surface_depth.dat", cell_nums)
ttrs.plot_array(rhof[0,:,:], "rhof")
# ttrs.plot_array(terr, "terrain")
ttrs.plot_array(dc.original_duet_array[0,:,:], "duet rhof")
ttrs.plot_array(dc.calibrated_array[1,:,:], "duet calibrated")
ttrs.plot_array(np.add(dc.calibrated_array[0,:,:],dc.calibrated_array[1,:,:]), "new method")
ttrs.plot_array(calib, "old method")
ttrs.plot_array(np.add(dc.calibrated_array[0,:,:],dc.calibrated_array[1,:,:]),"duet total")
# ttrs.plot_array(duet_depth,"duet depth")

#####
treelist_df = treelist.get_data()
X = treelist_df['X_m'].sample(n=1).iloc[0]
Y = treelist_df['Y_m'].sample(n=1).iloc[0]

trees_in_plot = treelist_df[(abs(treelist_df['X_m']-X)<10) & (abs(treelist_df['Y_m']-Y)<10)]

len(trees_in_plot.index)

import geojson
zroot = zarr_mutable
poly = geojson.Polygon(
    coordinates=[
        [   
            [zroot.attrs['xmin'], zroot.attrs['ymin']],
            [zroot.attrs['xmin'], zroot.attrs['ymax']],
            [zroot.attrs['xmax'], zroot.attrs['ymax']],
            [zroot.attrs['xmax'], zroot.attrs['ymin']],
            [zroot.attrs['xmin'], zroot.attrs['ymin']],
        ]
        ],
    precision=8,
    )
import numpy as np

