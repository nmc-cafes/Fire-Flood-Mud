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


OG_PATH = os.getcwd()
shp_name = "CCRPIPO_bounds.shp"
fgrid_name = "fuelgrid.zip"
qf_path = os.path.join(OG_PATH,"ff_out")
shp_path = os.path.join(OG_PATH,shp_name)
fgrid_path = os.path.join(OG_PATH,fgrid_name)

# Load a spatial data file
gdf = gpd.read_file(shp_path).to_crs(5070)
geojson = json.loads(gdf.to_json())

# Create a dataset
dataset = ff.create_dataset(name="CCRPIPO_dataset",
                            description="SEKI Demog - Crystal Cave Road",
                            spatial_data=geojson)

# Create a treelist from a dataset
treelist = dataset.create_treelist(name="CCRPIPO_treelist",
                                   description="Crystal Cave Road - no field data")

# Wait for a treelist to finish generating
treelist.wait_until_finished(verbose=True)

# Create a fuelgrid from a treelist
fuelgrid = treelist.create_fuelgrid(name="CCRPIPO_fuelgrid",
                                    description="MCrystal Cave Road - no field data",
                                    distribution_method="realistic",
                                    horizontal_resolution=2,
                                    vertical_resolution=1,
                                    border_pad=0)

# Wait for a fuelgrid to finish generating
fuelgrid.wait_until_finished(verbose=True)

# Download the Fuelgrid zarr data
fuelgrid.download_zarr(fgrid_path)

# Export the Fuelgrid zarr data to QUIC-fire inputs
zroot = zarr.open(fgrid_path, mode='r')
ff.export_zarr_to_quicfire(zroot, qf_path)

# Look at fuels
tb = gdf.total_bounds
xlen = math.ceil(tb[2]-tb[0])
ylen = math.ceil(tb[3]-tb[1])
nx = xlen/2
ny = (ylen+1)/2

def print_factors(x):
   print("The factors of",x,"are:")
   for i in range(1, x + 1):
       if x % i == 0:
           print(i)

print_factors(304920)

cell_nums = [55,84,66]
rhof = ttrs.import_fortran_dat_file("ff_out/treesrhof.dat", cell_nums)
ttrs.plot_array(rhof[1,:,:], "rhof")
