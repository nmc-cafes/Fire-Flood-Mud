#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Oct  9 16:18:21 2023

@author: ntutland
"""

import os
import json
import zarr
import geopandas as gpd
os.environ["FASTFUELS_API_KEY"] = "sxk-b78b909a-383c-4972-b480-749f9f926a4b"
import fastfuels_sdk as ff

def run_fastfuels(site_path,site_name,fire_name,ff_path,duet_path,pad=0):
    shp_name = site_name+"_bounds.shp"
    fgrid_name = site_name+"_fuelgrid.zip"
    shp_path = os.path.join(site_path,shp_name)
    fgrid_path = os.path.join(site_path,fgrid_name)

    # Load a spatial data file
    gdf = gpd.read_file(shp_path).to_crs(5070)
    geojson = json.loads(gdf.to_json())

    # Create a dataset
    dataset = ff.create_dataset(name=site_name,
                                description=fire_name+" Fire",
                                spatial_data=geojson)

    # Create a treelist from a dataset
    treelist = dataset.create_treelist(name=site_name,
                                       description=fire_name+" Fire")

    # Wait for a treelist to finish generating
    treelist.wait_until_finished(verbose=True)

    # Create a fuelgrid from a treelist
    fuelgrid = treelist.create_fuelgrid(name=site_name,
                                        description=fire_name+" Fire",
                                        distribution_method="realistic",
                                        horizontal_resolution=2,
                                        vertical_resolution=1,
                                        border_pad=pad)

    # Wait for a fuelgrid to finish generating
    fuelgrid.wait_until_finished(verbose=True)

    # Download the Fuelgrid zarr data
    fuelgrid.download_zarr(fgrid_path)

    # Load the immutable zarr store
    zarr_immutable = zarr.open(fgrid_path, mode='r')

    # Create a mutable zarr store
    zarr_mutable = zarr.open(zarr_path, mode='w')

    # Copy the data from the immutable zarr store to the mutable zarr store
    zarr.copy_all(zarr_immutable, zarr_mutable)

    ff.export_zarr_to_duet(zarr_immutable, duet_path, seed=47, wind_dir=180, wind_var=360, duration=5)
    ff.export_zarr_to_quicfire(zarr_immutable, qf_path)

if __name__=='__main__':
    OG_PATH = os.getcwd()
    fire_name = "Dixie"
    site_name = "Chips"
    plot_size = 500
    fgrid_mutable = site_name+"_fuelgrid_mutable.zarr"
    site_path = os.path.join(OG_PATH,"Sample_Sites", site_name)
    duet_path = os.path.join(site_path, "Duet")
    ff_path = os.path.join(site_path, "FastFuels")
    zarr_path = os.path.join(ff_path,fgrid_mutable)
    qf_path = os.path.join(site_path,"QF_Runs", str(plot_size)+"m")
    run_fastfuels(site_path,site_name,fire_name,ff_path,duet_path,qf_path)