#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Aug 10 11:30:58 2023

@author: ntutland
"""

import os
import sys
import numpy as np
import rasterio as rio
import rasterio.mask
from rasterio.enums import Resampling
import rasterio.windows as rwd
import fiona
import json
import zarr
import subprocess
import shutil
from time import sleep
from shapely import Polygon
import geopandas as gpd
os.environ["FASTFUELS_API_KEY"] = "sxk-b78b909a-383c-4972-b480-749f9f926a4b"
import fastfuels_sdk as ff
import r_funcs as r
from TTRS_QUICFire_Support import plot_array
sys.path.insert(0,"/Users/ntutland/Documents/Projects/fastfuels-sdk-python/fastfuels_sdk")
import exports as exp 
from scipy.io import FortranFile

def main(fire_name,severity_path,param_path,site_path,site_name,zarr_path,duet_path,qf_path):
    FF_DONE = True
    SEV_DONE = True      
    DUET_DONE = True
    # run fastfuels
    if FF_DONE == False:
        run_fastfuels(site_path,site_name,fire_name,qf_path,duet_path,pad=50)
        # get_spatial_data(site_path, site_name, fire_name, qf_path, duet_path, pad=50)
    
    # read in mutable fuelgrid zarr
    zarr_mutable = zarr.open(zarr_path, mode='r')
    print(zarr_mutable.attrs["nz"], zarr_mutable.attrs["nx"], zarr_mutable.attrs["ny"])
    # ff.export_zarr_to_duet(zarr_mutable, duet_path, seed=47, wind_dir=180, wind_var=360, duration=5)
     
    # crop to the severity grid
    if SEV_DONE == False:
        crop_severity(severity_path,site_path,site_name,zarr_mutable)
        # this is slightly mis-aligned, so the nodata at the edges needs to be removed
        # mask_nodata(site_path,site_name)
        # upsample to qf resolution
        upsample_raster(site_path, site_name)
        # crop to the study area
        crop_to_site(zarr_mutable, site_path, site_name)
    
    if DUET_DONE == False:
        run_duet(qf_path,duet_path)
        calibrate_duet(zarr_mutable,duet_path,qf_path,param_path)
    
    duet = _read_dat_file(duet_path, "surface_rhof.dat", arr_dim = (2, zarr_mutable.attrs["nx"], zarr_mutable.attrs["ny"]), order = "F")
    plot_array(duet[0,:,:],1, "duet","")

def crop_severity(severity_path,site_path,site_name,zmut):   
    xmin,xmax,ymin,ymax = zmut.attrs['xmin'],zmut.attrs['xmax'],zmut.attrs['ymin'],zmut.attrs['ymax']
    pad = 30/2
    xmin,xmax,ymin,ymax = xmin-pad, xmax+pad, ymin-pad, ymax+pad
    bbox = Polygon([(xmin,ymin),
                    (xmax,ymin),
                    (xmax,ymax),
                    (xmin,ymax),
                    (xmin,ymin)])
    epsg = 5070
    shp_path = os.path.join(site_path, site_name+"_dNBR.shp")
    out_path = os.path.join(site_path, site_name+"_dNBR.tif")
    gpd.GeoDataFrame(index=[0], geometry=[bbox], crs='epsg:{}'.format(epsg)).to_file(shp_path)
    r.terra_crop(severity_path, shp_path, out_path)

def crop_to_site(zmut, site_path, site_name):
    xmin,xmax,ymin,ymax = zmut.attrs['xmin'],zmut.attrs['xmax'],zmut.attrs['ymin'],zmut.attrs['ymax']
    xmin,xmax,ymin,ymax = xmin-1, xmax+1, ymin-1, ymax+1
    bbox = Polygon([(xmin,ymin),
                    (xmax,ymin),
                    (xmax,ymax),
                    (xmin,ymax),
                    (xmin,ymin)])
    epsg = 5070
    rst_path = os.path.join(site_path,site_name+"_dNBR_upsample.tif")
    shp_path = os.path.join(site_path, site_name+"_dNBR_crop.shp")
    out_path = os.path.join(site_path, site_name+"_dNBR_crop.tif")
    gpd.GeoDataFrame(index=[0], geometry=[bbox], crs='epsg:{}'.format(epsg)).to_file(shp_path)
    r.terra_crop(rst_path, shp_path, out_path)
    
def upsample_raster(site_path,site_name):
    with rio.open(os.path.join(site_path, site_name+"_dNBR.tif")) as sb: 
        upscale_factor = 15 #lf res / qf_res
        profile = sb.profile.copy()
        # resample data to target shape
        data = sb.read(
            out_shape=(
                sb.count,
                int(sb.height * upscale_factor),
                int(sb.width * upscale_factor)
            ),
            resampling=Resampling.nearest
        )
    
        # scale image transform
        transform = sb.transform * sb.transform.scale(
            (sb.width / data.shape[-1]),
            (sb.height / data.shape[-2])
        )
        profile.update({"height": data.shape[-2],
                    "width": data.shape[-1],
                   "transform": transform})
        with rasterio.open(os.path.join(site_path, site_name+"_dNBR_upsample.tif"), "w", **profile) as dataset:
            dataset.write(data)

def mask_nodata(site_path,site_name):
    with rio.open(os.path.join(site_path,site_name+"_dNBR.tif")) as sb:
        profile = sb.profile.copy()
        data_window = rwd.get_data_window(sb.read(masked=True))
        data_transform = rwd.transform(data_window, sb.transform)
        profile.update(
            transform=data_transform,
            height=data_window.height,
            width=data_window.width)

        data = sb.read(window=data_window)
    with rio.open(os.path.join(site_path, site_name+"_dNBR_mask.tif"), 'w', **profile) as dst:
        dst.write(data)


def fiona_from_poly(poly,epsg,site_path,site_name):
    # Create the fiona geometry object
    fiona_geometry = {
        'type': 'Polygon',
        'coordinates': [list(poly.exterior.coords)]
    }
    
    # Create a fiona geometry object with the specified EPSG number
    with fiona.Env():
        schema = {
            'geometry': 'Polygon',
            'properties': {},
        }
    
        with fiona.open(site_path+'/{}_dNBR.shp'.format(site_name), 
                        'w', 
                        driver='ESRI Shapefile', 
                        schema=schema, 
                        crs='EPSG:{}'.format(epsg)) as output:
            output.write({
                'geometry': fiona_geometry,
                'properties': {},
            })
    with fiona.open(site_path+'/{}_dNBR.shp'.format(site_name)) as shapefile:
        site = [feature["geometry"] for feature in shapefile]
    return site

def run_fastfuels(site_path,site_name,fire_name,qf_path,duet_path,pad):
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

# def get_spatial_data(site_path,site_name,fire_name,qf_path,duet_path,pad):
#     shp_name = site_name+"_bounds.shp"
#     shp_path = os.path.join(site_path,shp_name)

#     # Load a spatial data file
#     gdf = gpd.read_file(shp_path).to_crs(5070)
#     geojson = json.loads(gdf.to_json())

#     # Create a dataset
#     dataset = ff.create_dataset(name=site_name,
#                                 description=fire_name+" Fire",
#                                 spatial_data=geojson)
#     print(dataset.spatial_data)

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

def _read_dat_file(dire: str,
                   filename: str,
                   arr_dim: tuple,
                   order: str = "C") -> np.array:
    """
    Read in a .dat file as a numpy array.

    Dimensions of the array must be known, and in the order (z,y,x)
    """
    
    # Import and reshape .dat file
    with open(os.path.join(dire, filename), "rb") as fin:
            arr = FortranFile(fin).read_reals(dtype="float32").reshape(arr_dim, order=order)
    
    return arr

if __name__ == '__main__':
    OG_PATH = os.getcwd()
    fire_name = "Dixie"
    site_name = "Chips"
    plot_size = 500
    fgrid_mutable = site_name+"_fuelgrid_mutable.zarr"
    severity_path = os.path.join(OG_PATH,fire_name+"_dnbr.tif")
    param_path = os.path.join(OG_PATH)
    site_path = os.path.join(OG_PATH,"Sample_Sites", site_name)
    duet_path = os.path.join(site_path, "Duet")
    ff_path = os.path.join(site_path, "FastFuels")
    zarr_path = os.path.join(ff_path,fgrid_mutable)
    qf_path = os.path.join(site_path,"QF_Runs", str(plot_size)+"m")
    main(fire_name,severity_path,param_path,site_path,site_name,zarr_path,duet_path,qf_path)

    