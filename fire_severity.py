#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Oct 10 08:36:52 2023

@author: ntutland
"""

import os
import rasterio as rio
import rasterio.mask
from rasterio.enums import Resampling
import rasterio.windows as rwd
import fiona
from shapely import Polygon
import geopandas as gpd
import r_funcs as r


def main(severity_path, site_path, site_name, ff_zarr):
    crop_severity(severity_path,site_path,site_name,ff_zarr)
    # this is slightly mis-aligned, so the nodata at the edges needs to be removed
    # mask_nodata(site_path,site_name)
    # upsample to qf resolution
    upsample_raster(site_path, site_name)
    # crop to the study area
    crop_to_site(ff_zarr, site_path, site_name)

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

if __name__=='__main__':
    main(severity_path, site_path, site_name, ff_zarr)