#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Aug  3 11:16:05 2023

@author: ntutland
"""

import os
import pandas as pd
import geopandas as gpd
from shapely import Polygon

def create_burnplots(sites_path, fire_name, EPSG, plot_size, buffer):
    sites = os.path.join(sites_path,fire_name+"_Samples.shp")
    sample_sites = gpd.read_file(sites).set_crs(4326).to_crs(EPSG)
    full_size = plot_size + (2*buffer)
    site_list = list()
    for i in range(len(sample_sites.index)):
        pnt = sample_sites.loc[i]
        site_poly = make_bbox(pnt,full_size,EPSG)
        site_list.append(site_poly)

    site_bounds = gpd.GeoDataFrame(pd.concat(site_list)).reset_index()
    site_bounds = site_bounds.loc[:, site_bounds.columns != 'index']

    site_bounds.to_file(os.path.join(sites_path,fire_name+"_sites_"+str(plot_size)+"m.shp"))
    
    for site in site_bounds["site_name"]:
        samp = gpd.GeoDataFrame(site_bounds[site_bounds["site_name"]==site])
        os.makedirs(os.path.join(sites_path,site), exist_ok=True)
        samp.to_file(os.path.join(sites_path, site, site+"_bounds_"+str(plot_size)+"m.shp"))

def create_burnplot(fire_path, fire_name, site_name, EPSG, plot_size, buffer):
    sites = os.path.join(fire_path,"Sample_Sites",fire_name+"_Samples.csv")
    sample_sites = gpd.read_file(sites).set_crs(4326).to_crs(EPSG)
    full_size = plot_size + (2*buffer)
    site_list = list()
    for i in range(len(sample_sites.index)):
        pnt = sample_sites.loc[i]
        site_poly = make_bbox(pnt,full_size,EPSG)
        site_list.append(site_poly)

    site_bounds = gpd.GeoDataFrame(pd.concat(site_list)).reset_index()
    site_bounds = site_bounds.loc[:, site_bounds.columns != 'index']

    site_bounds.to_file(os.path.join(sites_path,fire_name+"_sites_"+str(plot_size)+"m.shp"))
    
    samp = gpd.GeoDataFrame(site_bounds[site_bounds["site_name"]==site_name])
    os.makedirs(os.path.join(fire_path,"Sample_Sites",site_name), exist_ok=True)
    samp.to_file(os.path.join(fire_path, "Sample_Sites", site_name, site_name+"_bounds_"+str(plot_size)+"m.shp"))
    

def make_bbox(pnt, dim, epsg):
    dim = dim/2
    x = pnt.geometry.x
    y = pnt.geometry.y
    nw, ne, se, sw = (x-dim,y+dim), (x+dim,y+dim), (x+dim,y-dim), (x-dim,y-dim)
    poly = Polygon((nw,ne,se,sw,nw))
    poly_gdf = gpd.GeoDataFrame({"site_name" : pnt["site_name"],
                                 "geometry" : poly},
                                index=[0],
                                crs=epsg)
    return poly_gdf

if __name__ == '__main__':
    OG_PATH = os.getcwd()
    sites_path = os.path.join(OG_PATH,"Sample_Sites")
    fire_name = "Dixie"
    EPSG = 5070
    plot_size = 500
    create_burnplots(sites_path, fire_name, EPSG, plot_size, buffer=50)