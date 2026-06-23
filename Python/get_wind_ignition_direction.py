#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Oct  3 09:15:16 2023

@author: ntutland
"""

import os
from pathlib import Path
import numpy as np
# from scipy.io import FortranFile
# from TTRS_QUICFire_Support import plot_array
import math
from datetime import datetime
import matplotlib.pyplot as plt
from meteostat import Point, Daily
import geopandas as gpd
import pylab
import matplotlib.patches as patches


def main(site_path, date, side_length, ignite_path):
    wind_dir = meteostat_winddir(site_path, *date)
    border_intersections = ignition_from_wind(side_length, 50, wind_dir)
    ignitions = buffer_ignition(border_intersections)
    plot_ignition_line(side_length, 50, *ignitions, wind_dir)
    # write_ignite(*ignitions, 5, ignite_path)
    
def ignition_from_wind(side_length, buffer, wind_dir):
    """
    Generate an ignition line based on wind direction that resides outside of
    the buffer zone and is perpendicular to the wind direction.
    """
    # Get coordinates of buffer corner nearest to wind direction
    if wind_dir < 90:
        x1, y1 = (side_length - buffer, side_length - buffer)
    elif wind_dir < 180:
        x1, y1 = (side_length - buffer, buffer)
    elif wind_dir < 270:
        x1, y1 = (buffer, buffer)
    else:
        x1, y1 = (buffer, side_length - buffer)
    
    theta = (180-wind_dir) % 180
    m = math.tan(math.radians(theta))
    
    L = m*(-x1) + y1
    T = (side_length - y1)/m + x1 if m!=0 else float('inf')
    R = m*(side_length - x1) + y1
    B = (-y1)/m + x1 if m!=0 else float('-inf')
    
    intersections = ((0,L),(T,side_length),(side_length,R),(B,0))
    true_list = []
    for j,k in intersections:
        if 0 <= j <= side_length and 0 <= k <= side_length:
            true_list.append((j,k))
    
    return true_list

def percent_along_line(pt1, pt2, perc):
    x = pt1[0] + perc * (pt2[0] - pt1[0])
    y = pt1[1] + perc * (pt2[1] - pt1[1]) 
    return (x,y)

def buffer_ignition(border_intersections, buffer_perc = 0.1):
    switched = [border_intersections[1], border_intersections[0]]
    new_start = percent_along_line(*border_intersections, buffer_perc)
    new_end = percent_along_line(*switched, buffer_perc)
    return [new_start, new_end]

def ignition_duration(start, end, pace):
    dist = math.sqrt((start[0]-end[0])**2 + (start[1]-end[1])**2)
    time = dist/pace
    return time

def write_ignite(start_loc, end_loc, pace, ignite_path):
    duration = ignition_duration(start_loc, end_loc, pace)
    with open(os.path.join(ignite_path), "w") as file:
        file.write("igntype=5\n")
        file.write("&atvlist\n")
        file.write("natv=1\n")
        file.write("targettemp=1000.0\n")
        file.write("flamedistance=4.00\n")
        file.write("/\n")
        file.write("{} {} {} {} {} {}\n".format(start_loc[0], end_loc[0], start_loc[1], end_loc[1], 0, duration))

def meteostat_winddir(site_path, year, month, day):
    site = gpd.read_file(site_path)
    center = site.centroid
    center = center.to_crs(4236)

    plot = Point(center[0].y, center[0].x)

    # Set time period
    start = datetime(year, month, day)
    end = datetime(year, month, day)

    # Get daily data
    data = Daily(plot, start, end)
    data = data.fetch()
    
    return data.wdir[0]

def plot_ignition_line(side_length, buffer, start_loc, end_loc, wind_dir):
    fig, ax = plt.subplots()
    plt.plot([start_loc[0], end_loc[0]], [start_loc[1], end_loc[1]], 'ro-')
    plt.xlim(0,302)
    plt.ylim(0,302)
    ax.set_aspect('equal')
    rect = patches.Rectangle((buffer,buffer), 
                             side_length-(2*buffer), 
                             side_length-(2*buffer), 
                             linewidth=1, 
                             edgecolor='black', 
                             facecolor='none')
    ax.add_patch(rect)
    draw_arrow(side_length, wind_dir)
    
def draw_arrow(side_length, wind_dir, arrow_length=25):
    
    start_x = side_length/2
    width = arrow_length / 4
    start_y = side_length/2
    
    #Draw arrow    
    end_x, end_y = pol2cart(arrow_length, wind_dir)
    pylab.arrow(start_x, start_y, end_x, end_y, fc="white", ec="black", 
                shape='full', width=width, head_width=width*3, 
                head_length=width*3, length_includes_head=True)

def pol2cart(rho, phi):
    """
    ZC. This functions converts polar cordinates to cartisian coordinates
    
    Inputs:
        rho: magnitude (float/int) 
        phi: direction (float/int)

    Returns
        x and y index
    """
    x = -rho * np.sin(np.radians(phi))
    y = -rho * np.cos(np.radians(phi))
    return(x, y)

if __name__=='__main__':
    OG_PATH = Path(__file__).parent.parent
    fire_name = "Dixie"
    site_name = "Chips"
    plot_size = 500
    site_path = os.path.join(OG_PATH, "Fire_Data", fire_name, "Sample_Sites", site_name, site_name+"_bounds_"+str(plot_size)+"m.shp")
    ignite_path = os.path.join(OG_PATH, "Fire_Data", fire_name, "Sample_Sites", site_name, "QF_runs", str(plot_size)+"m", "ignite.dat")
    date = (2021, 7, 19)
    side_length = 302
    main(site_path, date, side_length, ignite_path)