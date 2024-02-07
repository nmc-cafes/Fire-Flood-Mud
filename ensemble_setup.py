#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Oct 10 08:45:22 2023

@author: ntutland
"""
# Core imports
from os import environ, chdir
from pathlib import Path
import math
from shutil import copy
from time import sleep
from datetime import datetime

# External imports
import numpy as np
import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
import matplotlib.patches as patches
import pylab
import zarr
import json
from shapely import Polygon
from meteostat import Point, Daily
from scipy.io import FortranFile

environ["FASTFUELS_API_KEY"] = "sxk-b78b909a-383c-4972-b480-749f9f926a4b"
import fastfuels_sdk as fastfuels
import quicfire_tools as qft


def main():
    HERE = Path("/Users/ntutland/Documents/Projects/Fire-Flood-Mud")
    sites_path = HERE / "Sample_sites_NJT.csv"
    fire_df = pd.read_csv(sites_path)
    fire_gdf = gpd.GeoDataFrame(
        fire_df,
        geometry=gpd.points_from_xy(fire_df["X"], fire_df["Y"]),
        crs="EPSG:5070",
    )
    for i in range(len(fire_gdf.index)):
        if i > 23:  # Caldor is already done
            fire_name = fire_gdf.iloc[i]["Fire_Name"]
            site_name = fire_gdf.iloc[i]["Site_Name"]
            fire_date = fire_gdf.iloc[i]["Fire_Date"]
            site_coords = fire_gdf.iloc[i]["geometry"]
            domain_size = 500
            og_path = HERE

            print("\n", fire_name, "-", site_name, "\n")

            # prepare simulation
            qf_run = QuicfireRun(
                fire_name, site_name, fire_date, site_coords, domain_size, og_path
            )

            qf_run.create_burnplot()
            qf_run.run_fastfuels()
            qf_run.correct_fuelheight()
            qf_run.get_ignition()
            # qf_run.draw_ignition()
            qf_run.quicfire_simulation()


class QuicfireRun:
    def __init__(
        self,
        fire_name,
        site_name,
        fire_date,
        site_coords,
        domain_size,
        og_path,
        EPSG=5070,
        buffer=50,
        burnplot_done=False,
        fastfuels_done=False,
        duet_done=False,
        severity_done=False,
    ):
        OG_PATH = og_path
        self.OG_PATH = OG_PATH
        self.fire_name = fire_name
        self.site_name = site_name
        self.fire_date = fire_date
        self.site_coords = site_coords
        self.domain_size = domain_size
        self.EPSG = EPSG
        self.buffer = buffer
        self.ignition_pace = 5
        # Paths
        self.fire_path = OG_PATH / fire_name
        qf_name = "_".join([fire_name, site_name, str(domain_size) + "m"])
        self.qf_path = OG_PATH / "QF_runs" / fire_name / qf_name
        self.site_path = (
            OG_PATH / fire_name / "Sample_Sites" / site_name / (str(domain_size) + "m")
        )
        # Filenames
        self.shp_name = self.site_name + "_bounds_" + str(self.domain_size) + "m.shp"
        self.fgrid_name = self.site_name + "_fuelgrid.zip"
        self.mutable_name = self.site_name + "_fastfuels.zarr"
        self.dnbr_name = self.fire_name + "_dNBR.tif"
        # Done
        self.burnplot_done = burnplot_done
        self.fastfuels_done = fastfuels_done
        self.duet_done = duet_done
        self.severity_done = severity_done
        # Calculated
        self.wind_dir, self.wind_speed = self._meteostat()
        self.ignition_coords = None
        self.fgrid_zarr = self._import_fgrid_zarr() if fastfuels_done else None
        self.nx = self.fgrid_zarr.attrs["nx"] if fastfuels_done else None
        self.ny = self.fgrid_zarr.attrs["ny"] if fastfuels_done else None
        self.nz = self.fgrid_zarr.attrs["nz"] if fastfuels_done else None
        # Make dirs
        paths = [self.fire_path, self.qf_path, self.site_path]
        for p in paths:
            p.parent.parent.mkdir(exist_ok=True)
            p.parent.mkdir(exist_ok=True)
            p.mkdir(exist_ok=True)

    def create_burnplot(self):
        if self.burnplot_done == False:
            full_size = self.domain_size + (2 * self.buffer)
            site_poly = self._make_bbox(full_size)

            site_bounds = gpd.GeoDataFrame(site_poly)
            shp_path = self.site_path / self.shp_name
            site_bounds.to_file(shp_path)

            self.burnplot_done = True
        else:
            print(
                "Burn plot already created. To rerun, set self.burnplot_done to False"
            )

    def run_fastfuels(self):
        if not self.burnplot_done:
            raise Exception(
                "run_fastfuels: Burn plot must be created before running fastfuels"
            )
        if self.fastfuels_done == False:
            shp_path = self.site_path / self.shp_name
            fgrid_path = self.site_path / self.fgrid_name
            mutable_path = self.site_path / self.mutable_name

            # Load a spatial data file
            gdf = gpd.read_file(shp_path).to_crs(self.EPSG)
            geojson = json.loads(gdf.to_json())

            # Create a dataset
            dataset = fastfuels.create_dataset(
                name=self.site_name,
                description=self.fire_name + " Fire",
                spatial_data=geojson,
            )

            # Create a treelist from a dataset
            treelist = dataset.create_treelist(
                name=self.site_name, description=self.fire_name + " Fire"
            )

            # Wait for a treelist to finish generating
            treelist.wait_until_finished(verbose=True)

            # Create a fuelgrid from a treelist
            fuelgrid = treelist.create_fuelgrid(
                name=self.site_name,
                description=self.fire_name + " Fire",
                distribution_method="realistic",
                horizontal_resolution=2,
                vertical_resolution=1,
                border_pad=0,
            )

            # Wait for a fuelgrid to finish generating
            fuelgrid.wait_until_finished(verbose=True)

            # Download the Fuelgrid zarr data
            fuelgrid.download_zarr(fgrid_path)

            # Load the immutable zarr store
            zroot = zarr.open(fgrid_path, mode="r")

            # Create a mutable zarr store
            zarr_mutable = zarr.open(mutable_path, mode="w")

            # Copy the data from the immutable zarr store to the mutable zarr store
            zarr.copy_all(zroot, zarr_mutable)

            fastfuels.export_zarr_to_duet(
                zroot,
                self.site_path,
                seed=47,
                wind_dir=self.wind_dir,
                wind_var=360,
                duration=5,
            )
            fastfuels.export_zarr_to_quicfire(zroot, self.site_path)

            self.fastfuels_done = True
            self.nx = zroot.attrs["nx"]
            self.ny = zroot.attrs["ny"]
            self.nz = zroot.attrs["nz"]
            self.fgrid_zarr = zarr_mutable
        else:
            print(
                "FastFuels has already been run. To rerun, set self.fastfuels_done to False"
            )

    def correct_fuelheight(self):
        """Corrects the treesfueldepth.dat file by filling all non-surface layers with 1's"""

        height = _read_dat_file(
            self.site_path, "treesfueldepth.dat", (self.nz, self.ny, self.nx)
        )
        height[1:, :, :] = 1.0
        _write_array_to_dat(height, "treesfueldepth.dat", self.site_path, reshape=False)

    def get_ignition(self):
        """
        Generate an ignition line based on wind direction that resides outside of
        the buffer zone and is perpendicular to the wind direction.
        """
        if not self.fastfuels_done:
            raise Exception(
                "get_ignition: FastFuels must be run before ignitions can be calculated"
            )
        # Get coordinates of buffer corner nearest to wind direction
        if self.wind_dir < 90:
            x1, y1 = (self.nx - self.buffer, self.ny - self.buffer)
        elif self.wind_dir < 180:
            x1, y1 = (self.nx - self.buffer, self.buffer)
        elif self.wind_dir < 270:
            x1, y1 = (self.buffer, self.buffer)
        else:
            x1, y1 = (self.buffer, self.ny - self.buffer)

        theta = (180 - self.wind_dir) % 180
        m = math.tan(math.radians(theta))

        L = m * (-x1) + y1
        T = (self.ny - y1) / m + x1 if m != 0 else float("inf")
        R = m * (self.nx - x1) + y1
        B = (-y1) / m + x1 if m != 0 else float("-inf")

        intersections = ((0, L), (T, self.ny), (self.nx, R), (B, 0))
        border_intersections = []
        for j, k in intersections:
            if 0 <= j <= self.nx and 0 <= k <= self.nx:
                border_intersections.append((j, k))

        switched = [border_intersections[1], border_intersections[0]]
        start = _percent_along_line(*border_intersections, perc=0.1)
        end = _percent_along_line(*switched, perc=0.1)

        self.ignition_coords = [start, end]
        self._write_ignite()

    def draw_ignition(self):
        """
        Plot the ignition line within the burn domain, along with the analysis area and wind direction.

        Returns
        -------
        None. Shows a plot of the ignition location

        """
        if not self.ignition_coords:
            raise Exception(
                "draw_ignition: Ignitions coordinates must be calculated before they can be drawn"
            )
        start_loc, end_loc = self.ignition_coords
        fig, ax = plt.subplots()
        plt.plot([start_loc[0], end_loc[0]], [start_loc[1], end_loc[1]], "ro-")
        plt.xlim(0, self.nx)
        plt.ylim(0, self.ny)
        ax.set_aspect("equal")
        rect = patches.Rectangle(
            (self.buffer, self.buffer),
            self.nx - (2 * self.buffer),
            self.ny - (2 * self.buffer),
            linewidth=1,
            edgecolor="black",
            facecolor="none",
        )
        ax.add_patch(rect)
        self._draw_arrow()
        plt.show()

    def quicfire_simulation(self):
        sim = qft.SimulationInputs.create_simulation(
            self.nx,
            self.ny,
            fire_nz=self.nz,
            wind_speed=self.wind_speed,
            wind_direction=self.wind_dir,
            simulation_time=3600,
        )
        sim.set_custom_simulation()
        sim.set_output_files(
            react_rate=True,
            fuel_dens=True,
            fuel_moist=True,
            mass_burnt=True,
            radiation=True,
            surf_eng=True,
        )
        sim.quic_fire.fuel_density_flag = 4
        sim.quic_fire.fuel_moisture_flag = 4
        sim.quic_fire.ignitions_per_cell = 5
        sim.quic_fire.auto_kill = 1
        sim.qu_simparams.quic_domain_height = 1000

        # assemble ensemble
        self.qf_path.mkdir(exist_ok=True)
        sim.write_inputs(self.qf_path)

        # copy dat files and exe
        # dat files
        dat_files = [
            "ignite.dat",
            "treesrhof.dat",
            "treesmoist.dat",
            "treesfueldepth.dat",
            "topo.dat",
        ]
        for file in dat_files:
            src = self.site_path / file
            dst = self.qf_path / file
            copy(src, dst)
        # exe
        exe_src = Path(
            "/Users/ntutland/Documents/Quicfire/QF_6.0.0/exe/quicfire_MACI.exe"
        )
        exe_dst = self.qf_path / "quicfire_MACI.exe"
        copy(exe_src, exe_dst)
        # drawfire
        drawfire_dir = Path(
            "/Users/ntutland/Documents/Quicfire/QF_6.0.0/scripts/postprocessing/python3"
        )
        drawfire = []
        for file in drawfire_dir.iterdir():
            drawfire.append(file.name)
        for file in drawfire:
            src = drawfire_dir / file
            dst = self.qf_path / file
            copy(src, dst)

    def _write_ignite(self):
        """
        Write an ignite.dat file to the QUIC-Fire run directory.
        Ignition location is based on the average wind direction on the day of the fire.

        Returns
        -------
        None. Writes ignite.dat

        """
        start_loc, end_loc = self.ignition_coords
        duration = _ignition_duration(start_loc, end_loc, self.ignition_pace)
        ignite_path = self.site_path / "ignite.dat"
        with open(ignite_path, "w") as file:
            file.write("igntype=5\n")
            file.write("&atvlist\n")
            file.write("natv=1\n")
            file.write("targettemp=1000.0\n")
            file.write("flamedistance=4.00\n")
            file.write("/\n")
            file.write(
                "{} {} {} {} {} {}\n".format(
                    start_loc[0], end_loc[0], start_loc[1], end_loc[1], 0, duration
                )
            )
        print("ignite.dat written to {}".format(self.qf_path))

    def _copy_duet(self):
        files = ["duet", "FIA_FastFuels_fin_fulllist_populated.txt"]
        for file in files:
            src = self.OG_PATH / "Duet" / file
            dst = self.site_path / file
            if not src.exists():
                raise FileNotFoundError(
                    "run_duet: {} not found in Duet directory".format(file)
                )
            copy(src, dst)

    def _import_fgrid_zarr(self):
        zarr_path = self.qf_path / self.mutable_name
        zroot = zarr.open(zarr_path, mode="r")
        return zroot

    def _meteostat(self):
        sites = self.fire_path / (self.fire_name + "_sample_sites_NJT.shp")
        sample_sites = gpd.read_file(sites).to_crs(4326)
        center = sample_sites[
            sample_sites["Site_Name"] == self.site_name
        ].centroid.to_crs(4326)
        id = sample_sites.index[sample_sites["Site_Name"] == self.site_name].tolist()[0]
        plot = Point(center[id].y, center[id].x)
        plot.radius = 60000

        # from meteostat import Stations
        # stations = Stations()
        # stations = stations.nearby(center[id].y, center[id].x)
        # station = stations.fetch(1)
        # print(station)

        # Set time period
        start = datetime.strptime(self.fire_date, "%Y-%m-%d")
        end = datetime.strptime(self.fire_date, "%Y-%m-%d")

        # Get daily data
        data = Daily(plot, start, end)
        data = data.fetch()

        return (data.wdir[0], data.wspd[0])

    def _make_bbox(self, dim):
        dim = dim / 2
        x = self.site_coords.x
        y = self.site_coords.y
        nw, ne, se, sw = (
            (x - dim, y + dim),
            (x + dim, y + dim),
            (x + dim, y - dim),
            (x - dim, y - dim),
        )
        poly = Polygon((nw, ne, se, sw, nw))
        poly_gdf = gpd.GeoDataFrame(
            {"site_name": self.site_name, "geometry": poly}, index=[0], crs=self.EPSG
        )
        return poly_gdf

    def _draw_arrow(self, arrow_length=25):
        start_x = self.nx / 2
        width = arrow_length / 4
        start_y = self.ny / 2

        # Draw arrow
        end_x, end_y = _pol2cart(arrow_length, self.wind_dir)
        pylab.arrow(
            start_x,
            start_y,
            end_x,
            end_y,
            fc="white",
            ec="black",
            shape="full",
            width=width,
            head_width=width * 3,
            head_length=width * 3,
            length_includes_head=True,
        )


def _read_dat_file(
    dire: Path, filename: str, arr_dim: tuple, order: str = "C"
) -> np.array:
    """
    Read in a .dat file as a numpy array.

    Dimensions of the array must be known, and in the order (z,y,x)
    """

    # Import and reshape .dat file
    path = dire / filename
    with open(path, "rb") as fin:
        arr = FortranFile(fin).read_reals(dtype="float32").reshape(arr_dim, order=order)

    return arr


def _write_array_to_dat(
    array: np.ndarray,
    dat_name: str,
    output_dir: Path,
    dtype: type = np.float32,
    reshape: bool = True,
) -> None:
    """
    Write a numpy array to a fortran binary file. Array must be cast to the
    appropriate data type before calling this function. If the array is 3D,
    the array will be reshaped from (y, x, z) to (z, y, x) for fortran.
    """
    # Reshape array from (y, x, z) to (z, y, x) (also for fortran)
    if reshape:
        if len(array.shape) == 3:
            array = np.moveaxis(array, 2, 0).astype(dtype)
        else:
            array = array.astype(dtype)
    else:
        array = array.astype(dtype)

    # Write the zarr array to a dat file with scipy FortranFile package
    with FortranFile(Path(output_dir, dat_name), "w") as f:
        f.write_record(array)


def _ignition_duration(start, end, pace):
    dist = math.sqrt((start[0] - end[0]) ** 2 + (start[1] - end[1]) ** 2)
    time = dist / pace
    return time


def _percent_along_line(pt1, pt2, perc):
    x = pt1[0] + perc * (pt2[0] - pt1[0])
    y = pt1[1] + perc * (pt2[1] - pt1[1])
    return (x, y)


def _pol2cart(rho, phi):
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
    return (x, y)


if __name__ == "__main__":
    main()
