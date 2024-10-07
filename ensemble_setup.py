#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Oct 10 08:45:22 2023

@author: ntutland
"""
# Core imports
from os import environ
from pathlib import Path
import math
from shutil import copy
import subprocess
from os import chdir
from time import sleep
from sys import exit

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
from scipy.io import FortranFile

environ["FASTFUELS_API_KEY"] = "sxk-b78b909a-383c-4972-b480-749f9f926a4b"
import fastfuels_sdk as fastfuels
import quicfire_tools as qft

# import sys

# sys.path.insert(0, "/Users/ntutland/Documents/Projects/duet-tools/")
import duet_tools as duet


def main():
    HERE = Path("/Users/ntutland/Documents/Projects/Fire-Flood-Mud")
    sites_path = HERE / "Sample_Basins.csv"
    fire_df = pd.read_csv(sites_path)
    fire_gdf = gpd.GeoDataFrame(
        fire_df,
        geometry=gpd.points_from_xy(fire_df["X"], fire_df["Y"]),
        crs="EPSG:5070",
    )

    margins = [1.0, 0.05, 1.0]
    moisture = {
        "Caldor": [0.07, 0.18, 0.07],
        "Dixie": [0.07, 0.18, 0.07],
        "KNP": [0.07, 0.17, 0.07],
        "CedarCreek": [0.09, 0.18, 0.09],
        "CubCreek2": [0.08, 0.18, 0.08],
    }

    for i in range(len(fire_gdf.index)):
        if i >= 0:
            fire_name = fire_gdf.iloc[i]["Fire_Name"]
            site_name = fire_gdf.iloc[i]["Site_Name"]
            site_coords = fire_gdf.iloc[i]["geometry"]
            og_path = HERE

            print("\n", fire_name, "-", site_name, "\n")
            # prepare simulation
            qf_run = QuicfireRun(
                fire_name,
                site_name,
                site_coords,
                og_path,
                margins,
                moisture,
                fastfuels_done=True,
                duet_done=True,
                calibration_done=True,
                check_inputs=False,
                write_test_sim=False,
            )
            qf_run.run_fastfuels()
            qf_run.get_ignition_doubleline()
            qf_run.run_duet()
            qf_run.calibrate_duet_from_landfire()
            qf_run.calibrate_moisture()
            qf_run.modify_fuels()
            qf_run.quicfire_simulation()


class QuicfireRun:

    def __init__(
        self,
        fire_name,
        site_name,
        site_coords,
        og_path,
        margins,
        moisture,
        EPSG=5070,
        buffer=50,
        fastfuels_done=False,
        duet_done=False,
        severity_done=False,
        calibration_done=False,
        check_inputs=False,
        write_test_sim=False,
    ):
        OG_PATH = og_path
        self.OG_PATH: Path = OG_PATH
        self.fire_name: str = fire_name
        self.site_name: str = site_name
        self.site_coords: Polygon = site_coords
        self.EPSG: int = EPSG
        self.buffer: int = buffer
        self.margins: list = margins
        self.moisture: dict = moisture
        self.ignition_pace: int = 5
        self.ignition_length: int = 200
        self.wind_speed: int = 8
        # Paths
        self.fire_path = OG_PATH / fire_name
        qf_name = f"{site_name}"
        self.qf_path = OG_PATH / "QF_runs" / fire_name / qf_name
        self.site_path = OG_PATH / fire_name / "Sample_Basins" / site_name
        # Filenames
        self.shp_name = self.site_name + ".shp"
        self.fgrid_name = self.site_name + "_fuelgrid.zip"
        self.mutable_name = self.site_name + "_fastfuels.zarr"
        # Done
        self.fastfuels_done = fastfuels_done
        self.duet_done = duet_done
        self.severity_done = severity_done
        self.calibration_done = calibration_done
        # Calculated
        self.wind_dir = None
        self.ignition_coords = None
        self.fgrid_zarr = self._import_fgrid_zarr() if fastfuels_done else None
        self.nx = self.fgrid_zarr.attrs["nx"] if fastfuels_done else None
        self.ny = self.fgrid_zarr.attrs["ny"] if fastfuels_done else None
        self.nz = self.fgrid_zarr.attrs["nz"] if fastfuels_done else None
        self.nsp = self._find_nsp() if duet_done else None
        # Test
        self.check_inputs = check_inputs
        self.write_test_sim = write_test_sim
        # Make dirs
        paths = [self.fire_path, self.qf_path, self.site_path]
        for p in paths:
            p.parent.parent.mkdir(exist_ok=True)
            p.parent.mkdir(exist_ok=True)
            p.mkdir(exist_ok=True)

    def run_fastfuels(self):
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
                description=f"{self.fire_name} Fire",
                spatial_data=geojson,
            )

            # Create a treelist from a dataset
            treelist = dataset.create_treelist(
                name=self.site_name, description=f"{self.fire_name} Fire"
            )

            # Wait for a treelist to finish generating
            treelist.wait_until_finished(verbose=True)

            # Create a fuelgrid from a treelist
            fuelgrid = treelist.create_fuelgrid(
                name=self.site_name,
                description=f"{self.fire_name} Fire",
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

            fastfuels.export_zarr_to_quicfire(zroot, self.site_path)
            self.nx = zroot.attrs["nx"]
            self.ny = zroot.attrs["ny"]
            self.nz = zroot.attrs["nz"]
            self.fgrid_zarr = zarr_mutable

            fastfuels.export_zarr_to_duet(
                zroot,
                self.site_path,
                seed=47,
                wind_dir=self.wind_dir,
                wind_var=360,
                duration=5,
            )

            self.fastfuels_done = True

        else:
            print(
                "FastFuels has already been run. To rerun, set self.fastfuels_done to False"
            )

    def get_ignition_singleline(self):
        """
        Generate an ignition line based on wind direction that resides outside of
        the buffer zone and is perpendicular to the wind direction.
        """
        if not self.fastfuels_done:
            raise Exception(
                "get_ignition: FastFuels must be run before ignitions can be calculated"
            )
        # Get wind direction from the fastfuels topo file
        self._wdir_from_topo()
        # Get coordinates of buffer corner nearest to wind direction
        if self.wind_dir < 90:
            x1, y1 = (self.nx * 2 - self.buffer, self.ny * 2 - self.buffer)
        elif self.wind_dir < 180:
            x1, y1 = (self.nx * 2 - self.buffer, self.buffer)
        elif self.wind_dir < 270:
            x1, y1 = (self.buffer, self.buffer)
        else:
            x1, y1 = (self.buffer, self.ny * 2 - self.buffer)

        theta = (180 - self.wind_dir) % 180
        m = math.tan(math.radians(theta))

        L = m * (-x1) + y1
        T = (self.ny * 2 - y1) / m + x1 if m != 0 else float("inf")
        R = m * (self.nx * 2 - x1) + y1
        B = (-y1) / m + x1 if m != 0 else float("-inf")

        intersections = ((0, L), (T, self.ny * 2), (self.nx * 2, R), (B, 0))
        border_intersections = []
        for j, k in intersections:
            if 0 <= j <= self.nx * 2 and 0 <= k <= self.ny * 2:
                border_intersections.append((j, k))

        switched = [border_intersections[1], border_intersections[0]]
        start = _percent_along_line(*border_intersections, perc=0.1)
        end = _percent_along_line(*switched, perc=0.1)

        self.ignition_coords = [(start, end)]
        self._write_ignite()

    def get_ignition_doubleline(self):
        """
        Make two diverging ignition lines that start at the corner that corresponds
        to where the wind is coming from. Then, they will diverge along each side,
        with each ignition length corresponding to the percent that the angle is
        closer to. This way, ignition lines will always add up to 100m (or whatever
        I decide to set it to)
        """
        if not self.fastfuels_done:
            raise Exception(
                "get_ignition: FastFuels must be run before ignitions can be calculated"
            )
        # Get wind direction from the fastfuels topo file
        self._wdir_from_topo()
        # Get coordinates of buffer corner nearest to wind direction
        # subtract half the buffer. This will be the ignition starting point
        if self.wind_dir < 90:
            x1, y1 = (self.nx * 2 - self.buffer / 2, self.ny * 2 - self.buffer / 2)
            y_len = (25 + round(self.wind_dir / 90 * self.ignition_length)) * -1
            x_len = (25 + round((90 - self.wind_dir) / 90 * self.ignition_length)) * -1
        elif self.wind_dir < 180:
            x1, y1 = (self.nx * 2 - self.buffer / 2, self.buffer / 2)
            y_len = 25 + round((180 - self.wind_dir) / 90 * self.ignition_length)
            x_len = (25 + round((self.wind_dir - 90) / 90 * self.ignition_length)) * -1
        elif self.wind_dir < 270:
            x1, y1 = (self.buffer / 2, self.buffer / 2)
            y_len = 25 + round((self.wind_dir - 180) / 90 * self.ignition_length)
            x_len = 25 + round((270 - self.wind_dir) / 90 * self.ignition_length)
        else:
            x1, y1 = (self.buffer / 2, self.ny * 2 - self.buffer / 2)
            y_len = (25 + round((360 - self.wind_dir) / 90 * self.ignition_length)) * -1
            x_len = 25 + round((self.wind_dir - 270) / 90 * self.ignition_length)

        self.ignition_coords = [
            ((x1, y1), (x1, y1 + y_len)),
            ((x1, y1), (x1 + x_len, y1)),
        ]
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

        fig, ax = plt.subplots()
        for line in range(len(self.ignition_coords)):
            start_loc, end_loc = self.ignition_coords[line]
            plt.plot(
                [start_loc[0] / 2, end_loc[0] / 2],
                [start_loc[1] / 2, end_loc[1] / 2],
                "ro-",
            )
        plt.xlim(0, self.nx)
        plt.ylim(0, self.ny)
        ax.set_aspect("equal")
        rect = patches.Rectangle(
            (self.buffer / 2, self.buffer / 2),
            self.nx - (self.buffer),
            self.ny - (self.buffer),
            linewidth=1,
            edgecolor="black",
            facecolor="none",
        )
        ax.add_patch(rect)
        self._draw_arrow()
        plt.show()

    def run_duet(self):
        if self.duet_done == False:
            self._copy_duet()
            required = [
                "duet_v2.0.1a_FF.exe",
                "duet.in",
                "FIA_FastFuels_fin_fulllist_populated.txt",
            ]
            for file in required:
                test_path = self.site_path / file
                if not test_path.exists():
                    raise FileNotFoundError("run_duet: {} not found".format(file))
            # run DUET
            chdir(self.site_path)
            with subprocess.Popen(
                ["./duet_v2.0.1a_FF.exe"], stdout=subprocess.PIPE
            ) as process:

                def poll_and_read():
                    print(f"{process.stdout.read1().decode('utf-8')}")

                while process.poll() not in [0, -10, -11]:
                    poll_and_read()
                    print(f"{process.poll()}")
                    sleep(1)
                if process.poll() in [0, -10, -11]:
                    print(f"DUET run successfully (Exit poll: {process.poll()})")
                else:
                    print(f"Error: Poll = {process.poll()}")
                    exit(47)
            chdir(self.OG_PATH)
            # delete unneeded duet files
            to_delete = [
                "canopy.dat",
                "flattrees.dat",
                "surface_rhof.dat",
                "surface_ss_layered.dat",
            ]
            for file in to_delete:
                Path(self.site_path / file).unlink()
            # find nsp
            self.nsp = self._find_nsp()
        else:
            print("DUET has already been run. To rerun, set self.duet_done to False")

    def calibrate_duet_from_landfire(self):
        if self.calibration_done == False:
            xmin, xmax, ymin, ymax = (
                self.fgrid_zarr.attrs["xmin"],
                self.fgrid_zarr.attrs["xmax"],
                self.fgrid_zarr.attrs["ymin"],
                self.fgrid_zarr.attrs["ymax"],
            )
            pad = 30 / 2
            xmin, xmax, ymin, ymax = xmin - pad, xmax + pad, ymin - pad, ymax + pad
            bbox = Polygon(
                [(xmin, ymin), (xmax, ymin), (xmax, ymax), (xmin, ymax), (xmin, ymin)]
            )

            duet_run = duet.import_duet(self.site_path, self.nx, self.ny, self.nsp)
            query = duet.query_landfire(
                area_of_interest=bbox, directory=self.site_path, input_epsg=5070
            )
            try:
                grass_density = duet.assign_targets_from_sb40(query, "grass", "density")
                grass_height = duet.assign_targets_from_sb40(query, "grass", "height")
                grass_moisture = duet.assign_targets_from_sb40(
                    query, "grass", "moisture"
                )
                density_targets = duet.set_density(grass=grass_density)
                height_targets = duet.set_height(grass=grass_height)
                moisture_targets = duet.set_moisture(grass=grass_moisture)
                calibrated_grass = duet.calibrate(
                    duet_run, [density_targets, height_targets, moisture_targets]
                )
            except ValueError:
                print("No grass at sample site. Continuing with litter only.")
                calibrated_grass = duet_run
                pass
            try:
                litter_density = duet.assign_targets_from_sb40(
                    query, "litter", "density"
                )
                litter_height = duet.assign_targets_from_sb40(query, "litter", "height")
                litter_moisture = duet.assign_targets_from_sb40(
                    query, "litter", "moisture"
                )
                density_targets = duet.set_density(litter=litter_density)
                height_targets = duet.set_height(litter=litter_height)
                moisture_targets = duet.set_moisture(litter=litter_moisture)
                calibrated_litter = duet.calibrate(
                    calibrated_grass,
                    [density_targets, height_targets, moisture_targets],
                )
            except ValueError:
                print("No litter at sample site. Continuing with grass only.")
                calibrated_litter = calibrated_grass
                pass

            duet_density = calibrated_litter.to_numpy("integrated", "density")
            duet_height = calibrated_litter.to_numpy("integrated", "height")
            duet_moisture = calibrated_litter.to_numpy("integrated", "moisture")

            treesrhof = _read_dat_file(
                self.site_path, "treesrhof.dat", arr_dim=(self.nz, self.ny, self.nx)
            )
            treesfueldepth = _read_dat_file(
                self.site_path,
                "treesfueldepth.dat",
                arr_dim=(self.nz, self.ny, self.nx),
            )
            treesmoist = _read_dat_file(
                self.site_path, "treesmoist.dat", arr_dim=(self.nz, self.ny, self.nx)
            )
            # plot_array(treesrhof[0, :, :], "treesrhof fastfuels")
            # plot_array(treesmoist[0, :, :], "treesmoist fastfuels")
            # plot_array(treesfueldepth[0, :, :], "treesfueldepth fastfuels")
            # plot_array(duet_density, "calibrated density")
            # plot_array(duet_moisture, "calibrated moisture")
            # plot_array(duet_height, "calibrated height")
            treesrhof[0, :, :] = duet_density
            treesfueldepth[0, :, :] = duet_height
            treesmoist[0, :, :] = duet_moisture
            # plot_array(treesrhof[0, :, :], "treeshrhof calibrated")

            _write_array_to_dat(treesrhof, "treesrhof.dat", self.qf_path, reshape=False)
            _write_array_to_dat(
                treesfueldepth, "treesfueldepth.dat", self.qf_path, reshape=False
            )
            _write_array_to_dat(
                treesmoist, "treesmoist.dat", self.qf_path, reshape=False
            )
            self.duet_done = True
        else:
            print(
                "Calibration with Landfire has already been done."
                "To re-calibrate, set self.calibration_done to False"
            )

    def calibrate_moisture(self):
        moisture = self.moisture.get(self.fire_name)
        duet_run = duet.import_duet(self.site_path, self.nx, self.ny, self.nsp)
        grass_targets = duet.assign_targets(method="constant", value=moisture[0])
        litter_targets = duet.assign_targets(
            method="maxmin", max=moisture[1], min=moisture[2]
        )
        moisture_targets = duet.set_moisture(grass=grass_targets, litter=litter_targets)
        calibrated = duet.calibrate(duet_run, moisture_targets)
        calibrated_moisture = calibrated.to_numpy("integrated", "moisture")

        ff_moist = _read_dat_file(
            self.site_path, "treesmoist.dat", (self.nz, self.ny, self.nx)
        )
        ff_moist[0, :, :] = calibrated_moisture
        _write_array_to_dat(ff_moist, "treesmoist.dat", self.qf_path, reshape=False)

    def modify_fuels(self):
        margins = _get_margin_indices((self.ny, self.nx), 25)
        rhof = _read_dat_file(
            self.qf_path, "treesrhof.dat", (self.nz, self.ny, self.nx)
        )
        height = _read_dat_file(
            self.qf_path, "treesfueldepth.dat", (self.nz, self.ny, self.nx)
        )
        moist = _read_dat_file(
            self.qf_path, "treesmoist.dat", (self.nz, self.ny, self.nx)
        )

        # Modify fuels in margins
        margin_rhof = np.zeros((rhof.shape[1], rhof.shape[2]))
        margin_rhof[margins] = self.margins[0]
        rhof[0, :, :] = np.maximum(rhof[0, :, :], margin_rhof)
        margin_moist = np.ones((rhof.shape[1], rhof.shape[2]))
        margin_moist[margins] = self.margins[1]
        moist[0, :, :] = np.minimum(moist[0, :, :], margin_moist)
        margin_height = np.zeros((rhof.shape[1], rhof.shape[2]))
        margin_height[margins] = self.margins[2]
        height[0, :, :] = np.maximum(height[0, :, :], margin_height)

        for z in range(1, moist.shape[0]):
            moist[z, :, :][margins] = 0.1  # canopy fuels 10% moisture in margins
            # plot_array(moist[z, :, :], f"canopy moisture layer {z}")

        # Modify canopy rhof
        # rhof[1:10, :, :] = rhof[1:10, :, :] * 5
        # rhof[1:, :, :][rhof[1:, :, :] > 2.0] = 2.0

        # plot_array(rhof[0, :, :], f"modified rhof {self.margins}")
        # plot_array(moist[0, :, :], f"modified moisture {self.margins}")
        # plot_array(height[0, :, :], f"modified height {self.margins}")

        _write_array_to_dat(rhof, "treesrhof.dat", self.qf_path, reshape=False)
        _write_array_to_dat(moist, "treesmoist.dat", self.qf_path, reshape=False)
        _write_array_to_dat(height, "treesfueldepth.dat", self.qf_path, reshape=False)

    def quicfire_simulation(self):
        sim = qft.SimulationInputs.create_simulation(
            self.nx,
            self.ny,
            fire_nz=self.nz,
            wind_speed=self.wind_speed,
            wind_direction=self.wind_dir,
            simulation_time=10800,
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
        sim.qu_simparams.quic_domain_height = 1800

        # assemble ensemble
        self.qf_path.mkdir(exist_ok=True)
        sim.to_json(self.qf_path / f"{self.site_name}.json")

        # copy dat files and exe
        # dat files
        dat_files = [
            "ignite.dat",
            "topo.dat",
        ]
        for file in dat_files:
            src = self.site_path / file
            dst = self.qf_path / file
            copy(src, dst)

        if self.write_test_sim:
            self._json_to_test_qf_run()
            # exe
            exe_src = Path(
                "/Users/ntutland/Documents/Quicfire/QF_6.0.0/exe/quicfire_MACI.exe"
            )
            exe_dst = self.qf_path / "quicfire_MACI.exe"
            copy(exe_src, exe_dst)
            # drawfire
            drawfire_dir = Path(
                "/Users/ntutland/Documents/Quicfire/QF_6.0.0/scripts/postprocessing/python3/quicfire_vis"
            )
            drawfire = []
            for file in drawfire_dir.iterdir():
                drawfire.append(file.name)
            for file in drawfire:
                src = drawfire_dir / file
                dst = self.qf_path / file
                copy(src, dst)

        if self.check_inputs:
            self._check_inputs()

    def _wdir_from_topo(self):
        topo = _read_dat_file(
            self.site_path,
            "topo.dat",
            arr_dim=(1, self.ny, self.nx),
            order="C",
        )
        topo = topo[0, :, :]
        lowpoint = np.where(topo == np.min(topo))
        highpoint = np.where(topo == np.max(topo))
        low_coords = np.array([lowpoint[0][0], lowpoint[1][0]])
        high_coords = np.array([highpoint[0][0], highpoint[1][0]])
        center_coords = np.array([self.ny / 2, self.nx / 2])
        new_wdir = _calculate_angle(low_coords, high_coords, center_coords)
        self.wind_dir = int(round(new_wdir))
        # plot_array(topo, f"{self.site_name} topo", save=None)

    def _write_ignite(self):
        """
        Write an ignite.dat file to the QUIC-Fire run directory.
        Ignition location is based on the average wind direction on the day of the fire.

        Returns
        -------
        None. Writes ignite.dat

        """
        ignite_path = self.site_path / "ignite.dat"
        with open(ignite_path, "w") as file:
            file.write("igntype=5\n")
            file.write("&atvlist\n")
            file.write(f"natv={len(self.ignition_coords)}\n")
            file.write("targettemp=1000.0\n")
            file.write("flamedistance=4.00\n")
            file.write("/\n")
            for line in range(len(self.ignition_coords)):
                start_loc, end_loc = self.ignition_coords[line]
                duration = _ignition_duration(start_loc, end_loc, self.ignition_pace)
                file.write(
                    "{} {} {} {} {} {}\n".format(
                        start_loc[0], start_loc[1], end_loc[0], end_loc[1], 0, duration
                    )
                )
        print("ignite.dat written to {}".format(self.qf_path))

    def _copy_duet(self):
        files = ["duet_v2.0.1a_FF.exe", "FIA_FastFuels_fin_fulllist_populated.txt"]
        for file in files:
            src = self.OG_PATH / "Duet" / file
            dst = self.site_path / file
            if not src.exists():
                raise FileNotFoundError(
                    "run_duet: {} not found in Duet directory".format(file)
                )
            copy(src, dst)

    def _find_nsp(self) -> int:
        species = []
        with open(self.site_path / "surface_species.dat", "r") as file:
            for line in file:
                species.append(line.strip())
        return len(species) + 1

    def _import_fgrid_zarr(self):
        zarr_path = self.site_path / self.mutable_name
        zroot = zarr.open(zarr_path, mode="r")
        return zroot

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

    def _check_inputs(self):
        treesrhof = _read_dat_file(
            self.qf_path, "treesrhof.dat", (self.nz, self.ny, self.nx)
        )
        treesmoist = _read_dat_file(
            self.qf_path, "treesmoist.dat", (self.nz, self.ny, self.nx)
        )
        treesfueldepth = _read_dat_file(
            self.qf_path, "treesfueldepth.dat", (self.nz, self.ny, self.nx)
        )
        plot_array(treesrhof[0, :, :], "surface rhof")
        plot_array(treesmoist[0, :, :], "surface moist")
        plot_array(treesfueldepth[0, :, :], "surface fuel depth")

        canopy = np.sum(treesrhof[1:, :, :], axis=0)
        canopy[canopy == 0] = -25
        plot_array(canopy, "canopy rhof (no fuel= -25)")

        self.draw_ignition()
        # with open(self.qf_path / "ignite.dat") as file:
        #     for line in file:
        #         print(line)

        print(
            f"Dimensions from fastfuels: nx = {self.nx}, ny = {self.ny}, nz = {self.nz}"
        )

        # sim = qft.SimulationInputs.from_json(self.qf_path / f"{self.site_name}.json")
        # print(
        #     f"Dimensions in quicfire run: nx = {sim.qu_simparams.nx}, "
        #     f"ny = {sim.qu_simparams.ny}, nz = {sim.quic_fire.nz}"
        # )

    def _json_to_test_qf_run(self):
        sim = qft.SimulationInputs.from_json(self.qf_path / f"{self.site_name}.json")
        sim.set_output_files(fuel_dens=True)
        sim.write_inputs(self.qf_path)


def _read_dat_file(
    dire: Path, filename: str, arr_dim: tuple, order: str = "C"
) -> np.ndarray:
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


def _unit_vector(p1: np.ndarray, p2: np.ndarray) -> np.ndarray:
    # Calculate the direction vector
    v = p2 - p1

    # Normalize the vector (optional)
    magnitude = np.sqrt(np.sum(v**2))
    unit_vector = v / magnitude

    # Output the direction vector and the unit vector
    return unit_vector


def _calculate_angle(
    low_coords: np.ndarray, high_coords: np.ndarray, center_coords: np.ndarray
) -> float:
    v1 = _unit_vector(low_coords, center_coords)
    v2 = _unit_vector(center_coords, high_coords)
    # Calculate the average vector
    v_avg = np.add(v1, v2) / 2

    # Calculate the angle in radians using atan2
    angle_radians = math.atan2(v_avg[1], v_avg[0])

    # Convert to degrees
    angle_degrees = angle_radians * (180 / math.pi)
    angle = (angle_degrees + 180) % 360
    return angle


def _get_margin_indices(array_size, margin_width):

    # Generate row and column indices for the array
    rows, cols = np.indices(array_size)

    # Create margins directly using np.logical_or to identify margins
    margins = np.logical_or.reduce(
        (
            rows < margin_width,
            rows >= array_size[0] - margin_width,
            cols < margin_width,
            cols >= array_size[1] - margin_width,
        )
    )

    # Find the indices of the margins using np.where()
    result_margins = np.where(margins)

    return result_margins


def plot_array(x, title, save: Path = None):
    plt.figure(2)
    plt.set_cmap("viridis")
    plt.imshow(x, origin="lower")
    plt.colorbar()
    plt.title(title, fontsize=18)
    if save:
        plt.savefig(save)
    plt.show()


if __name__ == "__main__":
    main()
