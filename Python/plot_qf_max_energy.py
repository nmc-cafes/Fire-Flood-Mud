#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Sep 29 13:25:33 2023

@author: ntutland
"""

import numpy as np
# import zarr
# import dask.array as da
import matplotlib.pyplot as plt
from pathlib import Path
from scipy.io import FortranFile
SIMULATIONS_PATH = Path("/Users/ntutland/Documents/Projects/Fire-Flood-Mud/Dixie/Runs")
def compute_max_surf_energy(output_path):
    # Get a list of all the surfEnergy files
    surf_energy_files = list(output_path.glob("surfEnergy*"))
    # Create an empty array to store the maximum surface energy
    surf_energy_max = np.zeros((302, 302)) #nx, ny
    for surfEnergy_file in surf_energy_files:
        surf_energy_data = read_surf_energy_file(surfEnergy_file)
        surf_energy_max = np.maximum(surf_energy_max, surf_energy_data)
    return surf_energy_max
def read_surf_energy_file(file):
    with FortranFile(file, "r") as f:
        surf_energy_data = f.read_reals(dtype=np.float32).reshape((302, 302))
    return surf_energy_data
def process_outputs():
    sim_path = (
        SIMULATIONS_PATH
        / "Chips_5.3.1"
    )
    output_path = sim_path / "Output"
    # Get the maximum surface energy for each case, season, and distribution technique
    max_surf_energy_dict = {}
    surf_energy = compute_max_surf_energy(output_path)
    max_surf_energy_dict["Chips"] = surf_energy
    # Create a plot of the maximum surface energy for each case and season
    fig, ax = plt.subplots(1, 1, figsize=(10, 10))

    surf_energy = max_surf_energy_dict["Chips"]
    im = ax.imshow(
        surf_energy, origin="lower", cmap="jet"
    )
    ax.set_xlabel("x (m)")
    ax.set_ylabel("y (m)")
    ax.set_title("Maximum Surface Energy", fontsize=32)
    cbar = fig.colorbar(im, ax=ax, extend="both")
    cbar.ax.set_ylabel("Surface Energy (J/m$^2$)", fontsize=16)
    plt.savefig(output_path / "plots/surf_energy_max.png")
    plt.show()
    # Create a histogram showing the distribution of maximum surface energy values
    vmin_global = float("inf")
    vmax_global = float("-inf")
    surf_energy = max_surf_energy_dict["Chips"]
    vmin_global = min(vmin_global, surf_energy.min())
    vmax_global = max(vmax_global, surf_energy.max())
    fig, ax = plt.subplots(1, 1, figsize=(5, 5))
    bin_edges = np.linspace(vmin_global, vmax_global, num=50)
    data = max_surf_energy_dict["Chips"].flatten()
    # Remove the values falling into the first bin.
    bin_start = bin_edges[1]  # The upper edge of the first bin
    data = data[data > bin_start]
    ax.hist(
        data,
        bins=bin_edges[1:],
        alpha=0.5,
        label="Growing Season",
        color="g",
    )

    ax.set_ylabel("Frequency")
    ax.set_xlabel("Intensity Values (J/m$^2$)")
    fig.suptitle(
        "Distribution of Non-zero Intensity Values",
        fontsize=16,
    )
    plt.tight_layout()
    plt.savefig(output_path / "plots/intensity_histogram.png")
    plt.show()
if __name__ == "__main__":
    process_outputs()









