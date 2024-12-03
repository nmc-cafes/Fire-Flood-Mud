import numpy as np
from quicfire_tools import SimulationOutputs, SimulationInputs
import matplotlib.pyplot as plt
from pathlib import Path
from scipy.io import FortranFile
import pandas as pd
import xarray as xr
import zarr


def plot_array(x, title):
    plt.figure(2)
    plt.set_cmap("viridis")
    plt.imshow(x, origin="lower")
    plt.colorbar()
    plt.title(title, fontsize=18)
    plt.show()


runs_dir = Path(
    "/Users/ntutland/Documents/Projects/Fire-Flood-Mud/QF_runs/Severe_Steep"
)


def get_mass_burnt(sim: SimulationOutputs, arrpath: Path, plot: bool = True):
    mburnt = sim.get_output("mburnt_integ")
    mburnt_arr = mburnt.to_numpy(timestep=len(mburnt.times) - 1)
    mburnt_total = mburnt_arr[0, 0, :, :]
    if plot:
        plot_array(mburnt_total, "percent mass burnt")
    np.savetxt(arrpath / "mass_burnt_pct.txt", mburnt_total)
    return mburnt_total


def get_surface_moisture(sim: SimulationOutputs, arrpath: Path, plot: bool = True):
    moist = sim.get_output("fuels-moist")
    moist_init = moist.to_numpy(timestep=0)
    moist_final = moist.to_numpy(timestep=len(moist.times) - 1)
    moist_init = moist_init[0, 0, :, :]
    moist_final = moist_final[0, 0, :, :]
    if plot:
        plot_array(moist_init, "initial surface fuel moisture")
        plot_array(moist_final, "final surface fuel moisture")


def get_surface_consumption(sim: SimulationOutputs, arrpath: Path, plot: bool = True):
    dens = sim.get_output("fuels-dens")
    dens_init = dens.to_numpy(timestep=0)
    dens_final = dens.to_numpy(len(dens.times) - 1)
    fuel_present = np.where(dens_init[0, 0, :, :] > 0)
    surface_consumption_pct = np.zeros((np.shape(dens_init[0, 0, :, :])))
    surface_consumption_pct[fuel_present] = (
        dens_init[0, 0, :, :][fuel_present] - dens_final[0, 0, :, :][fuel_present]
    ) / dens_init[0, 0, :, :][fuel_present]
    surface_consumption = np.zeros((np.shape(dens_init[0, 0, :, :])))
    surface_consumption[fuel_present] = (
        dens_init[0, 0, :, :][fuel_present] - dens_final[0, 0, :, :][fuel_present]
    )
    surface_remaining = np.zeros((np.shape(dens_init[0, 0, :, :])))
    surface_remaining[fuel_present] = 1 - (
        (dens_init[0, 0, :, :][fuel_present] - dens_final[0, 0, :, :][fuel_present])
        / dens_init[0, 0, :, :][fuel_present]
    )
    if plot:
        plot_array(surface_consumption_pct, "surface fuel consumption percent")
        plot_array(surface_remaining, "surface fuel percent remaining")
        plot_array(surface_consumption, "surface fuel consumption total")
    np.savetxt(arrpath / "surface_consumption_pct.txt", surface_consumption_pct)
    np.savetxt(arrpath / "surface_remaining_pct.txt", surface_remaining)
    np.savetxt(arrpath / "surface_consumption_tot.txt", surface_consumption)
    return surface_consumption_pct


def get_canopy_consumption(sim: SimulationOutputs, arrpath: Path, plot: bool = True):
    dens = sim.get_output("fuels-dens")
    dens_init = dens.to_numpy(timestep=0)
    dens_final = dens.to_numpy(len(dens.times) - 1)
    dens_init = dens_init[0, 1:, :, :]
    dens_final = dens_final[0, 1:, :, :]
    canopy_init = np.sum(dens_init, axis=0)
    canopy_final = np.sum(dens_final, axis=0)
    fuel_present = np.where(canopy_init > 0)
    canopy_consumption_pct = np.zeros(np.shape(canopy_init))
    fuels_present = np.zeros(np.shape(canopy_init))
    fuels_present[fuel_present] = 1
    canopy_consumption_pct[fuel_present] = (
        canopy_init[fuel_present] - canopy_final[fuel_present]
    ) / canopy_init[fuel_present]
    canopy_remaining_pct = np.zeros(np.shape(canopy_init))
    canopy_remaining_pct[fuel_present] = 1 - (
        (canopy_init[fuel_present] - canopy_final[fuel_present])
        / canopy_init[fuel_present]
    )
    canopy_consumption = np.zeros((np.shape(canopy_init)))
    canopy_consumption[fuel_present] = (
        canopy_init[fuel_present] - canopy_final[fuel_present]
    )
    if plot:
        # print(np.sum(canopy_init))
        # print(np.sum(canopy_init) - np.sum(canopy_final))
        # print((np.sum(canopy_init) - np.sum(canopy_final)) / np.sum(canopy_init))
        plot_array(canopy_consumption_pct, "percent canopy fuel consumption")
        plot_array(canopy_remaining_pct, "total canopy remaining")
        plot_array(canopy_consumption, "total canopy fuel consumption")
        # for z in range(43):
        #     plot_array(dens_init[z, :, :], f"initial fuel density layer {z+1}")
        #     canopy_remaining_z = np.zeros(np.shape(canopy_init))
        #     fuel_present = np.where(dens_init[z, :, :] > 0)
        #     canopy_consumption = np.zeros(np.shape(canopy_init))
        #     canopy_consumption_z = np.zeros(np.shape(canopy_init))
        #     canopy_consumption_z[fuel_present] = (
        #         dens_init[z, :, :][fuel_present] - dens_final[z, :, :][fuel_present]
        #     ) / dens_init[z, :, :][fuel_present]
        #     canopy_remaining_z[fuel_present] = 1 - (
        #         (dens_init[z, :, :][fuel_present] - dens_final[z, :, :][fuel_present])
        #         / dens_init[z, :, :][fuel_present]
        #     )
        #     canopy_z = np.zeros(np.shape(canopy_init))
        #     canopy_z[canopy_consumption_z > 0] = 2
        #     canopy_z[canopy_remaining_z == 1] = 1
        #     plot_array(canopy_z, f"Layer {z+1}, consumption=2, no consumption=1")
    np.savetxt(arrpath / "canopy_consumption_pct.txt", canopy_consumption_pct)
    np.savetxt(arrpath / "canopy_remaining_pct.txt", canopy_remaining_pct)
    np.savetxt(arrpath / "canopy_consumption_tot.txt", canopy_consumption)
    return canopy_consumption_pct


def get_power(sim: SimulationOutputs, arrpath: Path, plot: bool = True):
    energy = sim.get_output("surfEnergy")
    arr = energy.to_numpy()
    arr3d = arr[:, 0, :, :]
    print("\t\t - max power")
    max_power = np.max(arr3d, axis=0)  # Max power
    print("\t\t - total power")
    tot_power = np.sum(arr3d, axis=0)  # Total power
    # Residence time
    print("\t\t - surface residence time")
    mean = np.mean(max_power[max_power > 0])
    threshold = 0.05 * mean
    initial = energy.to_numpy(timestep=0)
    prev = initial[0, 0, :, :]
    for t in range(1, len(energy.times)):
        temp = energy.to_numpy(timestep=t)
        temp = temp[0, 0, :, :]
        temp[np.where(temp <= threshold)] = 0
        temp[np.where(temp > threshold)] = 1
        prev = np.add(temp, prev)
    residence_time = prev.copy()
    # Energy FLUX (don't know if that's the right term)
    print("\t\t - energy flux")
    flux = np.divide(tot_power, residence_time, where=residence_time > 0)
    if plot:
        plot_array(max_power, "max power")
        plot_array(tot_power, "total power")
        plot_array(residence_time, "surface residence time")
        plot_array(flux, "energy flux")
    np.savetxt(arrpath / "max_power.txt", max_power)
    np.savetxt(arrpath / "total_power.txt", tot_power)
    np.savetxt(arrpath / "surface_residence_time.txt", residence_time)
    np.savetxt(arrpath / "energy_flux.txt", flux)
    return max_power


def get_fireline_intensity(
    sim: SimulationOutputs, arrpath: Path, zarr_path: Path, plot: bool = True
):
    sim.to_zarr(zarr_path, outputs="surfEng", over_write=True)
    ds = zarr.open(zarr_path)
    # Calc time when cell experienced max power.
    ds = ds.fillna(0)  # Convert nan to 0 for dask
    xarr_max_power_time = ds.data.argmax("time")
    xarr_max_power_time = xr.where(
        xarr_max_power_time == 0, np.nan, xarr_max_power_time
    )  # Convert 0 to nan. xarr_max_power_time of zero means it never burned.

    # This creates an array of fireline intensities for every y transect at every timestep
    dx = 2
    y_time_firelineintensity = np.array(ds.data.sum(dim="x")) * dx
    np.nan_to_num(y_time_firelineintensity, copy=False, nan=0.0)

    # This uses the  xarr_max_power_time for each cell to map to the correct  y_time_firelineintensity value
    t_indexes = np.array(xarr_max_power_time).flatten()
    np.nan_to_num(t_indexes, copy=False, nan=0)
    t_indexes = t_indexes.astype(int)
    y_indexes = np.repeat(
        np.arange(xarr_max_power_time.shape[0]), xarr_max_power_time.shape[1]
    )
    y_x_firelineintensity = y_time_firelineintensity[t_indexes, y_indexes]
    y_x_firelineintensity = y_x_firelineintensity.reshape(xarr_max_power_time.shape)
    xarr_FLI = xr.DataArray(y_x_firelineintensity, dims=("y", "x"))
    xarr_FLI = xr.where(xarr_FLI == 0, np.nan, xarr_FLI)  # Replace 0 with np.nan


def get_canopy_residence_time(sim: SimulationOutputs, arrpath: Path, plot: bool = True):
    dens = sim.get_output("fuels-dens")
    add_to = np.zeros((sim.ny, sim.nx))
    for t in range(1, len(dens.times)):
        temp = dens.to_numpy(timestep=t)
        temp = np.sum(temp[0, 1:, :, :], axis=0)
        prev = dens.to_numpy(timestep=t - 1)
        prev = np.sum(prev[0, 1:, :, :], axis=0)
        fire_lox = np.where(temp != prev)
        no_fire_lox = np.where(temp == prev)
        temp[fire_lox] = 1
        temp[no_fire_lox] = 0
        add_to = np.add(temp, add_to)
    residence_time = add_to * 30
    if plot:
        plot_array(residence_time, "canopy residence time")
    np.savetxt(arrpath / "canopy_residence_time.txt", residence_time)
    return residence_time


def get_max_reaction_rate(sim: SimulationOutputs, arrpath: Path, plot: bool = True):
    react = sim.get_output("fire-reaction_rate")
    initial = react.to_numpy(timestep=0)
    initial = np.sum(initial, axis=1)
    prev = initial[0, :, :]
    for t in range(1, len(react.times)):
        temp = react.to_numpy(timestep=t)
        temp = np.sum(temp, axis=1)
        temp = temp[0, :, :]
        prev = np.maximum(temp, prev)
    max_react = prev.copy()
    if plot:
        plot_array(max_react, "max reaction rate")
    np.savetxt(arrpath / "max_reaction_rate.txt", max_react)
    return max_react


fires = ["Caldor", "CedarCreek", "Dixie", "KNP"]
for fire in fires:
    fire_dir = runs_dir / fire
    sites = [f"{fire[:3]}{i}" for i in range(1, 21)]
    for run in sites:
        if run == "Cal16":
            print(run)
            runpath = fire_dir / run
            sim_inputs = SimulationInputs.from_json(runpath / f"{run}.json")
            nz, ny, nx = (
                sim_inputs.quic_fire.nz,
                sim_inputs.qu_simparams.ny,
                sim_inputs.qu_simparams.nx,
            )
            sim_outputs = SimulationOutputs(runpath / "Output", nz, ny, nx)

            arrpath = runpath / "Arrays"
            arrpath.mkdir(exist_ok=True)

            print("\t- getting mass burnt")
            get_mass_burnt(sim_outputs, arrpath, False)
            print("\t- getting surface fuel moisture")
            get_surface_moisture(sim_outputs, arrpath, False)
            print("\t- getting surface consumption")
            get_surface_consumption(sim_outputs, arrpath, False)
            print("\t- getting canopy consumption")
            get_canopy_consumption(sim_outputs, arrpath, False)
            print("\t- getting power variables")
            get_power(sim_outputs, arrpath, False)
            print("\t- getting canopy residence time")
            get_canopy_residence_time(sim_outputs, arrpath, False)
            # print("\t- getting max reaction rate")
            # get_max_reaction_rate(sim_outputs, arrpath, True)
