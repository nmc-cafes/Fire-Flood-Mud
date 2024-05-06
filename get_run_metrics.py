import numpy as np
from quicfire_tools import SimulationOutputs
from quicfire_tools.inputs import QUIC_fire
import matplotlib.pyplot as plt
from pathlib import Path
from scipy.io import FortranFile
import pandas as pd


def plot_array(x, title):
    plt.figure(2)
    plt.set_cmap("viridis")
    plt.imshow(x, origin="lower")
    plt.colorbar()
    plt.title(title, fontsize=18)
    plt.show()


runs_dir = Path("/Users/ntutland/Documents/Projects/Fire-Flood-Mud/QF_runs/")


def get_mass_burnt(sim: SimulationOutputs, arrpath: Path, plot: bool = True):
    mburnt = sim.get_output("mburnt_integ")
    mburnt_arr = mburnt.to_numpy()
    mburnt_total = mburnt_arr[-1, 0, :, :]
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
        # print(np.min(dens_init[0, 0, :, :][dens_init[0, 0, :, :] > 0]))
        # plot_array(dens_init[0, 0, :, :], "initial surface fuel density")
        # plot_array(dens_final[0, 0, :, :], "final surface fuel density")
        plot_array(surface_consumption_pct, "surface fuel consumption percent")
        # plot_array(surface_remaining, "surface fuel percent remaining")
    np.savetxt(arrpath / "surface_consumption_pct.txt", surface_consumption_pct)
    np.savetxt(arrpath / "surface_remaining_pct.txt", surface_remaining)
    np.savetxt(arrpath / "surface_consumption", surface_consumption)
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
    canopy_consumption = np.zeros(np.shape(canopy_init))
    fuels_present = np.zeros(np.shape(canopy_init))
    fuels_present[fuel_present] = 1
    canopy_consumption[fuel_present] = (
        canopy_init[fuel_present] - canopy_final[fuel_present]
    ) / canopy_init[fuel_present]
    canopy_remaining = np.zeros(np.shape(canopy_init))
    canopy_remaining[fuel_present] = 1 - (
        (canopy_init[fuel_present] - canopy_final[fuel_present])
        / canopy_init[fuel_present]
    )
    if plot:
        # print(np.sum(canopy_init))
        # print(np.sum(canopy_init) - np.sum(canopy_final))
        # print((np.sum(canopy_init) - np.sum(canopy_final)) / np.sum(canopy_init))
        plot_array(canopy_consumption, "total canopy fuel consumption")
        plot_array(canopy_remaining, "total canopy remaining")
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
    np.savetxt(arrpath / "canopy_consumption.txt", canopy_consumption)
    np.savetxt(arrpath / "canopy_remaining.txt", canopy_remaining)
    return canopy_consumption


def get_max_power(sim: SimulationOutputs, arrpath: Path, plot: bool = True):
    energy = sim.get_output("surfEnergy")
    initial = energy.to_numpy(timestep=0)
    prev = initial[0, 0, :, :]
    for t in range(1, len(energy.times)):
        temp = energy.to_numpy(timestep=t)
        temp = temp[0, 0, :, :]
        prev = np.maximum(temp, prev)
    max_power = prev.copy()
    if plot:
        plot_array(max_power, "max power")
    np.savetxt(arrpath / "max_power.txt", max_power)
    return max_power


def get_residence_time_from_power(
    sim: SimulationOutputs, arrpath: Path, plot: bool = True
):
    energy = sim.get_output("surfEnergy")
    initial = energy.to_numpy(timestep=0)
    prev = initial[0, 0, :, :]
    for t in range(1, len(energy.times)):
        temp = energy.to_numpy(timestep=t)
        temp = temp[0, 0, :, :]
        temp[np.where(temp > 0)] = 1
        prev = np.add(temp, prev)
    residence_time = prev.copy()
    if plot:
        plot_array(residence_time, "residence time from power")
    np.savetxt(arrpath / "residence_time_power.txt", residence_time)
    return residence_time


def get_residence_time_from_consumption(
    sim: SimulationOutputs, arrpath: Path, plot: bool = True
):
    dens = sim.get_output("fuels-dens")
    add_to = np.zeros((sim.ny, sim.nx))
    for t in range(1, len(dens.times)):
        temp = dens.to_numpy(timestep=t)
        temp = np.sum(temp, axis=1)
        temp = temp[0, :, :]
        prev = dens.to_numpy(timestep=t - 1)
        prev = np.sum(prev, axis=1)
        prev = prev[0, :, :]
        fire_lox = np.where(temp != prev)
        no_fire_lox = np.where(temp == prev)
        temp[fire_lox] = 1
        temp[no_fire_lox] = 0
        add_to = np.add(temp, add_to)
    residence_time = add_to * 30
    if plot:
        plot_array(residence_time, "residence time from consumption")
    np.savetxt(arrpath / "residence_time_consumption.txt", residence_time)
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


fire_site_dict = {
    "Caldor": ["American", "Camp1", "Camp2", "Camp3", "Strawberry"],
    "CedarCreek": ["Cedar1", "Cedar2", "Wolf1", "Wolf2", "Wolf3"],
    "CubCreek2": ["Chewuch", "Eightmile", "Falls", "LambButte", "Sherwood"],
    "Dixie": ["Genesee", "Kings", "Rush", "Ward", "Yellow"],
    "KNP": ["AshMountain", "KaweahMiddle", "KaweahNorth", "Potwisha", "Yucca"],
}
for fire in fire_site_dict.keys():
    fire_dir = runs_dir / fire
    sites = fire_site_dict.get(fire)
    runs = [f"{fire}_{site}_duet" for site in sites]
    for run in runs:
        print(run)
        runpath = fire_dir / run
        quic_fire = QUIC_fire.from_file(runpath, version="latest")
        nz = quic_fire.nz
        sim_outputs = SimulationOutputs(runpath / "Output", nz=nz, ny=302, nx=302)
        # print(sim_outputs.list_available_outputs())

        dens = sim_outputs.get_output("fuels-dens")
        dens_arr = dens.to_numpy(timestep=len(dens.times) - 1)
        plot_array(dens_arr[0, 0, :, :], "current fuel density")

        arrpath = runpath / "Arrays"
        arrpath.mkdir(exist_ok=True)

        print("\t- getting mass burnt")
        get_mass_burnt(sim_outputs, arrpath, False)
        print("\t- getting surface fuel moisture")
        get_surface_moisture(sim_outputs, arrpath, False)
        print("\t- getting surface consumption")
        get_surface_consumption(sim_outputs, arrpath, True)
        print("\t- getting canopy consumption")
        get_canopy_consumption(sim_outputs, arrpath, True)
        print("\t- getting max power")
        get_max_power(sim_outputs, arrpath, False)
        print("\t- getting residence time from power")
        get_residence_time_from_power(sim_outputs, arrpath, False)
        print("\t- getting residence time from consumption")
        get_residence_time_from_consumption(sim_outputs, arrpath, False)
        print("\t- getting max reaction rate")
        get_max_reaction_rate(sim_outputs, arrpath, False)
