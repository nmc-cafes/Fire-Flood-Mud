import numpy as np
from quicfire_tools import SimulationOutputs
from quicfire_tools.inputs import QUIC_fire
import matplotlib.pyplot as plt
from pathlib import Path


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


def get_surface_consumption(sim: SimulationOutputs, arrpath: Path, plot: bool = True):
    dens = sim.get_output("fuels-dens")
    dens_init = dens.to_numpy(timestep=0)
    dens_final = dens.to_numpy(len(dens.times) - 1)
    surface_consumption = dens_init[0, 0, :, :] - dens_final[0, 0, :, :]
    if plot:
        plot_array(surface_consumption, "surface fuel consumption")
    np.savetxt(arrpath / "surface_consumption.txt", surface_consumption)


def get_canopy_consumption(sim: SimulationOutputs, arrpath: Path, plot: bool = True):
    dens = sim.get_output("fuels-dens")
    dens_init = dens.to_numpy(timestep=0)
    dens_final = dens.to_numpy(len(dens.times) - 1)
    canopy_consumption = np.sum(dens_init, axis=1) - np.sum(dens_final, axis=1)
    canopy_consumption = canopy_consumption[0, :, :]
    if plot:
        plot_array(canopy_consumption, "canopy fuel consumption")
    np.savetxt(arrpath / "canopy_consumption.txt", canopy_consumption)


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


fires = ["Caldor", "CedarCreek", "CubCreek2", "Dixie", "KNP"]
for fire in fires:
    fire_dir = runs_dir / fire
    sites = [path.name for path in fire_dir.iterdir() if path.is_dir()]
    for site in sites:
        print(site)
        runpath = fire_dir / site
        quic_fire = QUIC_fire.from_file(runpath, version="latest")
        nz = quic_fire.nz
        sim_outputs = SimulationOutputs(runpath / "Output", nz=nz, ny=302, nx=302)
        # print(sim_outputs.list_available_outputs())

        # dens = sim_outputs.get_output("fuels-dens")
        # dens_arr = dens.to_numpy(timestep=len(dens.times) - 1)
        # plot_array(dens_arr[0, 0, :, :], "current fuel density")

        arrpath = runpath / "Arrays"
        arrpath.mkdir(exist_ok=True)

        print("\t- getting mass burnt")
        get_mass_burnt(sim_outputs, arrpath, False)
        print("\t- getting surface consumption")
        get_surface_consumption(sim_outputs, arrpath, False)
        print("\t- getting canopy consumption")
        get_canopy_consumption(sim_outputs, arrpath, False)
        print("\t- getting max power")
        get_max_power(sim_outputs, arrpath, False)
        print("\t- getting residence time from power")
        get_residence_time_from_power(sim_outputs, arrpath, False)
        print("\t- getting residence time from consumption")
        get_residence_time_from_consumption(sim_outputs, arrpath, False)
        print("\t- getting max reaction rate")
        get_max_reaction_rate(sim_outputs, arrpath, False)
