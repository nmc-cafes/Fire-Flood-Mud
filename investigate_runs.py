import numpy as np
from quicfire_tools import SimulationOutputs, SimulationInputs
import matplotlib.pyplot as plt
from pathlib import Path
from scipy.io import FortranFile
import pandas as pd
import xarray as xr
import zarr
import pandas as pd


def plot_array(x, title):
    plt.figure(2)
    plt.set_cmap("viridis")
    plt.imshow(x, origin="lower")
    plt.colorbar()
    plt.title(title, fontsize=18)
    plt.show()


def plot_1D_scatter(arr):
    """
    Plots a scatter plot where the x-axis represents indices of the array
    and the y-axis represents the values in the array.

    Parameters:
        arr (numpy.ndarray): A 1D numpy array.
    """
    if not isinstance(arr, np.ndarray) or arr.ndim != 1:
        raise ValueError("Input must be a 1D NumPy array.")

    x = np.arange(len(arr))  # Indices of the array

    plt.figure(figsize=(8, 5))
    plt.scatter(x, arr, color="b", alpha=0.7, edgecolors="k")
    plt.xlabel("Index")
    plt.ylabel("Value")
    plt.title("Scatter Plot of Array Values")
    plt.grid(True, linestyle="--", alpha=0.6)
    plt.show()


def plot_1D_scatter2(arr1, arr2, label1, label2, title):
    """
    Plots a scatter plot where the x-axis represents indices of the arrays
    and the y-axis represents the values in the arrays.

    Parameters:
        arr1 (numpy.ndarray): A 1D numpy array.
        arr2 (numpy.ndarray): A 1D numpy array.
    """
    if not isinstance(arr1, np.ndarray) or arr1.ndim != 1:
        raise ValueError("First input must be a 1D NumPy array.")
    if not isinstance(arr2, np.ndarray) or arr2.ndim != 1:
        raise ValueError("Second input must be a 1D NumPy array.")

    x1 = np.arange(len(arr1))  # Indices of the first array
    x2 = np.arange(len(arr2))  # Indices of the second array

    plt.figure(figsize=(8, 5))
    plt.scatter(x1, arr1, color="b", alpha=0.7, edgecolors="k", label=label1)
    plt.scatter(x2, arr2, color="r", alpha=0.7, edgecolors="k", label=label2)
    plt.xlabel("Height (m)")
    plt.ylabel("Fuel Loading (kg)")
    plt.title(title)
    plt.legend()
    plt.grid(True, linestyle="--", alpha=0.6)
    plt.show()


def plot_boxplot(data_dict):
    """
    Plots a box-and-whisker plot where the x-axis represents indices of the arrays
    and the y-axis represents the values in the arrays. Each index has a boxplot
    summarizing values across all arrays at that index.

    Parameters:
        data_dict (dict): A dictionary where keys are labels and values are 1D numpy arrays.
    """
    if not isinstance(data_dict, dict) or not all(
        isinstance(v, np.ndarray) and v.ndim == 1 for v in data_dict.values()
    ):
        raise ValueError("Input must be a dictionary of 1D NumPy arrays.")

    max_length = max(len(arr) for arr in data_dict.values())
    data_by_index = [[] for _ in range(max_length)]

    for arr in data_dict.values():
        for i, value in enumerate(arr):
            data_by_index[i].append(value)

    plt.figure(figsize=(10, 6))
    plt.boxplot(
        data_by_index,
        positions=np.arange(len(data_by_index)),
        patch_artist=True,
        showfliers=False,
    )
    plt.xlabel("Index")
    plt.ylabel("Value")
    plt.title("Box-and-Whisker Plot of Array Values at Each Index")
    plt.grid(True, linestyle="--", alpha=0.6)
    plt.xticks(np.linspace(0, max_length - 1, num=10))  # Treat x-axis as continuous
    plt.show()


def get_surface_consumption(sim: SimulationOutputs, plot: bool = True):
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
    return surface_consumption_pct


def get_canopy_consumption(sim: SimulationOutputs, plot: bool = True):
    dens = sim.get_output("fuels-dens")
    dens_init = dens.to_numpy(timestep=0)
    dens_final = dens.to_numpy(len(dens.times) - 1)
    dens_init = dens_init[0, 1:, :, :]
    dens_final = dens_final[0, 1:, :, :]
    canopy_init = np.sum(dens_init, axis=0)
    canopy_final = np.sum(dens_final, axis=0)
    fuel_present = np.where(canopy_init > 0)
    # canopy_consumption_pct = np.zeros(np.shape(canopy_init))
    canopy_consumption_pct = np.full(np.shape(canopy_init), -0.5)
    # fuels_present = np.zeros(np.shape(canopy_init))
    # fuels_present[fuel_present] = 1
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

    return canopy_consumption_pct


runs_dir = Path("D:/Fire-Flood-Mud/QF_runs/SBS")
fires = ["Caldor", "CedarCreek", "CubCreek2", "Dixie", "KNP"]
max_height_of_consump = []
consump_by_z_dict = {}
consump_df_dict = {"Fire": [], "Site": [], "Height": [], "Initial": [], "Final": []}
for fire in fires:
    for site in range(1, 21):
        site_name = f"{fire[:3]}{site}"

        runpath = runs_dir / fire / site_name
        sim_inputs = SimulationInputs.from_json(runpath / f"{site_name}.json")
        nz, ny, nx = (
            sim_inputs.quic_fire.nz,
            sim_inputs.qu_simparams.ny,
            sim_inputs.qu_simparams.nx,
        )
        sim_outputs = SimulationOutputs(runpath / "Output", nz, ny, nx)

        dens = sim_outputs.get_output("fuels-dens")
        dens_init = dens.to_numpy(timestep=0)
        dens_final = dens.to_numpy(timestep=len(dens.times) - 1)

        init_fuel_by_z = np.sum(dens_init[0, 1:, :, :], axis=(1, 2))
        final_fuel_by_z = np.sum(dens_final[0, 1:, :, :], axis=(1, 2))

        # plot_1D_scatter2(
        #     init_fuel_by_z,
        #     final_fuel_by_z,
        #     "Initial Canopy Loading",
        #     "Final Canopy Loading",
        #     f"{site_name}\nConsumption by height",
        # )

        consump_by_z = np.subtract(init_fuel_by_z, final_fuel_by_z)
        # plot_1D_scatter(consump_by_z)
        # for height in range(len(consump_by_z)):
        #     if consump_by_z[height] < 100:
        #         max_height_of_consump.append(height)
        #         break

        # consump_by_z_dict[site_name] = consump_by_z
        # for height in range(len(consump_by_z)):
        #     consump_df_dict["Fire"].append(fire)
        #     consump_df_dict["Site"].append(site_name)
        #     consump_df_dict["Height"].append(height + 1)
        #     consump_df_dict["Initial"].append(init_fuel_by_z[height])
        #     consump_df_dict["Final"].append(final_fuel_by_z[height])
        # moist = sim_outputs.get_output("fuels-moist")
        # moist_arr = moist.to_numpy(timestep=0)
        # plot_array(moist_arr[0, 0, :, :], "surface moisture")
        # plot_array(moist_arr[0, 25, :, :], "canopy moisture")

# consump_df = pd.DataFrame(consump_df_dict)
# consump_df.to_csv(Path(__file__).parent / "consump_by_z.csv")


# plot_boxplot(consump_by_z_dict)

# plot_1D_scatter(np.array(max_height_of_consump))

# for fire in fires:
#     fire_dir = runs_dir / fire
#     sites = [f"{fire[:3]}{i}" for i in range(1, 21)]
#     for run in sites:
#         print(run)
#         runpath = fire_dir / run
#         sim_inputs = SimulationInputs.from_json(runpath / f"{run}.json")
#         nz, ny, nx = (
#             sim_inputs.quic_fire.nz,
#             sim_inputs.qu_simparams.ny,
#             sim_inputs.qu_simparams.nx,
#         )
#         sim_outputs = SimulationOutputs(runpath / "Output", nz, ny, nx)
#         canopy_consuption_pct = get_canopy_consumption(sim_outputs, False)
#         plot_array(canopy_consuption_pct, run)
