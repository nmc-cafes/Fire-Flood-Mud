from pathlib import Path
import numpy as np
from quicfire_tools import SimulationOutputs
from matplotlib import pyplot as plt


def plot_array_fancy(x: np.ndarray, metric: str, timestep: int, layer: int):
    arr = x[timestep, layer, :, :]
    plt.figure(2)
    plt.set_cmap("viridis")
    plt.imshow(arr, origin="lower")
    plt.colorbar()
    plt.title(f"{metric} layer {layer}, timestep {timestep}", fontsize=18)
    plt.show()


def plot_array(x: np.ndarray, title: str):
    plt.figure(2)
    plt.set_cmap("viridis")
    plt.imshow(x, origin="lower")
    plt.colorbar()
    plt.title(title, fontsize=18)
    plt.show()


HERE = Path(__file__).parent
runpath = HERE / "QF_runs" / "Caldor" / "Cal5"
sim = SimulationOutputs(runpath / "Output", nz=84, nx=333, ny=295)

# runpath = HERE / "QF_runs" / "Moisture_Sensitivity" / "Dix5"
# sim = SimulationOutputs(runpath / "Output", nz=81, nx=411, ny=282)
dens = sim.get_output("fuels-dens")
# dens_init = dens.to_numpy(timestep=0)
dens_final = dens.to_numpy(timestep=len(dens.times) - 1)
dens_init = dens.to_numpy(timestep=0)

plot_array(dens_final[0, 0, :, :], "current surface fuel density")

canopy_init = np.sum(dens_init[0, 1:, :, :], axis=0)
canopy_final = np.sum(dens_final[0, 1:, :, :], axis=0)
canopy_remain = np.full(canopy_init.shape, -0.25)
canopy_remain[np.where(canopy_init > 0)] = (
    canopy_final[np.where(canopy_init > 0)] / canopy_init[np.where(canopy_init > 0)]
)
plot_array(canopy_remain, "current canopy fuel density")

# dens_init_surf = np.sum(dens_init[0, 0, :, :])
# dens_init_trees = np.sum(dens_init[0, 1:, :, :])
# dens_init_ladder = np.sum(dens_init[0, 1:5, :, :])
# dens_init_canopy = np.sum(dens_init[0, 5:, :, :])

# # print(dens_init_surf)
# # print(dens_init_trees)
# # print(dens_init_ladder)
# # print(dens_init_canopy)

# avg_cell_dens_surf = np.mean(dens_init[0, 0, :, :][dens_init[0, 0, :, :] > 0.01])
# avg_cell_dens_trees = np.mean(dens_init[0, 1:, :, :][dens_init[0, 1:, :, :] > 0.01])
# avg_cell_dens_ladder = np.mean(dens_init[0, 1:5, :, :][dens_init[0, 1:5, :, :] > 0.01])
# avg_cell_dens_canopy = np.mean(dens_init[0, 5:, :, :][dens_init[0, 5:, :, :] > 0.01])

# print(avg_cell_dens_surf)
# print(avg_cell_dens_trees)
# print(avg_cell_dens_ladder)
# print(avg_cell_dens_canopy)

# sum_y = np.sum(dens_init[0, 1:, :, :], axis=1)
# sum_x = np.sum(dens_init[0, 1:, :, :], axis=2)
# plt.figure(2)
# plt.set_cmap("viridis")
# plt.imshow(sum_y, origin="lower")
# plt.colorbar()
# plt.title(f"sum y axis", fontsize=18)
# plt.show()
# plt.figure(2)
# plt.set_cmap("viridis")
# plt.imshow(sum_x, origin="lower")
# plt.colorbar()
# plt.title(f"sum x axis", fontsize=18)
# plt.show()
