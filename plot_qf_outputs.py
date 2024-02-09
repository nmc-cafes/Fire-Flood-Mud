import numpy as np
from quicfire_tools import SimulationOutputs
import matplotlib.pyplot as plt
from pathlib import Path


def plot_array(x, title):
    plt.figure(2)
    plt.set_cmap("viridis")
    plt.imshow(x, origin="lower")
    plt.colorbar()
    plt.title(title, fontsize=18)
    plt.show()


runpath = Path(
    "/Users/ntutland/Documents/Projects/Fire-Flood-Mud/QF_runs/KNP/KNP_KaweahMiddle_500m"
)

sim = SimulationOutputs(runpath / "Output", nz=65, ny=302, nx=302)
print(sim.list_available_outputs())
dens = sim.get_output("fuels-dens")
dens_arr = dens.to_numpy()
plot_array(dens_arr[-1, 0, :, :], "final density")
