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

arrpath = runpath / "Arrays"
arrpath.mkdir(exist_ok=True)

# integrated percent mass burnt
mburnt = sim.get_output("mburnt_integ")
mburnt_arr = mburnt.to_numpy()
mburnt_total = mburnt_arr[-1, 0, :, :]
plot_array(mburnt_total, "percent mass burnt")
np.savetxt(arrpath / "mass_burnt_pct.txt", mburnt_total)

# surface consumption
dens = sim.get_output("fuels-dens")
dens_init = dens.to_numpy(timestep=0)
dens_final = dens.to_numpy(len(dens.times) - 1)
surface_consumption = dens_init[0, 0, :, :] - dens_final[0, 0, :, :]
plot_array(surface_consumption, "surface fuel consumption")
np.savetxt(arrpath / "surface_consumption.txt", surface_consumption)

# canopy consumption
canopy_consumption = np.sum(dens_init, axis=1) - np.sum(dens_final, axis=1)
canopy_consumption = canopy_consumption[0, :, :]
plot_array(canopy_consumption, "canopy fuel consumption")
np.savetxt(arrpath / "canopy_consumption.txt", canopy_consumption)
