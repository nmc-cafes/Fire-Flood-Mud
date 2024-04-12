from matplotlib import pyplot as plt
from pathlib import Path
import numpy as np
from scipy.io import FortranFile


def plot_array(x, title):
    plt.figure(2)
    plt.set_cmap("viridis")
    plt.imshow(x, origin="lower")
    plt.colorbar()
    plt.title(title, fontsize=18)
    plt.show()


def read_dat_file(
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


path = Path(__file__).parent / "Caldor" / "Sample_Sites" / "Camp2" / "500m"
height = read_dat_file(path, "surface_depth.dat", (2, 302, 302), order="F")
# plot_array(height[0, :, :], "grass height")
# plot_array(height[1, :, :], "litter height")

density = read_dat_file(path, "surface_rhof.dat", (2, 302, 302), order="F")
# plot_array(density[0, :, :], "grass density")
# plot_array(density[1, :, :], "litter density")

# plt.scatter(density[1, :, :].ravel(), height[1, :, :].ravel())
# plt.show()

qf_path = Path(__file__).parent / "QF_runs" / "Caldor" / "Caldor_Camp2_500m"
calibrated_rhof = read_dat_file(qf_path, "treesrhof.dat", (84, 302, 302))
plot_array(calibrated_rhof[0, :, :], "calibrated duet density")
calibrated_height = read_dat_file(qf_path, "treesfueldepth.dat", (84, 302, 302))
plot_array(calibrated_height[0, :, :], "calibrated duet fuel height")
calibrated_moisture = read_dat_file(qf_path, "treesmoist.dat", (84, 302, 302))
plot_array(calibrated_moisture[0, :, :], "calibrated duet moisture")

print(np.max(calibrated_moisture[0, :, :]))
